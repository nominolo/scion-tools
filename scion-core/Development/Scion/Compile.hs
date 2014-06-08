{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Compile where

import Development.Scion.Core
import Development.Scion.Types
import Development.Scion.Dispatcher
import Development.Scion.Utils.IO

import Prelude hiding ( lines )

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Attoparsec.Char8 as Atto
import System.Exit
import System.Process
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

------------------------------------------------------------------------------

data CompilerOptions = CompilerOptions
  { coOutputDir :: !(Maybe FilePath)
  }

-- TODO: Maybe args should be location annotated, so we can report where a
-- particular flag came from.  E.g., from the .cabal file.

compileFile :: DispatcherHandle   -- ^ Dispatcher handle
            -> FilePath -- ^ Source file to compile
            -> CompilerOptions
            -> [String] -- ^ Compiler flags
            -> IO (Either DispatcherError CompilationResult)
compileFile hdl sourceFile opts flags = do
  let cfg = dhConfig hdl

  (_inp, out, err, pHdl)
      <- runInteractiveProcess (dcGhcExecutable cfg)
                               (flags ++
                                (case coOutputDir opts of
                                   Nothing -> []
                                   Just fp -> ["-odir", fp]) ++
                                [ "-ferror-spans", sourceFile])
                               Nothing -- no custom working dir
                               Nothing -- inherit environment

  (aOut, aErr) <- captureProcessOutput out err (printOutput (dhLogger hdl)) $ \qErr -> do
                    qMsgs <- newTQueueIO
                    findMsgs <- async (parseErrors sourceFile qErr qMsgs)
                    collectMessages qMsgs [] <* wait findMsgs

  exitCode <- waitForProcess pHdl

  -- TODO: Add some kind of timeout
  _ <- wait aOut
  messages <- wait aErr

  -- TODO: Put error messages into .meta file

  let rslt = CompilationResult{ crSuccess = exitCode == ExitSuccess
                              , crFile = sourceFile
                              , crMessages = messages
                              }
  return $ Right rslt

 where
   collectMessages qMsgs msgs = do
     mbMsg <- atomically $ readTQueue qMsgs
     case mbMsg of
       Nothing -> return (reverse msgs)
       Just msg -> collectMessages qMsgs (msg:msgs)
      

parseErrors :: FilePath -> LinesQueue -> TQueue (Maybe Message) -> IO ()
parseErrors fname qLines qMsgs = findMessageHead Nothing
 where
   tfname = BC8.pack fname

   -- TODO: Use iostreams / pipes / conduit?
   findMessageHead mbLeftover = do
     mbLine <- maybe (atomically $ readTQueue qLines) (return . Just) mbLeftover
     case mbLine of
       Nothing -> atomically $ writeTQueue qMsgs Nothing   -- we're done
       Just line
         | Right r <- parseOnly (pMessageHead tfname) line
         -> parseMessageBody r []
         | otherwise
         -> findMessageHead Nothing
   
   parseMessageBody headInfo lines = do
     mbLine <- atomically $ readTQueue qLines
     case mbLine of
       Just line
         | "    " `BC8.isPrefixOf` line
         -> parseMessageBody headInfo (BC8.drop 4 line : lines)
       _otherwise -> do
         let msg = parseMessage headInfo (reverse lines)
         --print msg
         atomically $ writeTQueue qMsgs (Just msg)
         case mbLine of
           Nothing -> atomically $ writeTQueue qMsgs Nothing  -- we're done
           _ -> findMessageHead mbLine

   parseMessage :: (SourceSpan, Severity) -> [BC8.ByteString] -> Message
   parseMessage (srcSpan, sev) lines0 =
     let lines = BC8.unlines lines0 in
     case parseOnly pMessageBody lines of
       Left _err -> Message sev srcSpan (OtherMessage (T.decodeUtf8 lines))
       Right msg -> Message sev srcSpan msg

-- | Parse a GHC source span:
--
-- > "1:3"         =>  (0,2)-(0,2)
-- > "1:3-5"       =>  (0,2)-(0,4)
-- > "(2,1)-(4,8)  =>  (1,0)-(3,7)
parseRange :: Parser SourceSpan
parseRange = multiLine <|> singleLine
 where
   singleLine = do
     startLine <- decimal
     startCol  <- char ':' *> decimal
     spanMinusOne startLine startCol startLine 
       <$> option startCol (char '-' *> decimal)

   multiLine = spanMinusOne <$> (char '('     *> decimal)
                            <*> (char ','     *> decimal)
                            <*> (string ")-(" *> decimal)
                            <*> (char ','     *> decimal <*  char ')')
       
   spanMinusOne l1 c1 l2 c2 =
     SourceSpan (l1 - 1) (c1 - 1) (l2 - 1) (c2 - 1)

-- | Parse the head of a GHC error or warning messages:
--
-- > "<filename>:<span>:"           =>  (<span>, Error)
-- > "<filename>:<span>: Warning:"  => (<span>, Warning)
--
pMessageHead :: BC8.ByteString -> Parser (SourceSpan, Severity)
pMessageHead fname = do
  _        <- Atto.string fname <* Atto.char ':'
  srcSpan  <- parseRange <* Atto.char ':'
  severity <- option Error (string " Warning:" *> pure Warning)
  endOfInput
  return (srcSpan, severity)  


pMessageBody :: Parser MessageInfo
pMessageBody =
  NotInScope <$> (string "Not in scope:" *> skipSpace *> pQuotedIdent)

-- | Parse a quoted identifier (as used by GHC):
--
-- > "`foo' "   =>  "foo"
-- > "`foo'' "  =>  "foo'"
pQuotedIdent :: Parser T.Text
pQuotedIdent = do
  option () (Atto.string "type constructor or class" *> skipSpace)
  ((do ident <- char '`' *> takeTill isSpace
       guard (BC8.length ident > 1 && BC8.last ident == '\'')
       return $ T.decodeUtf8 (BC8.init ident))
    <|>
   (T.decodeUtf8 <$> takeTill isSpace))
