{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Dispatcher where

import Development.Scion.Core

import Prelude hiding ( lines )

import Control.Applicative
--import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception ( handle, SomeException )
import Control.Monad
import Data.Attoparsec.Char8 as Atto
import System.IO ( Handle )
import System.Exit
--import System.FilePath
import System.Process
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

------------------------------------------------------------------------------

data DispatcherConfig = DispatcherConfig
  { dcGhcExecutable :: !FilePath }

-- | Handle to the state of the dispatcher
data DispatcherHandle = DispatcherHandle
  { dhConfig :: !DispatcherConfig }

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

newDispatcherHandle :: DispatcherConfig -> IO DispatcherHandle
newDispatcherHandle = return . DispatcherHandle

------------------------------------------------------------------------------

-- TODO: Maybe args should be location annotated, so we can report where a
-- particular flag came from.  E.g., from the .cabal file.

compileFile :: DispatcherHandle   -- ^ Dispatcher handle
            -> FilePath -- ^ Source file to compile
            -> [String] -- ^ Compiler flags
            -> IO (Either DispatcherError CompilationResult)
compileFile hdl sourceFile flags = do
  let cfg = dhConfig hdl
  allOutput <- newCapturedOutput

  (_inp, out, err, pHdl)
      <- runInteractiveProcess (dcGhcExecutable cfg)
                               (flags ++ ["-ferror-spans", sourceFile])
                               Nothing -- no custom working dir
                               Nothing -- inherit environment

  qMsgs <- newTQueueIO
  outRdr <- async (captureOutput (coStdout allOutput) out)
  errRdr <- async (captureOutput (coStderr allOutput) err)
  findMsgs <- async (parseErrors sourceFile allOutput qMsgs)

  exitCode <- waitForProcess pHdl

  -- TODO: Parse error messages from stderr; Line looks like:
  -- <sourceFile>:<location>:( Warning:)?\n, then message follows indented by 4
  -- spaces

  -- TODO: Add some kind of timeout
  wait outRdr
  wait errRdr
  wait findMsgs

  messages <- collectMessages qMsgs []

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
      

data CapturedOutput = CapturedOutput
  { coStdout :: !(TQueue (Maybe BC8.ByteString))
  , coStderr :: !(TQueue (Maybe BC8.ByteString))
  }

newCapturedOutput :: IO CapturedOutput
newCapturedOutput =
  CapturedOutput <$> newTQueueIO <*> newTQueueIO

captureOutput :: TQueue (Maybe BC8.ByteString) -> Handle -> IO ()
captureOutput queue h = loop
 where
   loop = do
     mbLine <- handle (\ (_e :: SomeException) -> return Nothing) $ do
                    Just <$> BC8.hGetLine h
     atomically $ writeTQueue queue mbLine
     case mbLine of
       Nothing    -> return ()
       Just _line -> loop

parseErrors :: FilePath -> CapturedOutput -> TQueue (Maybe Message) -> IO ()
parseErrors fname capture qMsgs = findMessageHead Nothing
 where
   tfname = BC8.pack fname
   qLines = coStderr capture

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
pQuotedIdent = (do
  ident <- char '`' *> takeTill isSpace
  guard (BC8.length ident > 1 && BC8.last ident == '\'')
  return $ T.decodeUtf8 (BC8.init ident))
    <|>
  (T.decodeUtf8 <$> takeTill isSpace)
