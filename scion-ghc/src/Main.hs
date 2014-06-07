{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Development.Scion.Message
import Development.Scion.Types
--import Development.Scion.WorkerMessage

import Control.Applicative
import GHC.Paths (libdir)
import System.IO
import Control.Exception
import Control.Concurrent ( threadDelay )
import System.Exit
import qualified Data.Text as T

import GHC hiding ( mkModuleName, ModuleName, SourceError, Warning )
import qualified GHC as Ghc
import Outputable ( renderWithStyle, showPpr, Outputable, sdocWithDynFlags,
                    mkErrStyle, withPprStyle, showSDoc )
import MonadUtils ( liftIO )
import Exception ( ghandle )
import HeaderInfo ( getOptionsFromFile, getOptions, getImports )
import StringBuffer ( hGetStringBuffer )

message :: String -> IO ()
message m = putStrLn m >> hFlush stdout

main :: IO ()
main = do

  replyHandle <- makeExclusive stdout stderr
  ensureBinaryMode stdin
  ensureBinaryMode replyHandle

  message "Worker ready"

  waitForInitCommand replyHandle

workerVersion :: [Int]
workerVersion = [0]

waitForInitCommand :: Handle -> IO ()
waitForInitCommand replyHandle = do
  messageOrErr <- recvMessage stdin
  case messageOrErr of
    Left err -> do
      message err
      return ()
    Right cmd -> do
      --message $ "Got command: " ++ show cmd
      case cmd of
        GetWorkerVersion -> do
          sendMessage replyHandle (WorkerVersion workerVersion)
          waitForInitCommand replyHandle

        InitGhcWorker flags -> do
          ghcWorkerMain replyHandle flags

        _ -> do
          sendMessage replyHandle $
            WorkerFailure "Worker not initialised"

          return ()


ghcWorkerMain :: Handle -> [T.Text] -> IO ()
ghcWorkerMain replyHandle flags = do

  let ghcLogger dflags _severity _span pprStyle msg = do
        message $ renderWithStyle dflags msg pprStyle

  let args0 = map T.unpack flags

  handleWorkerError replyHandle $ do

    let args1 = map (mkGeneralLocated "worker-flags") args0

    (args2, staticFlagWarnings) <- parseStaticFlags args1

    runGhc (Just libdir) $ do
      dflags0 <- GHC.getSessionDynFlags

      let dflags1 = dflags0{ ghcMode = OneShot
                           , ghcLink = LinkBinary  -- Not used anyway
                           , hscOutName = error "hscOutName not set"
                           , verbosity = 1
                           , log_action = ghcLogger
                           }
      (dflags2, fileishArgs, dynFlagWarnings)
        <- GHC.parseDynamicFlags dflags1 args2

      let extraArgWarnings
            | null fileishArgs = []
            | otherwise =
              [T.pack $ "Ignoring flags: " ++ show (map unLoc fileishArgs)]

      let allWarnings =
            map (T.pack . unLoc) (staticFlagWarnings ++ dynFlagWarnings) ++
                extraArgWarnings

      GHC.defaultCleanupHandler dflags2 $ do

        _ <- GHC.setSessionDynFlags dflags2

        -- Not sure if these are necessary. They may have side effects. (Yes,
        -- the GHC API is that evil.)
        _dflags3 <- GHC.getSessionDynFlags
        _hsc_env <- GHC.getSession

        liftIO $ sendMessage replyHandle $
          GhcWorkerReady []

        workerMainLoop replyHandle


handleWorkerError :: Handle -> IO () -> IO ()
handleWorkerError replyHandle body = do
  ghandle (\exception -> do
    hFlush stderr
    hFlush stdout
    case fromException exception of
      Just (ioerr :: IOException) ->
        fatalMessage ("IOExc:" ++ (show ioerr))
      _ ->
        case fromException exception of
          Just UserInterrupt ->
            exitWith (ExitFailure 1)
          Just StackOverflow ->
            fatalMessage "stack overflow: use +RTS -K<size> to increase it"
          _ ->
            case fromException exception of
              Just (ex :: ExitCode) ->
                fatalMessage "Worker called exit()"
              _ ->
                fatalMessage $ "GHC Panic: " ++ show exception
     ) (-- error messages propagated as exceptions
        ghandle (\(ge :: GhcException)-> do
           hFlush stdout
  	   case ge of
             PhaseFailed _ code -> exitWith code
             Signal _ -> exitWith (ExitFailure 1)
             _ -> fatalMessage (show ge)
	  ) body)
 where
   fatalMessage :: String -> IO ()
   fatalMessage str = do
     message $ "FATAL: " ++ str
     sendMessage replyHandle (WorkerFailure (T.pack str))
     -- give parent a chance to receive our goodbye message
     threadDelay 1000000
     hClose stderr
     hClose replyHandle
     exitWith (ExitFailure 1)

------------------------------------------------------------------------------

workerMainLoop :: Handle -> Ghc ()
workerMainLoop replyHandle = do
  messageOrErr <- liftIO $ recvMessage stdin
  case messageOrErr of
    Left err -> do
      liftIO $ message err
      return ()
    Right cmd -> do
      case cmd of
        GetWorkerVersion -> do
          reply $ WorkerVersion workerVersion
          workerMainLoop replyHandle

        ParseImports path -> do
          ans <- ParsedImports <$> parseImports path
          reply ans
          workerMainLoop replyHandle

 where
   reply ans = liftIO $ sendMessage replyHandle ans

------------------------------------------------------------------------------

-- | Parse the imports and flags set via pragmas.
parseImports :: FilePath -> Ghc ModuleHeader
parseImports file = do
  dflags <- getSessionDynFlags
  liftIO $ do
    -- HeaderInfo.getOptions is pure but may throw an exception when
    -- the result is forced (WTF?!).  This reads the file twice, but
    -- I'll take correctness over performance anytime.
    options <- map (T.pack . unLoc) <$> getOptionsFromFile dflags file
    buf <- hGetStringBuffer file
    (source_imports, normal_imports, module_name0)
      <- getImports dflags buf file file
    let imports = map (mkImport True) source_imports ++
                  map (mkImport False) normal_imports
    return $! ModuleHeader
      { moduleHeaderModuleName = ghcToModuleName (unLoc module_name0)
      , moduleHeaderOptions = options
      , moduleHeaderImports = imports
      }
 where
   mkImport isSource imp0 =
     let imp = unLoc imp0 in
     ImportDependency
       { importModuleName = ghcToModuleName (unLoc (ideclName imp))
       , importPackageName = Nothing
       , importSource = isSource
       , importSafe = ideclSafe imp
       }

------------------------------------------------------------------------------

ghcToModuleName :: Ghc.ModuleName -> ModuleName
ghcToModuleName mn = mkModuleName (T.pack (moduleNameString mn))

