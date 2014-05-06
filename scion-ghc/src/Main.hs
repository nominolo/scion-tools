import Development.Scion.Utils.IO
import Development.Scion.WorkerMessage
import GHC.Paths (libdir)
import System.IO

main :: IO ()
main = do

  replyHandle <- makeExclusive stdout stderr
  ensureBinaryMode stdin
  ensureBinaryMode replyHandle

  putStrLn "Worker ready"
  hFlush stdout

  messageOrErr <- recvMessageFromHandle stdin
  case messageOrErr of
    Left err -> do
      putStrLn err
      hFlush stdout
      return ()
    Right cmd -> do
      --putStrLn $ "Got command: " ++ show cmd
      case cmd of
        GetWorkerVersion -> do
          sendMessageToHandle replyHandle (WorkerVersion [0])
          return ()
        _ ->
          return ()
