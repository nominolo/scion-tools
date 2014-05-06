import Development.Scion.Utils.IO

import GHC.Paths (libdir)

main :: IO ()
main = do
  replyHandle <- makeExclusive stdout stderr
  ensureBinaryMode stdin
  ensureBinaryMode replyHandle

  putStrLn "Hello world"

  -- hFlush stdout

  return ()


--processMessage :: 
