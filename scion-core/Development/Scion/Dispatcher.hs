module Development.Scion.Dispatcher where

import Development.Scion.Core

------------------------------------------------------------------------------

-- | Handle to the state of the dispatcher
data Handle = Handle

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

new :: IO Handle
new = return Handle

------------------------------------------------------------------------------

-- TODO: Maybe args should be location annotated, so we can report where a
-- particular flag came from.  E.g., from the .cabal file.

compileFile :: Handle   -- ^ Dispatcher handle
            -> FilePath -- ^ Source file to compile
            -> [String] -- ^ Compiler flags
            -> IO (Either DispatcherError CompilationResult)
compileFile hdl sourceFile flags = do
  return $ Left $ DispatcherError "unimplemented"
