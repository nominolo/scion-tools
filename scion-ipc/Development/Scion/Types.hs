{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification,
    StandaloneDeriving, DeriveGeneric, OverloadedStrings
 #-}
module Development.Scion.Types where

import Development.Scion.Binary
import Data.Binary

--import Control.Applicative
--import Data.Int ( Int64 )
import Data.Monoid
import GHC.Generics ( Generic )
--import System.IO
import qualified Data.Text as T
--import qualified Data.ByteString.Lazy.Char8 as BLC8

------------------------------------------------------------------------------

-- | A message from the dispatcher to the worker.
data WorkerCommand
  = GetWorkerVersion
  | InitGhcWorker [T.Text]  -- GHC flags
  | ParseImports !FilePath
  deriving (Eq, Show, Generic)

instance Binary WorkerCommand where put = genput; get = genget

------------------------------------------------------------------------------

-- | An answer from the worker to the dispatcher.
data WorkerResponse
  = WorkerVersion ![Int]
  | GhcWorkerReady [T.Text] -- warnings
  | WorkerFailure !T.Text
  | ParsedImports !ModuleHeader
  | SourceErrors [Message]
  deriving (Eq, Show, Generic)

instance Binary WorkerResponse where put = genput; get = genget

------------------------------------------------------------------------------

-- | A module name.  It may include dots, e.g., @Data.Map@
data ModuleName = ModuleName !T.Text
  deriving (Eq, Ord, Show, Read, Generic)

instance Binary ModuleName where put = genput; get = genget

mkModuleName :: T.Text -> ModuleName
mkModuleName = ModuleName

moduleName :: ModuleName -> T.Text
moduleName (ModuleName name) = name

------------------------------------------------------------------------------

-- | A single @import@ line in a module.
--
-- TODO: This currently does not include the import list or whether the module
-- is imported qualified.
data ImportDependency = ImportDependency
  { importModuleName  :: !ModuleName
  , importPackageName :: !(Maybe T.Text)
    -- ^ An explicitly specified package name (allowed by GHC).
  , importSource      :: !Bool
    -- ^ Whether the module is imported with @{-# SOURCE #-}@ pragma.
  , importSafe        :: !Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance Binary ImportDependency where put = genput; get = genget

------------------------------------------------------------------------------

-- | The high-level information about a module, used for dependency analysis.
data ModuleHeader = ModuleHeader
  { moduleHeaderModuleName   :: !ModuleName
  , moduleHeaderOptions      :: [T.Text]
    -- ^ Flags and language pragmas defined at the top of the module.
    --
    -- Note that a language pragma @{-# LANGUAGE Foo #-}@ gets translated into a
    -- flag @-XFoo@.
  , moduleHeaderImports      :: [ImportDependency]
  } deriving (Eq, Ord, Show, Read, Generic)

instance Binary ModuleHeader where put = genput; get = genget

------------------------------------------------------------------------------

-- | A range in the source program.  Both line and column indexes are
-- zero-based, i.e., @(0, 0)@ is the first position in every text file.
data SourceSpan = SourceSpan {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show, Generic)

instance Binary SourceSpan where put = genput; get = genget

instance Monoid SourceSpan where
  mempty = SourceSpan 0 0 (-1) (-1)
  mappend (SourceSpan l1 c1 l2 c2) (SourceSpan l3 c3 l4 c4) =
    SourceSpan lmin cmin lmax cmax
   where (lmin, cmin) = min (l1, c1) (l3, c3)
         (lmax, cmax) = max (l2, c2) (l4, c4)

-- instance ToJSON SourceSpan where
--   toJSON (SourceSpan l1 c1 l2 c2) =
--     object ["span" .= toJSON [l1, c1, l2, c2]]

------------------------------------------------------------------------------

data Severity = Warning | Error
  deriving (Eq, Ord, Show, Generic)

-- | An error message produced by a tool.
data Message = Message
  { msgSeverity :: !Severity
  , msgSpan     :: !SourceSpan
  , msgInfo     :: !MessageInfo
  } deriving (Eq, Ord, Show, Generic)

data MessageInfo
  = OtherMessage !T.Text
  | NotInScope !T.Text
  deriving (Eq, Ord, Show, Generic)

instance Binary Severity    where put = genput; get = genget
instance Binary MessageInfo where put = genput; get = genget
instance Binary Message     where put = genput; get = genget

------------------------------------------------------------------------------

