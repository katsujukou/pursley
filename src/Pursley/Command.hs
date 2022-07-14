module Pursley.Command
  ( GlobalOptions (..),
    Options (..),
    Command (..),
    program,
  )
where

import qualified Pursley.Command.Init as Init
import qualified Pursley.Command.Install as Install
import qualified Pursley.Command.Setup as Setup
import qualified Pursley.Command.Update as Update
import Pursley.Env (HasEnv)
import Pursley.Prelude
import Pursley.Types (CacheDir)

-- runPursley :: Env -> RIO Env ~> IO
-- runPursley = runRIO

data GlobalOptions = GlobalOptions
  { gloptsCacheDir :: !CacheDir,
    gloptsNoColor :: !Bool,
    gloptsQuiet :: !Bool
  }
  deriving (Eq, Show)

data Options = Options
  { optGlobalOptions :: !GlobalOptions,
    optCommand :: !Command
  }
  deriving (Eq, Show)

data Command
  = Update Update.UpdateOptions
  | Setup Setup.SetupOptions
  | Init Init.InitOptions
  | Install Install.InstallOptions
  deriving (Eq, Show)

program :: HasEnv env => Command -> RIO env ()
program = \case
  Update updateOpts -> do
    Update.update updateOpts
  Setup setupOpts -> do
    Setup.setup setupOpts
  Init initOpts -> do
    Init.init initOpts
  Install insOpts -> do
    Install.install insOpts
