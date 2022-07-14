module Pursley.Run where

import Pursley.Command (GlobalOptions (GlobalOptions, gloptsCacheDir))
import Pursley.Env (Env (..))
import Pursley.FileSystem (resolvePathUnixStyle)
import Pursley.Prelude
import Pursley.Types (CacheDir (CacheDir), PursVersion (PursVersion), unCacheDir)
import qualified RIO.Text as T

withEnv :: GlobalOptions -> RIO Env a -> IO a
withEnv GlobalOptions {..} app = do
  cacheDir <- resolvePathUnixStyle $ unCacheDir gloptsCacheDir
  let envCacheDir = CacheDir $ T.pack cacheDir
  let envPursVersion = PursVersion "0.15.3"
  let env = Env {..}
  runRIO env app