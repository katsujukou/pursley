module Pursley.Env
  ( HasEnv,
    HasCacheDir,
    HasInstallEnv,
    Env (..),
    InstallEnv (..),
  )
where

import Data.Generics.Product (HasType)
import Pursley.Config (PursleyConfig)
import Pursley.Prelude
import Pursley.Types

type HasCacheDir env = HasType CacheDir env

type HasPursVersion env = HasType PursVersion env

type HasPackageSet env = HasType PackageSet env

type HasConfig env = HasType PursleyConfig env

type HasEnv env =
  ( HasCacheDir env,
    HasPursVersion env
  )

type HasInstallEnv env =
  ( HasCacheDir env,
    HasPackageSet env,
    HasConfig env
  )

data Env = Env
  { envCacheDir :: !CacheDir,
    envPursVersion :: !PursVersion
  }
  deriving (Eq, Show, Generic)

data InstallEnv = InstallEnv
  { envCacheDir :: !CacheDir,
    envPursVersion :: !PursVersion,
    envPackageSet :: !PackageSet,
    envConfig :: !PursleyConfig
  }
  deriving (Eq, Show, Generic)