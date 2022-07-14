module Pursley.Types
  ( PackageName,
    unPackageName,
    packageNameParser,
    PackageInfo (..),
    PackageSet (..),
    CacheDir (..),
    PursVersion (..),
    unCacheDir,
  )
where

import Pursley.Prelude
import qualified RIO.Text as T
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)

newtype PackageName = PackageName Text
  deriving (Eq, Ord, Show)

unPackageName :: PackageName -> Text
unPackageName (PackageName pn) = pn

type PackageNameParser = Parsec.Parsec Void Text PackageName

packageNameParser :: PackageNameParser
packageNameParser =
  PackageName <$> do
    head <- T.singleton <$> letterChar
    rest <- T.pack <$> many (alphaNumChar <|> char '-')
    pure (head <> rest)

data PackageInfo = PackageInfo
  { pkginfoDependencies :: !(Set PackageName),
    pkginfoRepo :: !Text,
    pkginfoVersion :: !Text
  }
  deriving (Eq, Show, Generic)

newtype PackageSet = PackageSet (Map PackageName PackageInfo)
  deriving newtype (Eq, Show)

newtype PursVersion = PursVersion Text
  deriving newtype (Eq, Show, IsString)

newtype CacheDir = CacheDir Text
  deriving newtype (Eq, Show, IsString)

unCacheDir :: CacheDir -> FilePath
unCacheDir (CacheDir cd) = T.unpack cd