module Pursley.Config
  ( PursleyConfig,
    parseConfigJson,
    PursleyConfigError (..),
    describe,
  )
where

import Control.Applicative.Validation (V (V), invalid, toEither)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.List as L
import Pursley.Prelude
import Pursley.Types (PackageName, PursVersion (..), packageNameParser)
import qualified RIO.ByteString.Lazy as BSL
import RIO.Lens (_Left)
import qualified RIO.Set as S
import qualified RIO.Text as T
import qualified System.FilePath as FilePath
import qualified Text.Megaparsec as Megaparsec

data PursleyConfigJSON = PursleyConfigJSON
  { name :: !Text,
    version :: !Text,
    pursVersion :: !(Maybe Text),
    sources :: !(Maybe [FilePath]),
    dependencies :: !(Maybe [Text]),
    test :: Maybe PursleyTestConfigJSON
  }
  deriving (Eq, Show, Generic)

instance FromJSON PursleyConfigJSON

data PursleyTestConfigJSON = PursleyTestConfigJSON
  { sources :: ![FilePath],
    dependencies :: !(Maybe [Text])
  }
  deriving (Eq, Show, Generic)

instance FromJSON PursleyTestConfigJSON

data PursleyConfig = PursleyConfig
  { confName :: !PackageName,
    confVersion :: !Text,
    confPursVersion :: !(Maybe PursVersion),
    confSources :: ![FilePath],
    confDependencies :: !(Set PackageName),
    confTestSources :: ![FilePath],
    confTestDependencies :: !(Set PackageName)
  }
  deriving (Eq, Show, Generic)

data PursleyConfigError
  = JsonParseError !Text
  | InvalidValue !Text !Text
  deriving (Eq, Ord, Show)

fromPursleyConfigJSON :: PursleyConfigJSON -> Either [PursleyConfigError] PursleyConfig
fromPursleyConfigJSON json =
  toEither $
    PursleyConfig
      <$> validatePackageName "name" json.name
      <*> pure json.version
      <*> validateVersion json.pursVersion
      <*> traverse (validateFilePath "sources") (fromMaybe ["src"] json.sources)
      <*> fmap S.fromList (traverse (validatePackageName "dependencies") (fromMaybe [] json.dependencies))
      <*> traverse (validateFilePath "test.sources") (fromMaybe ["test"] $ json.test <&> \v -> v.sources)
      <*> fmap S.fromList (traverse (validatePackageName "test.dependencies") (fromMaybe [] $ json.test >>= \v -> v.dependencies))
  where
    validateVersion = pure . fmap PursVersion

    validatePackageName item =
      V
        . over _Left (const [InvalidValue item "It should contain only alphanumeric characters and hyphens(-)"])
        . Megaparsec.runParser packageNameParser ""

    validateFilePath item fp
      | not (FilePath.isValid fp) = invalid [InvalidValue item "It should be a valid file path"]
      | FilePath.isAbsolute fp = invalid [InvalidValue item "It should not be an absolute path"]
      | otherwise = pure fp

parseConfigJson :: BSL.ByteString -> Either [PursleyConfigError] PursleyConfig
parseConfigJson =
  let parseJson :: BSL.ByteString -> Either [PursleyConfigError] PursleyConfigJSON
      parseJson = over _Left (L.singleton . JsonParseError . T.pack) . Aeson.eitherDecode
   in fromPursleyConfigJSON <=< parseJson

describe :: PursleyConfigError -> Text
describe = \case
  JsonParseError err -> "<error>" <> err <> "</error>"
  InvalidValue item hint ->
    T.unlines
      [ " - <error><bold>Invalid value at " <> item <> "</bold></error>:",
        "    <warn>" <> hint <> "</warn>"
      ]