module Pursley.Error
  ( PursleyError (..),
    describe,
    errorMessageLines,
  )
where

import Pursley.Config (PursleyConfigError)
import qualified Pursley.Config as Config
import Pursley.Prelude
import qualified RIO.Text as T

data PursleyError
  = -- Command, Exit Code. Error Messages
    ChildProcessError !Text !Int ![Text]
  | -- Target revision (branch, commit hash, tag, ...) , Error Messages
    GitCheckoutError !Text ![Text]
  | -- Args, Error Messages
    GitTagsError !Text ![Text]
  | ConfigJsonNotFound !FilePath
  | ConfigJsonError !FilePath ![PursleyConfigError]
  deriving (Eq, Show)

errorMessageLines :: PursleyError -> [Text]
errorMessageLines = \case
  ChildProcessError _ _ errs -> errs
  GitCheckoutError _ errs -> errs
  GitTagsError _ errs -> errs

describe :: PursleyError -> Text
describe = \case
  (ChildProcessError cmd _ err) ->
    T.unlines $
      [ "An error occurred during executing",
        "  command: " <> cmd,
        "  message: "
      ]
        ++ map ("    " <>) err
  (GitCheckoutError target err) ->
    T.unlines $
      [ "An error occurred during checking out git revision: " <> target,
        "  message: "
      ]
        ++ map ("    " <>) err
  (GitTagsError _ err) ->
    T.unlines $
      [ "An error occurred during executing git tag",
        "  message: "
      ]
        ++ map ("    " <>) err
  (ConfigJsonNotFound fp) ->
    "An error occurred during reading config from "
      <> T.pack fp
      <> ":\n"
      <> "  <warn><bold>File not found.</bold><warn>"
  (ConfigJsonError fp errs) ->
    T.unlines $
      "An error occurred during reading config from " <> T.pack fp <> ":"
        : map Config.describe errs