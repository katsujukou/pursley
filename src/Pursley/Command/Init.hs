module Pursley.Command.Init
  ( InitOptions (..),
    init,
  )
where

import Pursley.Console (echo, echoShow)
import Pursley.Prelude

data InitOptions = InitOptions
  deriving (Eq, Show)

init :: InitOptions -> RIO env ()
init opts = do
  echo "Not implemented yet!"
  echoShow opts