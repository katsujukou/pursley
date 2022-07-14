module Pursley.Command.Setup
  ( SetupOptions (..),
    setup,
  )
where

import Pursley.Console (echo, echoShow)
import Pursley.Prelude

data SetupOptions = SetupOptions
  { setupPinning :: !(Maybe Text)
  }
  deriving (Eq, Show)

setup :: forall env. SetupOptions -> RIO env ()
setup opts = do
  echo "Not implemented yet!"
  echoShow opts
