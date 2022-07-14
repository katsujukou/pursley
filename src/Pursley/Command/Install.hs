module Pursley.Command.Install
  ( InstallOptions (..),
    install,
  )
where

import Network.HTTP.Conduit (newManager, parseRequest, tlsManagerSettings)
import Pursley.Console (echo, echoShow)
import Pursley.Prelude
import Pursley.Types (PackageName)

data InstallOptions = InstallOptions
  { insInputs :: ![PackageName],
    insNoCache :: !Bool
  }
  deriving (Eq, Show)

install :: InstallOptions -> RIO env ()
install opts = do
  echo "Not implemented yet!😭"
  echoShow opts
-- req <- parseRequest "https://www.google.com"
-- manager <- newManager tlsManagerSettings
