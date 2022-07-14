module Pursley.Fetch
  ( URL,
  )
where

import Conduit
import Data.Conduit.Binary (sinkLbs)
import Data.String (String)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Simple
import Pursley.Prelude
import qualified RIO.Map as M

type URL = String

data FetchTarballStatus
  = Downloading !(Maybe (Int, Int))
  | Extracting !FilePath
  | Done
  | Failed !Text
  deriving (Eq, Show)

data FetchTarballURL = FetchTarballURL
  { ftbLabel :: !Text,
    ftbSrc :: !URL,
    ftbDest :: !FilePath
  }

-- fetchTarballWithProgress :: forall io t. (MonadThrow io, MonadUnliftIO io, Traversable t) => t FetchTarballURL -> io ()
-- fetchTarballWithProgress inputs = do
--   stats <- newTVarIO (M.empty :: M.Map Text FetchTarballStatus)
--   forM_ inputs \(FetchTarballURL label src dest) -> async $ do
--     req <- parseRequest src
--     runConduitRes do
--       httpSource req (getRes stats label)
--         .| printC
--   where
--     getRes stats label res = do
--       case getResponseStatusCode res of
--         code
--           | code == 200 -> do

--               getResponseBody res
--                 .| sinkLbs
--           -- atomically modifyTVar' stats $ M.update (\_ -> Just (Failed body)) label
--           -- sinkNull
--           | otherwise -> do
--               getResponseBody res