module Pursley.Command.Update where

import Conduit (ConduitT, await, runConduitRes, sinkNull, yield, (.|))
import qualified Data.ByteString.Char8 as C8 (readInt)
import Network.HTTP.Simple (getResponseBody, getResponseHeader, getResponseStatusCode, httpSource)
import Pursley.Env (HasEnv)
import Pursley.Prelude
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS
import RIO.List (headMaybe)
import qualified RIO.List as L
import qualified System.IO as IO

data UpdateOptions = UpdateOptions
  { updatePinTag :: !(Maybe Text),
    updatePinCommit :: !(Maybe Text)
  }
  deriving (Eq, Show)

update ::
  forall env.
  HasEnv env =>
  UpdateOptions ->
  RIO env ()
update UpdateOptions {..} = do
  prog <- newTVarIO (Nothing :: Maybe (Int, Int))
  let thd1 = do
        runConduitRes do
          httpSource "https://github.com/purescript/package-sets/archive/psc-0.15.3.tar.gz" (getRes prog)
            .| progress prog
            .| sinkNull
      thd2 = loop prog
  _ <- liftIO $ race thd1 thd2
  return ()
  where
    loop prog = do
      threadDelay 16000
      IO.putStr "\x1b[2J"
      prog' <- readTVarIO prog
      case prog' of
        Just (cur, tot) -> do
          IO.putStr $ replicate (64 * cur `div` tot) '=' ++ ">\n"
        _ -> do
          IO.putStr "Downloading..."
      loop prog

    getRes prog res = do
      liftIO $ IO.print (getResponseStatusCode res)
      atomically $ do
        modifyTVar' prog \_ -> do
          cl <- headMaybe $ getResponseHeader "Content-Length" res
          totalSize <- fst <$> C8.readInt cl
          pure (0, totalSize)
      getResponseBody res

    progress prog = loop'
      where
        loop' = do
          threadDelay 10000
          mx <- await
          case mx of
            Nothing -> pure ()
            Just chunk -> do
              atomically $ do
                modifyTVar' prog (fmap (first (+ BS.length chunk)))
              yield chunk
              loop'

-- cacheDir <- unCacheDir <$> view (the @CacheDir)
-- let repoDir = cacheDir </> "repo"
-- unlessM (doesDirectoryExist repoDir) do
--   makeDirectoryRecursive repoDir
--   res <-
--     exec' $
--       (Process.shell "git clone https://github.com/purescript/package-sets.git .")
--         { Process.cwd = Just repoDir
--         }

--   case res of
--     Left err -> do
--       echoT $ "[ERROR] Failed to download the package set." <> describe err
--       exitFailure
--     _ -> do
--       pure ()
-- -- step2. checkout target revision
-- revOrErr <-
--   runExceptT $
--     checkoutOrInferTargetRev repoDir (updatePinCommit <|> updatePinTag)
-- case revOrErr of
--   Left e -> do
--     echoT $ describe e
--     exitFailure
--   Right rev -> do
--     echoT $ "[INFO] The package set has been updated to: " <> rev
--     pure ()
-- where
--   checkoutOrInferTargetRev repoDir mtarget = do
--     case mtarget of
--       Just target -> do
--         checkoutRev repoDir target
--       Nothing -> do
--         -- 設定されている purs のバージョンにマッチするタグのリストを取得
--         pv@(PursVersion pursVer) <- lift $ view (the @PursVersion)
--         let pattern = "\"psc-" <> dropPatchVer pv <> "*\""
--         res <-
--           exec' $
--             (Process.shell $ "git tag -l " <> pattern)
--               { Process.cwd = Just repoDir
--               }
--         case res of
--           Right tags -> do
--             let inferred = fromMaybe "master" . L.headMaybe . reverse $ tags
--             when (inferred == "master") do
--               echoT $
--                 "[WARN] Failed to determine the latest revision of package set which is compatible with purs-"
--                   <> pursVer
--                   <> ". Now the package set pins master revision."

--             checkoutRev repoDir inferred
--           Left err -> do
--             throwError $ GitTagsError (T.pack pattern) (errorMessageLines err)

--   checkoutRev repoDir target = do
--     res <-
--       exec' $
--         (Process.shell $ "git -c advice.detachedHead=false checkout " <> T.unpack target)
--           { Process.cwd = Just repoDir
--           }
--     case res of
--       Left err -> throwError $ GitCheckoutError target (errorMessageLines err)
--       Right _ -> pure target

--   dropPatchVer (PursVersion pv) = L.intercalate "." $ wordsWhen (== '.') $ T.unpack pv