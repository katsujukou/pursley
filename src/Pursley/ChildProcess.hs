module Pursley.ChildProcess
  ( exec,
    exec',
  )
where

import Pursley.Error (PursleyError (ChildProcessError))
import Pursley.Prelude
import qualified RIO.Text as T
import qualified System.IO as IO
import System.Process (CmdSpec (RawCommand, ShellCommand), CreateProcess (CreateProcess, cmdspec), readCreateProcessWithExitCode, showCommandForUser)

exec :: forall m. MonadIO m => CreateProcess -> String -> m (Either PursleyError [Text])
exec cp@CreateProcess {..} input = do
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode cp input
  case exitCode of
    ExitFailure code -> do
      pure $ Left $ ChildProcessError (cmdToText cmdspec) code (map T.pack . lines $ err)
    _ -> pure $ Right (map T.pack . lines $ out)
  where
    cmdToText (ShellCommand s) = T.pack s
    cmdToText (RawCommand s args) = T.pack $ showCommandForUser s args

exec' :: forall m. MonadIO m => CreateProcess -> m (Either PursleyError [Text])
exec' cp = exec cp ""
