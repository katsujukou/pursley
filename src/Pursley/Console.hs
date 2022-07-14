module Pursley.Console
  ( echo,
    echoT,
    echoShow,
  )
where

import Pursley.Prelude
import qualified Turtle

echoT :: forall m. MonadIO m => Text -> m ()
echoT = traverse_ Turtle.echo . Turtle.textToLines

echo :: forall m. MonadIO m => String -> m ()
echo = liftIO . Turtle.echo . Turtle.fromString

echoShow :: forall m a. (MonadIO m, Show a) => a -> m ()
echoShow = echo . show