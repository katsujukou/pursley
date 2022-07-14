module Pursley.FileSystem
  ( makeDirectoryRecursive,
    resolvePathUnixStyle,
  )
where

import Pursley.Prelude
import RIO.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath (isPathSeparator, joinPath, pathSeparator, (</>))
import qualified Turtle

makeDirectoryRecursive :: MonadIO m => FilePath -> m FilePath
makeDirectoryRecursive [] = pure ""
makeDirectoryRecursive fp@(f : _) =
  case wordsWhen isPathSeparator fp of
    [] -> pure ""
    (seg : segs) -> do
      parent <- case seg of
        "~" -> Turtle.home
        _
          | f == pathSeparator -> pure (pathSeparator : seg)
          | otherwise -> pure seg
      go parent segs
      where
        go p [] = pure p
        go parent (s : ss) = do
          let cur = parent </> s
          whenM (not <$> doesDirectoryExist cur) do
            Turtle.mkdir cur
          go cur ss

resolvePathUnixStyle :: forall io. MonadIO io => FilePath -> io FilePath
resolvePathUnixStyle fp =
  canonicalizePath =<< do
    case wordsWhen isPathSeparator fp of
      "~" : rest -> (</> joinPath rest) <$> Turtle.home
      _ -> pure fp
