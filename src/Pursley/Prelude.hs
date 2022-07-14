module Pursley.Prelude
  ( hush,
    note,
    wordsWhen,
    module RIO,
  )
where

import RIO

hush :: forall a b. Either a b -> Maybe b
hush (Right b) = Just b
hush _ = Nothing

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'