module Control.Applicative.Validation
  ( V (..),
    validation,
    invalid,
    isValid,
    toEither,
    andThen,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (Bifunctor)
import Data.Functor.Classes (Eq1, Ord1)
import Prelude

newtype V err result = V (Either err result)
  deriving newtype (Functor, Bifunctor)

validation :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
validation f _ (V (Left err)) = f err
validation _ g (V (Right result)) = g result

-- | Fail with a validation error.
invalid :: forall err result. err -> V err result
invalid = V . Left

-- | Test whether validation was successful or not.
isValid :: forall err result. V err result -> Bool
isValid (V (Right _)) = True
isValid _ = False

toEither :: forall err result. V err result -> Either err result
toEither (V e) = e

andThen :: forall err a b. V err a -> (a -> V err b) -> V err b
andThen v1 f =
  validation invalid f v1

deriving instance (Eq err, Eq result) => Eq (V err result)

deriving instance Eq err => Eq1 (V err)

deriving instance (Ord err, Ord result) => Ord (V err result)

deriving instance Ord err => Ord1 (V err)

instance (Show err, Show result) => Show (V err result) where
  show = \case
    V (Left err) -> "invalid (" <> show err <> ")"
    V (Right result) -> "pure (" <> show result <> ")"

instance Semigroup err => Applicative (V err) where
  pure = V . Right
  (V ef) <*> (V ea) = case (ef, ea) of
    (Left err1, Left err2) -> V $ Left (err1 <> err2)
    (Left err, _) -> V $ Left err
    (_, Left err) -> V $ Left err
    (Right f, Right x) -> V $ Right (f x)

instance (Semigroup err, Semigroup a) => Semigroup (V err a) where
  (<>) = liftA2 (<>)

instance (Semigroup err, Monoid a) => Monoid (V err a) where
  mempty = pure mempty

instance Foldable (V err) where
  foldMap = validation (const mempty)
  foldr f b = validation (const b) (`f` b)
  foldl f b = validation (const b) (f b)

instance Traversable (V err) where
  sequence = validation (pure . V . Left) (fmap (V . Right))
  traverse f = validation (pure . V . Left) (fmap (V . Right) . f)