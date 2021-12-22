module Triple where

import Data.Monoid

data Triple a = Triple a a a deriving (Eq, Ord, Show, Read)

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Foldable Triple where
  foldMap f (Triple a b c) = f a <> f b <> f c
  foldr f init (Triple a b c) = f a $ f b $ f c init

instance Traversable Triple where
  traverse f (Triple a b c) = Triple <$> f a <*> f b <*> f c
  sequenceA (Triple a b c) = Triple <$> a <*> b <*> c
