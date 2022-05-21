module LinearAlgebra.Vector
  ( Vector
  , fromArray
  , toArray
  , fromFunction
  , null
  , elem
  , add
  , diff
  , dot
  , smult
  , opposite
  , colinear
  ) where

import Prelude hiding (add)
import Data.Array ((..), (!!), all, foldl, length, zipWith, uncons)
import Data.Maybe (Maybe(..), fromMaybe)

newtype Vector a
  = Vector (Array a)

derive instance Eq a => Eq (Vector a)
derive instance Functor Vector

instance Show a => Show (Vector a) where
  show (Vector v) = "(Vector " <> show v <> ")" 

fromArray :: forall a. Array a -> Vector a
fromArray = Vector

toArray :: forall a. Vector a -> Array a
toArray (Vector v) = v

fromFunction :: forall a. Int -> (Int -> a) -> Vector a
fromFunction n f = Vector $ f <$> 0 .. (n - 1)

-- | returns the element at index i
-- | returns zero if the index are not valid
elem :: forall a. Semiring a => Int -> Vector a -> a
elem i v = fromMaybe zero $ elem' i v

elem' :: forall a. Int -> Vector a -> Maybe a
elem' i (Vector m) = m !! i

-- | tests if the vector is null i.e. contains only zero values
null :: forall a. Eq a => Semiring a => Vector a -> Boolean
null (Vector v) = all (_ == zero) v

add :: forall a. Semiring a => Vector a -> Vector a -> Vector a
add (Vector v1) (Vector v2) = Vector $ zipWith (+) v1 v2

diff :: forall a. Ring a => Vector a -> Vector a -> Vector a
diff v1 v2 = v1 `add` (-one `smult` v2)

-- | dot product between two vectors.
-- | https://en.wikipedia.org/wiki/Dot_product
dot :: forall a. Semiring a => Vector a -> Vector a -> a
dot (Vector v1) (Vector v2) = foldl (+) zero $ zipWith (*) v1 v2

-- | scalar multiplication
smult :: forall a. Semiring a => a -> Vector a -> Vector a
smult a (Vector v) = Vector $ map (a * _) v

opposite :: forall a. Ring a => Vector a -> Vector a
opposite (Vector v) = Vector $ map (zero - _) v

-- | returns true if v1 and v2 are colinear i.e. there exists a scalar n such that v1 = n v2
-- | https://en.wikipedia.org/wiki/Collinearity
colinear :: forall a. Eq a => Field a => Vector a -> Vector a -> Boolean
colinear (Vector v1) (Vector v2)
  | length v1 /= length v2 = false
  | otherwise = case uncons $ zipWith (/) v1 v2 of
    Nothing -> true
    Just { head, tail } -> all (_ == head) tail
