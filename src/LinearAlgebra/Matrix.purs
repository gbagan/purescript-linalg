module LinearAlgebra.Matrix
  ( Matrix
  , Solutions
  , fromArray
  , fromFunction
  , identity
  , invalid
  , toArray
  , column
  , row
  , columns
  , rows
  , ncols
  , nrows
  , mapWithIndex
  , elem
  , elem'
  , add
  , diff
  , scale
  , mult
  , mult'
  , kronecker
  , transpose
  , inverse
  , kernel
  , image
  , determinant
  , rank
  , trace
  , gaussJordan
  , solveLinearSystem
  , solveLinearSystem'
  ) where

import Prelude hiding (add)
import Data.Array ((..), (!!), all, any, filter, find, foldl, length, null, replicate, updateAtIndices, uncons, zipWith)
import Data.Array as Array
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import LinearAlgebra.Vector as V

newtype Matrix a
  = Matrix { r :: Int, c :: Int, m :: Array (Array a) }

derive instance Eq a => Eq (Matrix a)

instance Functor Matrix where
    map f (Matrix {r, c, m}) = Matrix {r, c, m: map (map f) m}

instance Show a => Show (Matrix a) where
    show (Matrix {r, c, m}) | r == 0 = "Invalid matrix"
                            | otherwise = "(Matrix " <> show r <> " " <> show c <> " " <> show m <> ")"

invalid :: forall a. Matrix a
invalid = Matrix { r: 0, c: 0, m: [] }

fromFunction :: forall a. Int -> Int -> (Int -> Int -> a) -> Matrix a
fromFunction r c f = Matrix { r, c, m: (0 .. (r - 1)) <#> \i -> (0 .. (c - 1)) <#> f i }

fromArray :: forall a. Semiring a => Int -> Int -> Array (Array a) -> Matrix a
fromArray r c m = fromFunction r c \i j -> fromMaybe zero $ m !! i >>= (_ !! j) 

toArray :: forall a. Matrix a -> Array (Array a)
toArray (Matrix { m }) = m

nrows :: forall a. Matrix a -> Int
nrows (Matrix { r }) = r

ncols :: forall a. Matrix a -> Int
ncols (Matrix { c }) = c

-- | returns the element at indices (i, j)
-- | returns zero if the indices are not valid
elem :: forall a. Semiring a => Int -> Int -> Matrix a -> a
elem i j m = fromMaybe zero $ elem' i j m

foreign import unsafeElem :: forall a. Fn3 (Matrix a) Int Int a

elem' :: forall a. Int -> Int -> Matrix a -> Maybe a
elem' i j (Matrix { m }) = m !! i >>= (_ !! j)

-- mapWithIndex is the bottleneck for many algorithms so it is implemented in javascript
foreign import _mapWithIndex :: forall a b. Fn4 (Array (Array a)) Int Int (Int -> Int -> a -> b) (Array (Array b))

mapWithIndex :: forall a b. (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapWithIndex f (Matrix { r, c, m }) = Matrix { r, c, m: runFn4 _mapWithIndex m r c f }

row :: forall a. Int -> Matrix a -> V.Vector a
row i (Matrix { m }) = fromMaybe (V.fromArray []) $ V.fromArray <$> m !! i

column :: forall a. Int -> Matrix a -> V.Vector a
column j m@(Matrix { r }) = fromMaybe (V.fromArray []) $ V.fromArray <$> traverse (\i -> elem' i j m) (0 .. (r - 1))

rows :: forall a. Matrix a -> Array (V.Vector a)
rows (Matrix { m }) = V.fromArray <$> m

columns :: forall a. Matrix a -> Array (V.Vector a)
columns = rows <<< transpose

-- | computes the identity matrix of dimension nxn
-- | https://en.wikipedia.org/wiki/Identity_matrix
identity :: forall a. Field a => Int -> Matrix a
identity n = fromFunction n n \i j -> if i == j then one else zero

-- | computes the transpose of the matrix
-- | https://en.wikipedia.org/wiki/Transpose
transpose :: forall a. Matrix a -> Matrix a
transpose m@(Matrix { r, c }) = fromFunction c r \i j -> runFn3 unsafeElem m j i

add :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
add m1 m2
  | nrows m1 /= nrows m2 || ncols m1 /= ncols m2 = fromArray 0 0  []
  | otherwise = fromFunction (nrows m1) (ncols m1) \i j -> elem i j m1 + elem i j m2

diff :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
diff m1 m2 = add m1 (-one `scale` m2)

-- | computes the (left) scalar multiplication of a matrice
-- | https://en.wikipedia.org/wiki/Scalar_multiplication
scale :: forall a. Semiring a => a -> Matrix a -> Matrix a
scale x = map (x * _)

mult :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
mult m1@(Matrix { r: r1, c: c1 }) m2@(Matrix { r: r2, c: c2 })
  | c1 == r2 = fromFunction r1 c2 \i j -> V.dot (row i m1) (column j m2)
mult _ _ = invalid

mult' :: forall a. Semiring a => Matrix a -> V.Vector a -> V.Vector a
mult' m@(Matrix { r }) v = V.fromFunction r \i -> V.dot (row i m) v

-- | Kronecker product
-- | https://en.wikipedia.org/wiki/Kronecker_product

kronecker :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
kronecker m@(Matrix {r, c}) m'@(Matrix {r: r', c: c'}) =
  fromFunction (r * r') (c * c') \i j -> elem (i / r') (j / c') m * elem (i `mod` r') (j `mod` c') m'


mapRow :: forall a. Int -> (a -> a) -> Matrix a -> Matrix a
mapRow k f = mapWithIndex \i _ x -> if i == k then f x else x

swapRows :: forall a. Semiring a => Int -> Int -> Matrix a -> Matrix a
swapRows r1 r2 m = mapWithIndex fn m
  where
  fn i j x
    | i == r1 = runFn3 unsafeElem m r2 j
    | i == r2 = runFn3 unsafeElem m r1 j
    | otherwise = x

-- | computes the reduced row echelon form of the matrix and compute its determinant if the matrix is square
-- | see https://en.wikipedia.org/wiki/Row_echelon_form 
gaussJordan :: forall a. Eq a => Field a => Matrix a -> { mat :: Matrix a, det :: a }
gaussJordan m@(Matrix { r, c }) = { mat: res.mat, det: res.det }
  where
  res = foldl step { mat: m, pivot: 0, det: one } (0 .. (c - 1))

  step { mat, pivot, det } j = case range pivot (r - 1) # find \i -> elem i j mat /= zero of
    Nothing -> { mat, pivot, det: zero }
    Just k ->
      let
        v = elem k j mat
        mat2 = mapRow k (_ / v) mat
        mat3 = swapRows k pivot mat2
        mat4 =
          mat3
            # mapWithIndex \i j' x ->
                if i == pivot then x else x - (runFn3 unsafeElem mat3 i j) * (runFn3 unsafeElem mat3 pivot j')
      in
        { mat: mat4
        , pivot: pivot + 1
        , det: det * v * (if pivot == k then one else -one)
        }

  range n n'
    | n <= n' = n .. n'
    | otherwise = []

augmentedMatrix :: forall a. Semiring a => Matrix a -> Matrix a
augmentedMatrix m@(Matrix { r, c }) = fromFunction r (r + c) fAug
  where
  fAug i j
    | j < c = elem i j m
    | i == j - c = one
    | otherwise = zero

-- | computes the inverse of the matrix
-- | using Gauss-Jordan Elimination and augmented matrix
-- | https://en.wikipedia.org/wiki/Invertible_matrix#Gaussian_elimination
inverse :: forall a. Eq a => Field a => Matrix a -> Maybe (Matrix a)
inverse m@(Matrix { r, c })
  | r == c =
    if elem (r - 1) (r - 1) echelon == one then
      Just $ fromFunction r r \i j -> elem i (j + r) echelon
    else
      Nothing
    where
    echelon = _.mat $ gaussJordan $ augmentedMatrix m

inverse _ = Nothing

-- | computes the trace of the matrix
-- | https://en.wikipedia.org/wiki/Trace_(linear_algebra)
trace :: forall a. Eq a => Semiring a => Matrix a -> a
trace m@(Matrix { r, c })
  | r == c = 0 .. (r - 1) # foldl (\acc i -> acc + elem i i m) zero

trace _ = zero

-- | computes the determinant of a square matrix
-- | https://en.wikipedia.org/wiki/Determinant
determinant :: forall a. Eq a => Field a => Matrix a -> a
determinant = _.det <<< gaussJordan

filterWithIndex :: forall a. (Int -> a -> Boolean) -> Array a -> Array a
filterWithIndex f t = _.val <$> filtered
  where
  zipped = zipWith { val: _, index: _ } t (0 .. (length t - 1))
  filtered = zipped # filter \{ val, index } -> f index val

imker :: forall a. Eq a => Field a => Matrix a -> { im :: Array (V.Vector a), ker :: Array (V.Vector a) }
imker m@(Matrix { r, c }) = { im, ker }
  where
  echelon = _.mat $ gaussJordan $ augmentedMatrix $ transpose m
  a = fromFunction c r \i j -> elem i j echelon
  b = fromFunction c c \i j -> elem i (j + r) echelon
  im = rows a # filter (not <<< V.null)
  ker = rows b # filterWithIndex \i _ -> V.null (row i a)

-- | computes a basis the image (or column space) of the matrix
-- | https://en.wikipedia.org/wiki/Row_and_column_spaces
image :: forall a. Eq a => Field a => Matrix a -> Array (V.Vector a)
image = _.im <<< imker

-- | computes a basis for the kernel (or null space) of the matrix
-- | https://en.wikipedia.org/wiki/Kernel_(linear_algebra)
kernel :: forall a. Eq a => Field a => Matrix a -> Array (V.Vector a)
kernel = _.ker <<< imker

-- | computes the rank of the matrix
rank :: forall a. Eq a => Field a => Matrix a -> Int
rank = length <<< _.im <<< imker

-- | Represents the set of solutions for the function solveLinearSystem.
-- | The set of solutions is { sol + v | v is a linear combination of vectors in basis }
type Solutions a
  = { sol :: V.Vector a, basis :: Array (V.Vector a) }

-- | solve the equation M x = b for a given matrix M and vector b
solveLinearSystem :: forall a. Eq a => Field a => Matrix a -> V.Vector a -> Maybe (Solutions a)
solveLinearSystem m b = { sol: _, basis: kernel m } <$> solveLinearSystem' m b

-- | same as solveLinearSystem but without basis
solveLinearSystem' :: forall a. Eq a => Field a => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solveLinearSystem' m@(Matrix { r, c }) b =
  -- if the last non zero row contains is of the form 0 = 1 then there is no solution 
  if r == 0 || (0 .. (c - 1) # all \j -> runFn3 unsafeElem echelon (r' - 1) j == zero) then
    Nothing
  else
    let
      toBeUpdated =
        0 .. (r' - 1)
          <#> ( \i ->
                -- k is the column of the leading one of the row i, k always exists
                let
                  k = fromMaybe 0 $ 0 .. (c - 1) # find \j -> runFn3 unsafeElem echelon i j == one
                in
                  k /\ runFn3 unsafeElem echelon i c
            )

      sol = V.fromArray $ updateAtIndices toBeUpdated (replicate c zero)
    in
      Just sol
  where
  augmented = fromFunction r (c + 1) \i j -> if j == c then V.elem i b else elem i j m
  echelon = removeZeroRows $ _.mat $ gaussJordan augmented
  r' = nrows echelon

removeZeroRows :: forall a. Eq a => Semiring a => Matrix a -> Matrix a
removeZeroRows (Matrix { c, m }) =
  let
    m' = filter (any (_ /= zero)) m
  in
    Matrix { r: Array.length m', c, m: m' }
