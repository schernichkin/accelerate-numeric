{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Linear algebra algorithms
module Data.Array.Accelerate.Algo.LA
  ( eliminate
  , dotProduct
  , madd
  , mid
  , minv
  , mmul
  , mscale
  , pinvc
  , pinvr
  ) where

import Data.Array.Accelerate
import Prelude hiding ( (<*), drop, map, replicate, snd, tail, zip, zipWith )

dotProduct :: ( Elt e, IsNum e )
           => Acc (Vector e)
           -> Acc (Vector e)
           -> Acc (Scalar e)
dotProduct x y = fold (+) 0 $ zipWith (*) x y

-- | Identity matrix
mid :: ( Elt e, IsNum e )
    => Exp Int
    -> Acc (Array DIM2 e)
mid n = generate (lift $ Z:.n:.n) f
  where
    f ix =
      let Z :. (y :: Exp Int) :. (x :: Exp Int) = unlift ix
      in y ==* x ? (1, 0)

-- | Calculate matrix inverse O(n^3)
minv :: ( Elt e, IsFloating e )
     => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
minv a = eliminate $ generate (lift $ Z :. m :. n + m) f
  where
    Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift $ shape a
    f ix =
      let Z :. (y :: Exp Int) :. (x :: Exp Int) = unlift ix
      in  x <* m ? ( a ! ix, x - m ==* y ? ( 1, 0 ) )

-- | Calculate Moore–Penrose pseudoinverse assuming
-- that the columns are linearly independent O(n^3)
pinvc :: ( Elt e, IsFloating e )
      => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
pinvc a = mmult (minv $ mmul (transpose a) a) a

-- | Calculate Moore–Penrose pseudoinverse assuming
-- that the rows are linearly independent O(n^3)
pinvr :: ( Elt e, IsFloating e )
      => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
pinvr a = mmul (transpose a) (minv $ mmult a a)

-- | Sum of two matrices O(n^2)
madd :: ( Elt e, IsNum e )
     => Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
madd = zipWith (+)

-- | Matrix multiplication O(n^3)
mmul :: ( Elt e, IsNum e )
     => Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
mmul a = mmult a . transpose

-- | Multiply matrix by transposed matrix O(n^3)
mmult :: ( Elt e, IsNum e )
     => Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
mmult a b = fold1 (+) $ zipWith (*) ar br
  where
    Z :. (ma :: Exp Int) :. (_ :: Exp Int) = unlift $ shape a
    Z :. (mb :: Exp Int) :. (_ :: Exp Int) = unlift $ shape b
    ar = replicate (lift $ Z :. All :. mb  :. All) a
    br = replicate (lift $ Z :. ma  :. All :. All) b

-- | Multiply matrix by scalar O(n^2)
mscale :: ( Elt e, IsNum e )
       => Acc (Scalar e)
       -> Acc (Array DIM2 e)
       -> Acc (Array DIM2 e)
mscale = map . (*) . the

-- | Add index to each array element
indexed :: ( Shape sh, Elt e ) => Acc (Array sh e) -> Acc (Array sh (sh,e))
indexed xs = zip (generate (shape xs) id) xs

-- | Return maximum element with it index.
imaxBy :: ( Shape ix, Elt a, Elt b, IsScalar b )
       => (Exp a -> Exp b) -> Acc (Array ix a) -> Acc (Scalar (ix, a))
imaxBy f = fold1All g . indexed
  where
    g a b = (f (snd a) >* f (snd b)) ? (a, b)

-- | Perform elimination step.
-- First column should contain at least one non-zero element
eliminationStep :: ( Elt e, IsFloating e )
                => Exp Int
                -> Acc (Array DIM2 e)
                -> Acc (Array DIM2 e)
eliminationStep i a = generate (lift $ Z :. m :. (n - 1)) f
  where
    Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift $ shape a
    (pivotRowIndex', pivot) = unlift $ the $ imaxBy abs $ drop i $ slice a $ lift $ Z :. All :. (0 :: Int)
    pivotRowIndex = unindex1 pivotRowIndex' + i -- adjust row index since we have dropped first n elements
    pivotRow = map (/ pivot) $ tail $ slice a (lift $ Z :. pivotRowIndex :. All)
    f ix =
      let Z :. (y' :: Exp Int) :. (x :: Exp Int) = unlift ix
      in  y' ==* i ?
          ( pivotRow ! index1 x
          , let y = y' ==* pivotRowIndex ? ( i, y' )
            in a ! index2 y (x + 1) - a ! index2 y 0 * pivotRow ! index1 x
          )

-- | Gaussian elimination with partial pivoting assuming
-- that the rows are linearly independent O(n^3)
eliminate :: ( Elt e, IsFloating e )
          => Acc (Array DIM2 e)
          -> Acc (Array DIM2 e)
eliminate arr = asnd $ awhile (unit . (<* y) . the . afst) step $ lift (unit 0, arr)
  where
    (Z :. (y :: Exp Int) :. (_ :: Exp Int)) = unlift $ shape arr
    step args =
      let n = the $ afst args
      in  lift (unit $ n + 1, eliminationStep n (asnd args))

-- TODO: Row echelon form transformation
