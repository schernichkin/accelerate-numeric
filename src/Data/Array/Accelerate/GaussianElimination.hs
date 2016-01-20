{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Gaussian elimination with partial pivoting
module Data.Array.Accelerate.GaussianElimination
  ( eliminate
  ) where

import Data.Array.Accelerate
import Prelude hiding ( (<*), drop, map, snd, tail, zip )

-- | Add index to each array element
indexed :: (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Array sh (sh,e))
indexed xs = zip (generate (shape xs) id) xs

-- | Return maximum element with it index.
imaxBy :: forall ix a b . ( Shape ix, Elt a, Elt b, IsScalar b )
       => (Exp a -> Exp b) -> Acc (Array ix a) -> Acc (Scalar (ix, a))
imaxBy f = fold1All g . indexed
  where
    g a b = (f (snd a) >* f (snd b)) ? (a, b)

-- | Perform elimination step.
-- First column should contain at least one non-zero element
eliminationStep :: forall e . ( Elt e, IsFloating e )
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

eliminate :: forall e . ( Elt e, IsFloating e )
          => Acc (Array DIM2 e)
          -> Acc (Array DIM2 e)
eliminate arr = asnd $ awhile (unit . (<* y) . the . afst) step $ lift (unit 0, arr)
  where
    (Z :. (y :: Exp Int) :. (_ :: Exp Int)) = unlift $ shape arr
    step args =
      let n = the $ afst args
      in  lift (unit $ n + 1, eliminationStep n (asnd args))
