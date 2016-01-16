{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Gaussian elimination with partial pivoting
module Data.Array.Accelerate.GaussianElimination
  ( eliminate
  ) where

import Data.Array.Accelerate
import Prelude hiding ( zip, map, (<*), snd, drop )

-- | Extract column from row-major ordered matrix.
col :: ( Elt e
       , Slice (Plain ((Z :. All) :. head))
       , Lift Exp ((Z :. All) :. head) )
    => head
    -> Acc (Array (FullShape (Plain ((Z :. All) :. head))) e)
    -> Acc (Array (SliceShape (Plain ((Z :. All) :. head))) e)
col i arr = slice arr $ lift $ Z :. All :. i

-- | Add index to each array element
indexed :: (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Array sh (sh,e))
indexed xs = zip (generate (shape xs) id) xs

-- | Return maximum element with it index.
imaxBy :: forall ix a b . ( Shape ix, Elt a, Elt b, IsScalar b )
     => (Exp a -> Exp b) -> Acc (Array ix a) -> Acc (Scalar (ix, a))
imaxBy f = fold1All g . indexed
  where
    g a b = (f (snd a) >* f (snd b)) ? (a, b)

-- | Map array elements with index information
imap :: forall ix a b . ( Shape ix, Elt a, Elt b )
     => (Exp (ix, a) -> Exp b)
     -> Acc (Array ix a)
     -> Acc (Array ix b)
imap f = map f . indexed

-- Gaussian elimination step (for n's column)
eliminationStep :: forall e . ( Elt e, IsFloating e )
          => Exp Int -> Acc (Array DIM2 e) -> Acc (Scalar e, Array DIM2 e)
eliminationStep n arr = lift (unit pivot, (pivot /=* 0) ?| (eliminated, arr))
  where
    (pivotRowIndex', pivot) = unlift $ the $ imaxBy abs $ drop n $ col n arr
    pivotRowIndex = unindex1 pivotRowIndex' + n -- adjust row index since we have dropped first n elements
    eliminated = imap f arr
      where
        f args =
          let (idx, e) = unlift args :: (Exp DIM2, Exp e)
              (Z :. y :. x) = unlift idx
          in y ==* n ?
            ( x <* n ? (0, arr ! index2 pivotRowIndex x / pivot)
            , x ==* n ?
              ( 0
              , y ==* pivotRowIndex ?
                ( let k = arr ! index2 n n / pivot in (arr ! index2 n x) - (arr ! index2 pivotRowIndex x * k)
                , let k = arr ! index2 y n / pivot in e - (arr ! index2 pivotRowIndex x * k)
                )
              )
            )

-- | Gaussian elimination
eliminate :: forall e . ( Elt e, IsFloating e )
          => Acc (Array DIM2 e) -> Acc (Scalar Int, Array DIM2 e)
eliminate arr = lift (i, res)
  where
    (Z :. y :. _) = unlift $ shape arr :: (Z :. Exp Int :. Exp Int)
    (i, _, res) = unlift $ awhile canGo step $ lift (unit 0, unit 1, arr) :: (Acc (Scalar Int), Acc (Scalar e), Acc (Array DIM2 e))

    canGo args =
      let (n, p, _) = unlift args :: (Acc (Scalar Int), Acc (Scalar e), Acc (Array DIM2 e))
      in unit ((the p /=* 0) &&* (the n <* y))

    step args =
      let (n, _, a) = unlift args :: (Acc (Scalar Int), Acc (Scalar e), Acc (Array DIM2 e))
          (p, a') = unlift $ eliminationStep (the n) a :: (Acc (Scalar e), Acc (Array DIM2 e))
      in lift (unit $ the n + 1, p, a')
