{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Array.Accelerate.Matrix
  ( minv
  , mmul
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.GaussianElimination
import Prelude hiding ( (<*), replicate, zipWith )

-- | Calculate matrix inverse.
minv :: ( Elt e, IsFloating e )
     => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
minv a = eliminate $ generate (lift $ Z :. m :. n + m) f
  where
    Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift $ shape a
    f ix =
      let Z :. (y :: Exp Int) :. (x :: Exp Int) = unlift ix
      in  x <* m ? ( a ! ix, x - m ==* y ? ( 1, 0 ) )

-- | Matrix multiplication
mmul :: ( Elt e, IsNum e )
     => Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
     -> Acc (Array DIM2 e)
mmul a = mmult a . transpose

-- | Multiply matrix by transposed matrix
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
