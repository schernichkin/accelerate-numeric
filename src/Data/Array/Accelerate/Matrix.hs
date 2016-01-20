{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Array.Accelerate.Matrix
  ( minv
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.GaussianElimination
import Prelude hiding ( (<*) )

minv :: ( Elt e, IsFloating e )
     => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
minv a = eliminate $ generate (lift $ Z :. m :. n + m) f
  where
    Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift $ shape a
    f ix =
      let Z :. (y :: Exp Int) :. (x :: Exp Int) = unlift ix
      in  x <* m ? ( a ! ix, x - m ==* y ? ( 1, 0 ) )
