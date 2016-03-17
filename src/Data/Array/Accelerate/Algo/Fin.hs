-- | Financial algorithms
module Data.Array.Accelerate.Algo.Fin
  ( compound
  , discount
  , presentValue
  ) where

import Data.Array.Accelerate
import Prelude hiding (sum, zipWith)

import           Data.Array.Accelerate.Interpreter

compoundingFactor :: (IsFloating e, Elt e) => Exp e -> Exp e -> Exp e
compoundingFactor r t = (1 + r) ** t

-- | Calculate future value of money
compound :: (IsFloating e, Elt e) => Exp e -> Exp e -> Exp e -> Exp e
compound r t pv = pv * compoundingFactor r t

-- | Calculate present value of money
discount :: (IsFloating e, Elt e) => Exp e -> Exp e -> Exp e -> Exp e
discount r t fv = fv / compoundingFactor r t

presentValue :: (IsFloating e, Elt e) => Exp e -> Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
presentValue r t m = sum $ zipWith (discount r) t m

test = run $ presentValue 0.13 (use $ fromList (Z:.2) [0, 1]) (use $ fromList (Z:.2) [100, 100::Float])
