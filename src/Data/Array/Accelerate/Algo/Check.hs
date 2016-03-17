-- | Various checksums and validation algorithms
module Data.Array.Accelerate.Algo.Check
  ( validateRuInn
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Algo.LA
import Data.Array.Accelerate.Algo.String
import Prelude hiding ((++), all, drop, fromIntegral, length, map, take, tail, zipWith)

validateRuInn :: Acc (Vector Char) -> Acc (Scalar Bool)
validateRuInn v = let l = length v in
  (l ==* 10) ?| (unit $ validate10 v,
  (l ==* 12) ?| (unit $ validate12 v,
  (l ==* 11) ?| (unit $ v ! index1 0 ==* constant 'F' &&* validate10 (tail v),
    unit $ constant False)))
  where
    vs :: Acc (Vector Int)
    vs = use $ fromList (Z:.11) [3, 7, 2, 4, 10, 3, 5, 9, 4, 6, 8]

    allDigitst = the . all isDigit
    checkSum xs ys = let c = the (xs `dotProduct` ys) `mod` 11
                     in (c ==* 10 ? (0, c))

    validate10 inn = allDigitst inn &&* (ds ! index1 9 ==* n1)
      where
        ds = map digitToInt inn
        n1 = checkSum ds $ drop 2 vs

    validate12 inn = allDigitst inn &&* (ds ! index1 11 ==* n1)
                                    &&* (ds ! index1 10 ==* n2)
      where
        ds = map digitToInt inn
        n1 = checkSum ds vs
        n2 = checkSum ds $ tail vs
