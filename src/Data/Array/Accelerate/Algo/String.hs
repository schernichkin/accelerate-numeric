-- | String manipulation algorithms
module Data.Array.Accelerate.Algo.String
  ( digitToInt
  , isDigit
  , padLeft
  , padRight
  , truncLeft
  , truncRight
  ) where

import Data.Array.Accelerate
import Prelude hiding ((++), all, drop, fromIntegral, length, map, take, tail, zipWith)

digitToInt :: Exp Char -> Exp Int
digitToInt c =
  (dec <=* 9) ? (dec,
  (hexl <=* 5) ? (hexl + 10,
  (hexu <=* 5) ? (hexu + 10, 0)))
  where
    dec =  ord c - ord (constant '0')
    hexl = ord c - ord (constant 'a')
    hexu = ord c - ord (constant 'A')

isDigit :: Exp Char -> Exp Bool
isDigit c = o >=* ord (constant '0') &&* o <=* ord (constant '9')
  where o = ord c

padLeft :: (Elt e) => Exp Int -> Exp e -> Acc (Vector e) -> Acc (Vector e)
padLeft l e v = (p <=* 0) ?| (v, (fill (index1 p) e) ++ v)
  where p = l - length v

padRight :: (Elt e) => Exp Int -> Exp e -> Acc (Vector e) -> Acc (Vector e)
padRight l e v = (p <=* 0) ?| (v, v ++ (fill (index1 p) e))
  where p = l - length v

truncLeft :: (Elt e) => Exp Int -> Acc (Vector e) -> Acc (Vector e)
truncLeft l v = (p >=* 0) ?| (v, drop (-p) v)
  where p = l - length v

truncRight :: (Elt e) => Exp Int -> Acc (Vector e) -> Acc (Vector e)
truncRight l v = take l v
