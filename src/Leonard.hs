module Leonard where

import Data.List
import Data.Bits

toDigits :: Integral a => a -> [Int]
toDigits x = loop [] x
  where
    loop acc 0 = acc
    loop acc b = let (q, r) = quotRem b 10 in loop (fromIntegral r:acc) q

toNumber :: Num a => [Int] -> a
toNumber = foldl' (\a b -> 10 * a + fromIntegral b) 0

digitsMask :: [Int] -> Int
digitsMask x = foldl' (\a d -> a .|. (2 ^ d)) 0 x

allButZeroMask :: Int
allButZeroMask = 2^10 - 2
