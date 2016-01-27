module Leonard where

import Data.List
import Data.Bits

-- ^ Number to its digits.
toDigits :: Integral a => a -> [Int]
toDigits x = loop [] x
  where
    loop acc 0 = acc
    loop acc b = let (q, r) = quotRem b 10 in loop (fromIntegral r:acc) q

-- ^ Back to number.
toNumber :: Num a => [Int] -> a
toNumber = foldl' (\a b -> 10 * a + fromIntegral b) 0

-- ^ Bitmask keeping what digits were in the list.
digitsMask :: [Int] -> Int
digitsMask x = foldl' (\a d -> a .|. (2 ^ d)) 0 x

-- ^ Bitmask corresponding to 1-9 pandigital numbers.
allButZeroMask :: Int
allButZeroMask = 2^10 - 2

-- ^ Just . Sqrt for perfect squares, Noting otherwise.
perfectSqrt :: Int -> Maybe Int
perfectSqrt x = let s = truncate $ sqrt $ fromIntegral x in case x == s * s of
    True -> Just s
    False -> Nothing

-- ^ Minimal number with n digits.
minNDigit :: Integral a => Int -> a
minNDigit n = 10^(n-1)

-- ^ Maximal number with n digits.
maxNDigit :: Integral a => Int -> a
maxNDigit n = 10^(n) - 1

-- ^ Number of numbers with n digits.
numNDigit :: Integral a => Int -> a
numNDigit n = maxNDigit n - minNDigit n + 1
