{-
Title: Floating numbers utilities
Description: Module that handles different representations of floating numbers
Author: Pablo Baeyens
-}
module Floating(
  toContinuedFrac
  ,fromContinuedFrac
  ,bitsToFloating
  ,convergents)
where

-- | Transform a rational number into its continued fraction expression
toContinuedFrac :: Rational -> [Integer]
toContinuedFrac 0 = []
toContinuedFrac x | x == fromIntegral n = [n]
                  | otherwise           = n : toContinuedFrac (1 / f)
 where
  n = floor x
  f = x - fromIntegral n

-- | Transform a continued fraction expression into a rational number
fromContinuedFrac :: [Integer] -> Rational
fromContinuedFrac [x   ] = fromIntegral x
fromContinuedFrac (x:xs) = fromIntegral x + 1 / fromContinuedFrac xs
fromContinuedFrac []     = 0

convergents :: Rational -> [Rational]
convergents x = map (\i -> fromContinuedFrac $ take i frac) [1 .. n]
 where
  frac = toContinuedFrac x
  n    = length frac

-- | Transform a list of Bools into a floating number
bitsToFloating :: Floating a => [Bool] -> a
bitsToFloating xs = sum $ map (\(i, x) -> 2 ^^ (-i) * toBit x) (zip [1 ..] xs)
  where toBit b = if b then 1 else 0
