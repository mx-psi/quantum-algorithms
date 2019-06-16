{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Quantum Fourier Transform and related algorithms
Author: Pablo Baeyens
-}

module Algorithms.Shor(
  shorMain
  , factor
  , getPower
  , binaryExp
  , quantumOp
  , Result(..)
  , fromBaseFactor
  )
where

import System.Random
import Data.Maybe
import Data.Either
import Control.Monad

import Math.NumberTheory.Primes.Testing (isPrime)
import Quipper
import QuipperLib.Simulation
import QuipperLib.Arith

import Algorithms.QFT
import Options
import Print
import Utils
import Floating
import Data.Ratio
import Error

-- | A datatype indicating why Shor's algorithm has failed or whether
-- quantum factorization would be needed
data Result = One
             | Prime
             | BadOrder Integer Integer
             | Other
             | Quantum Integer
             | BaseFactor Integer
             | GCDFactor Integer
             deriving (Eq, Ord, Show)

-- | Checks if is base factor
isBaseFactor :: Result -> Bool
isBaseFactor (BaseFactor _) = True
isBaseFactor _              = False

-- | From base factor
fromBaseFactor :: Result -> Integer
fromBaseFactor (BaseFactor x) = x
fromBaseFactor _              = impossible "fromBaseFactor"


-- | Find a non-trivial factor of a number,
-- or return the number itself if it doesn't have trivial divisors
factor :: Integer -> IO Result
factor n
  | n < 0 = pure (BaseFactor (-1))
  | n == 1 = pure One
  | isPrime n = pure Prime
  | n `mod` 2 == 0 = pure (BaseFactor 2)
  | isBaseFactor root = pure root
  | otherwise = do
    x <- randomRIO (1, n - 1)
    if gcd x n > 1 then pure (GCDFactor (gcd x n)) else pure (Quantum x)
  where root = getPower n


-- | Gets power of number
getPower :: Integer -> Result
getPower n = if null exponents
  then Other
  else BaseFactor $ round (base (head exponents))
 where
  n' :: Double
  -- Number to factor, expressed as a double
  n' = fromIntegral n

  a :: Double
  -- We check every possible root up to `a`
  a = fromIntegral $ ceiling $ logBase 2 n'

  base :: Double -> Double
  -- The base for a given exponent
  base x = fromIntegral $ round (n' ** (1 / x))

  exponents :: [Double]
  -- List of possible exponents, from biggest to smallest
  exponents = filter (\x -> n == round ((base x) ** x)) [a, a - 1 .. 2]


-- | Binary exponentiation
-- `binaryExp x a m` efficiently calculates x^a `mod` m
binaryExp :: Integer -> Integer -> Integer -> Integer
binaryExp x 0 m = 1
binaryExp x a m =
  (binaryExp (x ^ 2 `mod` m) q m `mod` m * (x `mod` m) ^ r) `mod` m
  where (q, r) = a `divMod` 2


toIntM :: Integer -> IntM
toIntM x = intm_with_length
  (Just (1 + ceiling (logBase (2 :: Double) (fromIntegral x))))
  x

-- | Shor's operator for order-finding
quantumOp :: Integer -> Integer -> Int -> QDInt -> Circ QDInt
quantumOp x n j y = do
  q_n           <- qinit (toIntM n)
  (y, z)        <- q_mult_param a y -- x^n mod n * y
  (z, q_n, res) <- q_mod_unsigned z q_n
  pure res -- x^n*y mod n
 where
  a :: IntM
  a = intm_with_length
    (Just (1 + ceiling (logBase (2 :: Double) (fromIntegral n))))
    (binaryExp x (fromIntegral j) n)


-- | `factorFromOrder x n` finds a factor of `n`
-- given the order `r` of `x` in Z_n*.
-- If `x` is sampled uniformly at random and `r`
-- The algorithm can not be feasibly simulated; it is here for
-- illustrative purposes only
factorFromOrder :: Integer -> Integer -> IO (Maybe Integer)
factorFromOrder x n = do
  phi <- bitsToFloating <$> run_generic_io (0 :: Double) getOrder x n
  let r = phaseToOrder phi x n
  pure
    ( if r `mod` 2 /= 0
      then Nothing
      else
        ( do
          let a = binaryExp x (r `div` 2) n
          if a /= n - 1 then pure (gcd (a - 1) n) else Nothing
        )
    )


-- | Get the order (in the form of a phase) from an integer on a group
getOrder :: Integer -> Integer -> Circ [Qubit]
getOrder x n = do
  eigv <- qinit
    ( intm_with_length
      (Just (1 + ceiling (logBase (2 :: Double) (fromIntegral n))))
      1
    )
  estimatePhase
    (\i -> named_gate $ "O(" ++ show x ++ "," ++ show n ++ "," ++ show i ++ ")")
    eigv
    (1 + 2 * ceiling (logBase 2 (fromIntegral n)))


-- | Get the phase of an integer from the phase of its associated operator
phaseToOrder :: Double -> Integer -> Integer -> Integer
phaseToOrder phi x n | phi == 0    = 1
                     | -- Bad order
                       null orders = 1
                     | -- Bad order
                       otherwise   = head orders
 where
  phi' = toRational phi
  orders =
    filter (\r -> binaryExp x r n == 1) (map denominator (convergents phi'))



-- | Main function for Shor's algorithm
shorMain :: Options -> Integer -> IO ()
shorMain opts n = do
  when (n > 10 ^ 15)
       (reportErr "Integer can't be bigger than floating-point precision")
  result <- factor n

  putStrLn "Running classical part of Shor's algorithm."

  case result of
    One   -> putStrLn $ (show n) ++ " is a base case of the algorithm."
    Prime -> putStrLn $ (show n) ++ " is a prime number."
    BaseFactor x ->
      putStrLn
        $  "The factor "
        ++ show x
        ++ " can be found classically in polynomial time."
    GCDFactor x ->
      putStrLn $ "The factor " ++ show x ++ " was found by random sampling."
    Quantum x -> do
      putStrLn
        $  show x
        ++ " is a unit of the group of integers modulo "
        ++ show n
        ++ "."
      putStrLn $ "Quantum computation would be needed to factor this integer."

      -- Print circuit to PDF
      let shape =
            (qdint_shape (1 + ceiling (logBase (2 :: Double) (fromIntegral n))))
      when (x > 16)
           (warn "Units bigger than 16 may be too big for a proper display.")
      logPrint "Shor's oracle" ("oracle_" ++ outFile opts)
      captureTo ("oracle_" ++ outFile opts)
                (print_generic defaultStyle (quantumOp x n 1) shape)
      waitForKey
      putStrLn "Gate count for oracle:"
      print_generic GateCount (quantumOp x n 1) shape

      logPrint "Shor's circuit diagram (Quantum phase estimation)"
               (outFile opts)
      captureTo (outFile opts) $ print_generic defaultStyle (getOrder x n)
      waitForKey
      putStrLn "Gate count for complete circuit (counting oracles as gates)"
      print_generic GateCount (getOrder x n)
