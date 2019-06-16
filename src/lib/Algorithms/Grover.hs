{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Grover algorithm
Author: Pablo Baeyens
-}

module Algorithms.Grover(
  groverMain
  , diffusion
  , groverOperator
  , groverManualCheck
  , getMatch
  , phaseShift
  , showWord
  ) where

import Oracle
import Options
import Quipper
import QuipperLib.Simulation
import Control.Monad
import Data.Maybe
import System.IO
import Error
import Print
import Utils
import System.Random

-- | Perform manual counting
manualCount :: TruthTable -> Int
manualCount = length . filter snd

-- | Phase shift on n-qubits
-- This operator shifts the sign of the phase of
-- the zero state and leaves other states unchanged.
phaseShift :: [Qubit] -> Circ [Qubit]
phaseShift []     = impossible "phaseShift"
phaseShift (x:xs) = do
  x:xs <- mapUnary qnot (x : xs)
  x    <- gate_Z x `controlled` xs
  xs'  <- mapUnary qnot (x : xs)
  pure xs'


-- | Diffusion operator
-- This operator does the inversion around the mean
diffusion :: [Qubit] -> Circ [Qubit]
diffusion = map_hadamard >=> phaseShift >=> map_hadamard


-- | Grover's algorithm main loop
groverOperator :: Oracle [Qubit] -> ([Qubit], Qubit) -> Circ ([Qubit], Qubit)
groverOperator oracle (xs, y) = do
  (xs, y) <- circuit oracle (xs, y)
  xs      <- diffusion xs
  pure (xs, y)


-- | Grover's algorithm
grover :: Int -> Oracle [Qubit] -> Circ [Bit]
grover m oracle = do
  (x, y) <- qinit (qc_false (shape oracle), True)
  (x, y) <- map_hadamard (x, y)
  (x, y) <- n `timesM` box "Grover's operator" (groverOperator oracle) $ (x, y)
  qdiscard y
  measure x
 where
  n :: Int
  n = round $ (pi / 4) * sqrt (inputSize oracle / fromIntegral m)


-- | Pretty print word
showWord :: [Bool] -> String
showWord = map (\b -> if b then '1' else '0')

-- | Gets match found by Grover's algorithm with given accuracy
getMatch :: Int -> Oracle [Qubit] -> IO [Bool]
getMatch m oracle | m - 1 > n `div` 2 = (take n . randoms) <$> newStdGen
                  | otherwise = run_generic_io (0 :: Double) (grover m oracle)
  where n = inputSize oracle

-- | Manually checks Grover's algorithm to see if it was a match
groverManualCheck :: TruthTable -> [Bool] -> Bool
groverManualCheck tt word =
  fromMaybe (impossible "groverManualCheck") (lookup word tt)

-- | Main Grover's algorithm IO action
groverMain :: Options -> Maybe FilePath -> IO ()
groverMain opts = withFilePath
  ( \(tt, oracle) -> do

    let m = manualCount tt
    printLog opts Verbose $ "The number of solutions is " ++ show m
    when (m == 0) $ reportErr "Oracle must be non-zero"

    -- Print circuit to PDF
    logPrint "Grover's circuit diagram" (outFile opts)
    captureTo (outFile opts) $ print_generic defaultStyle (grover m oracle)

    -- Run algorithm
    let times = 50
    printLog opts Verbose
      $  "Running Grover's algorithm "
      ++ show times
      ++ " times to improve correctness."
    (attempts, word) <- retry (groverManualCheck tt) (getMatch m oracle)
    putStrLn
      $  "Found string: "
      ++ showWord word
      ++ " after "
      ++ show attempts
      ++ " attempts."

    -- Manual check
    printLog
      opts
      Check
      (  "'"
      ++ showWord word
      ++ "' is"
      ++ (if groverManualCheck tt word then "" else " NOT")
      ++ " a valid match."
      )
  )
