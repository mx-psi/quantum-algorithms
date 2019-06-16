{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Quantum counting and quantum existence algorithms
Description: Implementation of functions related to quantum counting and quantum existence
Author: Pablo Baeyens
-}

module Algorithms.Count(
  countMain
  , getNumber
  , manualCount
  , exists
  )
  where

import Quipper
import QuipperLib.Simulation

import Oracle
import Algorithms.Grover
import Algorithms.QFT

import Options
import Utils
import Floating
import Print
import Control.Monad
import Error

-- | Perform manual counting
manualCount :: TruthTable -> Int
manualCount = length . filter snd

-- | Main circuit for counting
-- This is only useful for quantum existence
quantumCount :: Oracle [Qubit] -> Int -> Circ [Qubit]
quantumCount oracle t = do
  eigv <- qinit (qc_false (shape oracle), True)
  eigv <- map_hadamard eigv
  estimatePhase
    ( \i -> (box $ "Grover's operator^" ++ show i)
      (i `timesM` (\x -> global_phase 1 >> groverOperator oracle x))
    )
    eigv
    t

-- | Get the number of solutions of an equation with given precision
-- This algorithm is valid but takes too much time for it to
-- be feasibly exemplified
phaseToNum :: Options -> Oracle [Qubit] -> Int -> IO Int
phaseToNum opts oracle t = do
  bits <- run_generic_io (0 :: Double) (quantumCount oracle t)
  printLog opts Debug ("Bits: " ++ show bits)

  let phi = bitsToFloating bits
  printLog opts Debug ("phi: " ++ show phi)

  pure $ floor (2 ** n * (sin pi * phi) ^^ 2)
 where
  n :: Double
  n = inputSize oracle


-- | Count the number of solutions to an equation
-- This algorithm is valid but takes too much time for it to
-- be feasibly exemplified
getNumber :: Options -> Oracle [Qubit] -> IO Int
getNumber opts oracle = do
  m1 <- phaseToNum opts oracle t
  m2 <- phaseToNum opts oracle t

  printLog opts Debug $ "m1: " ++ show m1 ++ "m2: " ++ show m2

  let t2 = ceiling $ logBase 2 $ min (f m1) (f m2)
  phaseToNum opts oracle t
 where
  n :: Double
  n = inputSize oracle
  t :: Int
  t = ceiling (n / 2) + 6
  f :: Int -> Double
  f x = fromIntegral $ ceiling $ 30 * sqrt
    ((2 ** n * 2 ^^ x + 1) * (2 ** n - 2 ** n * 2 ^^ x + 1))

-- | Quantum existance algorithms
-- This algorithm is valid but takes too much time for it to be simulated
exists :: Options -> Oracle [Qubit] -> IO Bool
exists opts oracle = (> 0) <$> phaseToNum opts oracle t
 where
  n :: Double
  n = inputSize oracle
  t :: Int
  t = ceiling (n / 2) + 2


-- | Main algorithm for quantum counting
countMain :: Options -> Maybe FilePath -> IO ()
countMain opts = withFilePath
  ( \(tt, oracle) -> do

    let n           = inputSize oracle
    let tDiagram    = ceiling (n / 2) + 2 -- Artifficially reduced to permit visualization
    let tStatistics = ceiling (n / 2) + 6

    when
      (n > 5)
      ( warn
        "The input size of the oracle may be to big for printing. Attempting anyway..."
      )

    -- Print circuit to PDF
    logPrint "Quantum Counting Circuit (with reduced precision)" (outFile opts)
    captureTo (outFile opts)
              (print_generic defaultStyle (quantumCount oracle tDiagram))
    waitForKey
    putStrLn "Gate count statistics: "
    print_generic GateCount (quantumCount oracle tStatistics)
  )
