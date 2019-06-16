{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Deutsch-Jozsa algorithm
Author: Pablo Baeyens
-}

module Algorithms.Deutsch(
  deutschMain
  ,deutschJozsa
  ,getType
  ,classicalGetType
  ,OracleType(..)
  ) where

import Oracle (Oracle, shape, circuit, withFilePath, TruthTable, inputSize)
import Options
import Quipper
import QuipperLib.Simulation
import Print
import Data.Foldable
import System.IO
import Data.Maybe
import Control.Monad
import Error

-- | The type of an oracle
data OracleType =
  Balanced     -- ^ A balanced oracle
  | Constant   -- ^ A constant oracle
  deriving (Eq, Show)


-- | Implements the Deutsch and Deutsch-Jozsa algorithm.
--  Given an oracle computes the circuit of Deutsch-Jozsa algorithm
deutschJozsa :: (QShape ba qa ca) => Oracle qa -> Circ ca
deutschJozsa oracle = do
  -- Initialize x (to False) and y (to True)
  (x, y) <- qinit (qc_false (shape oracle), True)

  -- Apply Hadamard gates to each qubit
  (x, y) <- map_hadamard (x, y)

  -- Apply oracle
  (x, y) <- boxedOracle (x, y)

  -- Apply Hadamard gate again to x
  x      <- map_hadamard x

  -- Measure and discard
  z      <- measure x
  qdiscard y
  pure z
  where boxedOracle = box "Oracle" $ circuit oracle


-- | Get type of an oracle using Deutsch-Jozsa algorithm
getType :: Options -> Oracle [Qubit] -> IO OracleType
getType opts oracle = do
  -- The Deutsch-Josza circuit for the given oracle
  let deutschCircuit = deutschJozsa oracle

  -- Simulate Deutsch-Jozsa circuit
  result <- run_generic_io (0 :: Double) deutschCircuit

  -- Print list of measured results
  printLog opts Debug ("Measured result: " ++ show result)

  -- if any of the qubits on the result is True, the oracle is balanced,
  --  otherwise it is constant.
  pure (if or result then Balanced else Constant)


-- Get the type of an oracle by manually checking its truth table
classicalGetType :: TruthTable -> Maybe OracleType
classicalGetType tt | n == 0 || n == length tt = Just Constant
                    | n == length tt `div` 2   = Just Balanced
                    | otherwise                = Nothing
  where n = length $ filter snd tt

-- | Main function
deutschMain :: Options -> Maybe FilePath -> IO ()
deutschMain opts = withFilePath
  ( \(tt, oracle) -> do

    when
      (inputSize oracle > 7)
      (warn "Input size may be too big for the simulator. Attempting anyway...")

    -- Printing to pdf
    logPrint "Deutsch-Jozsa algorithm circuit" (outFile opts)
    captureTo (outFile opts) $ print_generic defaultStyle (deutschJozsa oracle)

    -- Run Deutsch-Jozsa algorithm
    printLog opts Verbose "Getting type of oracle"
    oracleType <- getType opts oracle
    putStrLn $ "Oracle type: " ++ show oracleType

    -- Manual check of oracle
    printLog
      opts
      Check
      ( case classicalGetType tt of
        Nothing
          -> "Oracle violates promise: it is not balanced nor constant. Undefined behaviour."
        Just t -> "The correct type is " ++ show t ++ "."
      )
  )
