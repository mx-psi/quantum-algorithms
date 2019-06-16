{-
Title: Handcrafted Oracles
Description: Hand-made oracles for testing and representation
Author: Pablo Baeyens
-}

{-# LANGUAGE MonoLocalBinds #-}
module HandCraftedOracles where

import Oracle (Oracle, buildOracle, shape, circuit)
import Quipper
import QuipperLib.Simulation
import System.Environment

build_circuit
xor x y = (x && not y) || (not x && y)

build_circuit
xorList :: [Bool] -> Bool
xorList = foldl xor False

-- | Parity oracle
parity
  :: Int            -- ^ Number of inputs
  -> Oracle [Qubit] -- ^ The oracle
parity n = buildOracle (replicate n qubit) (unpack template_xorList)


-- | Identity oracle
oracleId = buildOracle qubit pure

build_circuit
example (x, y, z, w) = (z && w) `xor` (x || y)

-- | A more complicated oracle
oracleExample =
  buildOracle (qubit, qubit, qubit, qubit) (unpack template_example)
