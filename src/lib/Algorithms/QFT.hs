{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Quantum Fourier Transform and related algorithms
Author: Pablo Baeyens
-}

module Algorithms.QFT(qft, estimatePhase)
where

import Control.Monad
import Quipper
import Oracle
import Utils
import Error

import QuipperLib.QFT

-- | QFT algorithm
-- Simplified implementation of the one at QuipperLib.QFT
qft :: [Qubit] -> Circ [Qubit]
qft []     = pure []
qft (x:xs) = do
  xs'  <- qft xs
  xs'' <- rotations x xs' (length xs')
  x'   <- hadamard x
  pure (x' : xs'')
 where
    -- Auxiliary function that calculates the rotations
  rotations :: Qubit -> [Qubit] -> Int -> Circ [Qubit]
  rotations _ []     _ = pure []
  rotations c (q:qs) n = do
    qs' <- rotations c qs n
    q'  <- rGate ((n + 1) - length qs) q `controlled` c
    pure (q' : qs')

reverseQft :: [Qubit] -> Circ [Qubit]
reverseQft = reverse_generic_endo qft


-- | Quantum phase estimation algorithm
estimatePhase
  :: (QData qa) => (Int -> (qa -> Circ qa)) -> qa -> Int -> Circ [Qubit]
estimatePhase operator eigv n = do
  phase <- qinit (replicate t False)
  phase <- map_hadamard phase
  eigv  <- forEach (numbered phase)
                   (\(i, q) qx -> operator (2 ^ i) qx `controlled` q)
                   eigv
  phase <- box "RQFT" reverseQft phase
  qdiscard eigv
  pure phase
  where t = n + 2
