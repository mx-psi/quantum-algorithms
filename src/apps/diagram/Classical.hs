{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Classical gates
Description: Module that defines some classical gates
Author: Pablo Baeyens
-}
module Classical(
  toffoli
  ,nand
  ,random
  ,fanout
  ,notCirc
  ,andCirc
  ,wire
  ,xnor) where
import Quipper
import QuipperLib.ClassicalOptim

-- | The Toffoli gate
toffoli :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
toffoli (x, y, z) = do
  z <- qnot z `controlled` (x, y)
  pure (x, y, z)


-- | The (simulated) NAND gate
nand :: (Qubit, Qubit) -> Circ Qubit
nand (qx, qy) = do
  qa           <- qinit True
  (qx, qy, qa) <- toffoli (qx, qy, qa)
  qdiscard (qx, qy)
  pure qa


-- | The (simulated) RANDOM gate
random :: Circ Qubit
random = qinit False >>= hadamard


-- | The (simulated) FANOUT gate
fanout :: Qubit -> Circ (Qubit, Qubit)
fanout x = do
  y         <- qinit True
  z         <- qinit False

  (x, y, z) <- toffoli (x, y, z)

  qterm True y
  pure (x, z)

-- | NOT gate
notCirc :: Qubit -> Circ Qubit
notCirc x = fanout x >>= nand

-- | AND gate
andCirc :: (Qubit, Qubit) -> Circ Qubit
andCirc (x, y) = nand (x, y) >>= notCirc

-- | Simple wire
wire :: QData qa => qa -> Circ qa
wire = pure


build_circuit
booleanXnor (x, y) = (not x || y) && (x || not y)

-- | XNOR gate
xnor :: (Qubit, Qubit) -> Circ Qubit
xnor = unpack template_booleanXnor
