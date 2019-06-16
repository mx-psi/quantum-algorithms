module TestOracles(
  balancedOracleTable
  , balancedOracle
  , constantOracleTable
  , constantOracle
  , otherOracleTable
  , otherOracle
  , tripleZeroTable
  , tripleZeroOracle
  , endsWith11Table
  , endsWith11Oracle
  , basisOne
  , basisTwo) where

import Quipper
import Oracle
import Data.Map.Strict
import Quantum.Synthesis.Ring (Cplx)

-- *** Deutsch-Jozsa oracles *** --

balancedOracleTable =
  [ ([False, False], False)
  , ([False, True] , True)
  , ([True, False] , True)
  , ([True, True]  , False)
  ]

-- | A Balanced oracle
balancedOracle :: Oracle [Qubit]
balancedOracle = oracle
  where Right oracle = truthTableToOracle balancedOracleTable

constantOracleTable =
  [ ([False, False], False)
  , ([False, True] , False)
  , ([True, False] , False)
  , ([True, True]  , False)
  ]

-- | A constant oracle
constantOracle :: Oracle [Qubit]
constantOracle = oracle
  where Right oracle = truthTableToOracle constantOracleTable

otherOracleTable =
  [ ([False, False], True)
  , ([False, True] , False)
  , ([True, False] , False)
  , ([True, True]  , False)
  ]

-- | A non-constant, non-balanced oracle
otherOracle :: Oracle [Qubit]
otherOracle = oracle where Right oracle = truthTableToOracle otherOracleTable

-- *** Grover oracles and maps *** ---

tripleZeroTable =
  [ ([False, False, False], True)
  , ([False, False, True] , False)
  , ([False, True, False] , False)
  , ([False, True, True]  , False)
  , ([True, False, False] , False)
  , ([True, False, True]  , False)
  , ([True, True, False]  , False)
  , ([True, True, True]   , False)
  ]

tripleZeroOracle :: Oracle [Qubit]
tripleZeroOracle = oracle
  where Right oracle = truthTableToOracle tripleZeroTable


endsWith11Table =
  [ ([False, False, False], False)
  , ([False, False, True] , False)
  , ([False, True, False] , False)
  , ([False, True, True]  , True)
  , ([True, False, False] , False)
  , ([True, False, True]  , False)
  , ([True, True, False]  , False)
  , ([True, True, True]   , True)
  ]

endsWith11Oracle :: Oracle [Qubit]
endsWith11Oracle = oracle
  where Right oracle = truthTableToOracle endsWith11Table


-- | Basis for one qubit
basisOne :: [Map [Bool] (Cplx Double)]
basisOne = [fromList [([False], 1)], fromList [([True], 1)]]


-- | Basis for two qubits
basisTwo :: [Map [Bool] (Cplx Double)]
basisTwo =
  [ fromList [([False, False], 1)]
  , fromList [([False, True], 1)]
  , fromList [([True, False], 1)]
  , fromList [([True, True], 1)]
  ]

-- phaseShift :: [Qubit] -> Circ [Qubit]
