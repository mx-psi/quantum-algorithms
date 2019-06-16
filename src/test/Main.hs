import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Either
import Data.Maybe
import Options
import Utils
import Oracle
import Floating
import Quipper
import QuipperLib.Simulation
import System.Random
import Data.Map.Strict
import ProbTest
import TestOracles
import QuipperLib.Arith
import Algorithms.Deutsch
import Algorithms.Grover
import Algorithms.Shor
import Algorithms.Count

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests, probTests]

-- ** Properties tests **

-- | Property testing
qcProps = testGroup
  "Properties"
  [ QC.testProperty "Most common on a singleton list"
    $ \x -> mostCommon [x :: Int] == x
  , QC.testProperty "toContinuedFrac . fromContinuedFrac == id"
    $ \x -> (x > 0) QC.==> fromContinuedFrac (toContinuedFrac x) == x
  , QC.testProperty "The last convergent of a number is the number"
    $ \x -> (x > 0) QC.==> last (convergents x) == x
  , QC.testProperty "toTruthTable . toParamTable == id"
    $ \x -> toTruthTable (toParamTable x) == x
  , QC.testProperty "getPower gets a non-trivial divisor" $ \x n ->
    (x > 1 && n > 1)
      QC.==> x
      ^      n
      >      10
      ^      15
      ||     ( x ^ n `mod` fromBaseFactor
               (getPower $ (x :: Integer) ^ (n :: Integer))
             )
      ==     0
  , QC.testProperty "Modular exponentiation is well-defined"
    $ \x a m -> (m > 1 && a > 1) QC.==> binaryExp x a m == (x ^ a `mod` m)
  , QC.testProperty "Quantum integers test" $ \x ->
    (x > 0)
      QC.==> integer_of_intm_unsigned
               ( intm_with_length
                 (Just (1 + ceiling (logBase (2 :: Double) (fromIntegral x))))
                 x
               )
      ==     x
  ]

-- ** Unit testing **

-- | Unit tests
unitTests = testGroup
  "Unit Tests"
  [ miscTests
  , oracleReadingTests
  , deutschTests
  , groverUnitTests
   --, shorUnitTests
  ]

-- | Miscellaneous unit tests
miscTests = testGroup
  "Miscellaneous tests"
  [testCase "Most common comparison" $ mostCommon [1, 1, 2, 3] @?= 1]

-- | Unit tests related to oracle parsing
oracleReadingTests :: TestTree
oracleReadingTests = testGroup
  "Oracle tests"
  [ testCase "decode empty CSV" $ decode "" @=? Right []
  , testCase "Malformed row"
  $  isLeft (decode "a,")
  @? "Malformed table was accepted"
  , testCase "Valid CSV" $ decode "1,0\n0,1" @=? Right [("1", "0"), ("0", "1")]
  , testCase "Valid ParamTable" $ readCSV [("1", "0"), ("0", "1")] @=? Right
    [([PFalse], PTrue), ([PTrue], PFalse)]
  , testCase "Empty ParamTable"
  $  isLeft (readCSV [])
  @? "Malformed table was accepted"
  , testCase "Partial ParamTable"
  $  isLeft (readCSV [("10", "0")])
  @? "Malformed table was accepted"
  , testCase "Empty input ParamTable"
  $  isLeft (readCSV [("", "0")])
  @? "Malformed table was accepted"
  , testCase "Duplicate input ParamTable"
  $  isLeft (readCSV [("1", "0"), ("1", "0")])
  @? "Malformed table was accepted"
  , testCase "Wrong size ParamTable"
  $  isLeft (readCSV [("1", "0"), ("10", "0")])
  @? "Malformed table was accepted"
  , testCase "Wrong output ParamTable"
  $  isLeft (readCSV [("1", "0"), ("0", "10")])
  @? "Malformed table was accepted"
  ]

-- | Tests for Deutsch-Jozsa algorithm
deutschTests :: TestTree
deutschTests = testGroup
  "Deutsch-Jozsa algorithm tests"
  [ testCase "Classical get type Balanced"
  $   classicalGetType balancedOracleTable
  @=? Just Balanced
  , testCase "Classical get type Constant"
  $   classicalGetType constantOracleTable
  @=? Just Constant
  , testCase "Classical get type Other"
  $   classicalGetType otherOracleTable
  @=? Nothing
  , testCase
    "Deutsch-Jozsa on balanced oracle"
    ( do
      let tipo1 = fromJust (classicalGetType balancedOracleTable)
      tipo2 <- getType defaultOptions balancedOracle
      tipo1 == tipo2 @? "Mismatch on types"
    )
  , testCase
    "Deutsch-Jozsa on constant oracle"
    ( do
      let tipo1 = fromJust (classicalGetType constantOracleTable)
      tipo2 <- getType defaultOptions constantOracle
      tipo1 == tipo2 @? "Mismatch on types"
    )
  ]

-- | Grover's algorithm tests
groverUnitTests :: TestTree
groverUnitTests = testGroup
  "Grover's algorithm tests"
  [ testCaseSteps "Phase shift on one qubit" $ \step -> do
    g <- newStdGen
    step "|0> → -|0>"
    sim_amps g phaseShift (basisOne !! 0) @?= fromList [([False], -1)]
    step "|1> → |1>"
    sim_amps g phaseShift (basisOne !! 1) @=? basisOne !! 1
  , testCaseSteps "Phase shift on two qubits" $ \step -> do
    g <- newStdGen
    step "|00> → -|00>"
    sim_amps g phaseShift (basisTwo !! 0) @?= fromList [([False, False], -1)]
    step "|01> → |01>"
    sim_amps g phaseShift (basisTwo !! 1) @=? basisTwo !! 1
    step "|10> → |10>"
    sim_amps g phaseShift (basisTwo !! 2) @=? basisTwo !! 2
    step "|11> → |11>"
    sim_amps g phaseShift (basisTwo !! 3) @=? basisTwo !! 3
  ]

shorUnitTests :: TestTree
shorUnitTests = testGroup
  "Shor's algorithm tests"
  [ testCase "BitsToFloating" $ bitsToFloating [False, True] @=? 0.25
  , testCase "Continued fraction example"
  $   toContinuedFrac (31 / 13)
  @?= [2, 2, 1, 1, 2]
  , testCase "getPower example" $ getPower (3 ^ 5) @?= BaseFactor 3
  , testCase "getPower failure" $ getPower 6 @=? Other
  ]

-- | Probabilistic tests
-- These tests may frequently fail, specially on edge cases
probTests :: TestTree
probTests = testGroup "Probabilistic tests (may fail)" [groverProbTests]

-- | Grover's probabilistic tests
groverProbTests :: TestTree
groverProbTests = testGroup
  "Grover's algorithm tests"
  [ testCase "000 oracle" $ probTest
    (groverManualCheck tripleZeroTable <$> getMatch 1 tripleZeroOracle)
  , testCase "Ends with '11' oracle" $ probTest
    (groverManualCheck endsWith11Table <$> getMatch 2 endsWith11Oracle)
  , testCase "Balanced oracle (~50% chance of failure)" $ withConfidence
    0.40
    10
    (groverManualCheck balancedOracleTable <$> getMatch 2 balancedOracle)
  ]
