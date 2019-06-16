module ProbTest(
  probTest
  ,withConfidence
  ,testIO) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad

-- | Defines probabilistic test with
-- default confidence and boolean
probTest :: IO Bool -> Assertion
probTest = withConfidence confidence repetitions
 where
  confidence  = 0.51
  repetitions = 100

-- | Defines probabilistic test for
-- a given level of confidence and number of repetitions
withConfidence :: Double -> Int -> IO Bool -> Assertion
withConfidence confidence times m = do
  results <- replicateM times m
  let correct = length $ filter id results
  let perc    = fromIntegral correct / fromIntegral times
  (perc >= confidence)
    @? "Expected "
    ++ show (100 * confidence)
    ++ "% correctness, but got "
    ++ show (100 * perc)
    ++ "%"


-- | Tests a boolean assertion within the IO monad
testIO :: String -> IO Bool -> Assertion
testIO msg = (>>= (@? msg))
