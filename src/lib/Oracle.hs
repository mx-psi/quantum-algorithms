{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Oracle utilities
Description: Module that handles the reading of oracles
Author: Pablo Baeyens
-}

module Oracle(
  Oracle
  ,buildOracle
  ,withFilePath
  ,shape
  ,circuit
  ,inputSize
  ,TruthTable
  ,toParamTable
  ,toTruthTable
  ,decode
  ,readCSV
  ,truthTableToOracle
  ,fromFile
  ,paramTableToOracle
  )
where

import Quipper
import QuipperLib.ClassicalOptim
import qualified Data.Set as Set
import Text.Printf
import Control.Monad
import Data.Foldable
import Error

import System.IO
import System.Directory


-- *** Basic types ***

-- | An oracle for a reversible function
data Oracle qa = Oracle {
  shape   :: qa, -- ^ The shape of the input
  circuit :: (qa,Qubit) -> Circ (qa,Qubit) -- ^ The circuit oracle
  }

-- | The type of truth table of an oracle
type CSV = [(String,String)]
type ParamTable = [([BoolParam], BoolParam)]
type TruthTable = [([Bool], Bool)]
type Error = String

-- TODO: Reemplazar para tests
-- data ParsingError = MalformedRow String
--                   | IllegalChar Char
--                   | EmptyTable
--                   | PartialPredicate
--                   | DuplicateInput String
--                   | EmptyInput
--                   | NonSingleDigit String


-- | Convert a parameter table into a truth table
toTruthTable :: ParamTable -> TruthTable
toTruthTable = map (\(a, b) -> (map fromParam a, fromParam b))
  where fromParam = (== PTrue)

-- | Convert a truth table into a parameter table
toParamTable :: TruthTable -> ParamTable
toParamTable = map (\(a, b) -> (map toParam a, toParam b))
  where toParam x = if x then PTrue else PFalse


-- | Build an oracle from a non-reversible circuit
buildOracle
  :: QData qa
  => qa -- ^ Shape of input
  -> (qa -> Circ Qubit) -- ^ the circuit
  -> Oracle qa -- ^ the resulting oracle
buildOracle qa circ =
  Oracle qa (without_comments . classical_to_reversible_optim circ)


-- | Get the input size of an oracle
inputSize :: Num a => Oracle [Qubit] -> a
inputSize = fromIntegral . length . shape



-- *** Boolean parameters ***

-- | Read a list of parameters
readBoolParam :: String -> Either Error [BoolParam]
readBoolParam = mapM readParam
 where
  readParam :: Char -> Either Error BoolParam
  readParam '0' = Right PFalse
  readParam '1' = Right PTrue
  readParam x   = Left $ printf "illegal character: '%c'" x


-- *** Oracle reading ***

-- | Build Circuit from ParamTable using `build_circuit` directive
build_circuit
booleanFromTruth :: ParamTable -> [Bool] -> Bool
booleanFromTruth []          z = False
booleanFromTruth ((x, y):xs) z = if toBool x == z
  then newBool y
  else booleanFromTruth xs z
 where
  toBool []     = []
  toBool (x:xs) = newBool x : toBool xs


-- | Build a circuit from a truth table
-- Actual implementation on Bools is done on `booleanFromTruth`
-- Behaviour is undefined when truth table is ill-formed
fromParamTable :: ParamTable -> [Qubit] -> Circ Qubit
fromParamTable = unpack template_booleanFromTruth


-- | Decode unquoted csv-file
decode :: String -> Either Error CSV
decode = mapM decodeRow . filter (not . null) . lines
 where
  decodeRow row = do
    let (src, dst) = span (/= ',') row
    when (length dst < 2) (Left $ printf "malformed row: '%s'" row)
    pure (src, tail dst)

-- | Read truth table from string table.
-- Assumes non-empty table
readCSV :: CSV -> Either Error ParamTable
readCSV rawTable = do
  when (null rawTable) (Left "empty truth table")

  -- Create truth table and set of inputs
  (inputs, table) <- foldlM buildLine mempty rawTable

  -- Check set of inputs is complete
  when (length inputs /= 2 ^ n) (Left "predicate is partial")
  pure table
 where
  n = length $ fst $ head rawTable
  buildLine (inputs, paramTable) (x, y)
    | null x = Left "empty input"
    | x `elem` inputs = Left $ printf "malformed input: '%s' is duplicated" x
    | length x /= n = Left
    $ printf "malformed input: '%s' has size %d." x (length x)
    | length y /= 1 = Left
    $ printf "malformed output: '%s' is not single digit." y
    | otherwise = do
      x' <- readBoolParam x
      y' <- readBoolParam y
      pure (x `Set.insert` inputs, (x', head y') : paramTable)


-- | Get shape from truth table
-- Assumes non-empty table
getShape :: ParamTable -> Either Error [Qubit]
getShape xs = pure (replicate n qubit) where n = length $ fst $ head xs


-- | Parameter table to oracle
paramTableToOracle :: ParamTable -> Either Error (Oracle [Qubit])
paramTableToOracle table = do
  -- get shape and circuit
  shape <- getShape table
  let circ = fromParamTable table

  -- Build Oracle
  pure (buildOracle shape circ)


-- | Truth table to oracle
truthTableToOracle :: TruthTable -> Either Error (Oracle [Qubit])
truthTableToOracle = paramTableToOracle . toParamTable


-- | Read an oracle from a csv encoded string
decodeOracle :: String -> Either Error (Oracle [Qubit])
decodeOracle csv = do
  -- Decode raw table with elements as strings
  rawTable <- decode csv
  when (null rawTable) (Left "empty truth table")

  -- get truth table and shape
  table <- readCSV rawTable
  paramTableToOracle table


-- **Input/Output actions**

-- | Opens file if it exists, otherwise signals failure
fromFile :: FilePath -> IO Handle
fromFile file = do
  exists <- doesFileExist file
  if exists
    then openFile file ReadMode
    else reportErr $ "Requested file " ++ show file ++ " does not exist."

-- | Creates a main program that
--  - Attempts to read an oracle from stdin or first argument
--  - If sucessful, runs IO action using it
--  - If unsuccessful, exit and output usage information
withFilePath
  :: ((TruthTable, Oracle [Qubit]) -> IO ()) -> Maybe FilePath -> IO ()
withFilePath m fp = do
  s <- maybe getContents (fromFile >=> hGetContents) fp
  either
    reportErr
    m
    (   (,)
    <$> ( do
          paramTable <- decode s >>= readCSV
          pure $ toTruthTable paramTable
        )
    <*> decodeOracle s
    )
