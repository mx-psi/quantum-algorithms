{-
Title: Diagrams main
Description: Module that handles the printing of circuit diagrams
Author: Pablo Baeyens
-}
module Main where

import Options.Applicative
import Data.Foldable

import System.Directory

import Quipper
import Quipper.Printing

import Classical
import Algorithms.QFT
import Algorithms.Grover
import Print
import Text.Printf

import Control.Monad
import Data.Semigroup((<>))

import Oracle
import System.IO
import Error

-- | The possible modes of execution
data Mode =
  GenAll -- ^ Generate every diagram from the book
  | Circuit FilePath FilePath -- ^ Generate just this circuit
  deriving Show

-- | Parser for generating all examples
parseGenAll :: Parser Mode
parseGenAll = flag'
  GenAll
  (long "gen-all" <> help "Generate all examples from the document")

-- | Parser for circuits
parseCircuit :: Parser Mode
parseCircuit =
  Circuit
    <$> strOption
          ( short 'c' <> long "circuit" <> metavar "FILE" <> help
            "Circuit file for which to generate a diagram"
          )
    <*> strOption
          (  short 'o'
          <> long "output"
          <> metavar "OUTPUT"
          <> value "out.pdf"
          <> help "Write circuit to OUTPUT (defaults to 'out.pdf')"
          )


-- | Program options parser
-- Either generate all the circuits or parse a given circuit
parser :: Parser Mode
parser = parseGenAll <|> parseCircuit

-- | Quantum phase estimation diagram
qpe = qinit [False, False]
  >>= \eig -> estimatePhase (\i -> box ("U^" ++ show i) pure) eig 2


-- | List of examples to be printed
examples :: [(String, IO ())]
examples =
  [ ("nand"     , print_generic defaultStyle nand (qubit, qubit))
  , ("toffoli"  , print_generic defaultStyle toffoli (qubit, qubit, qubit))
  , ("random"   , print_generic defaultStyle random)
  , ("fanout"   , print_generic defaultStyle fanout qubit)
  , ("not"      , print_generic defaultStyle notCirc qubit)
  , ("and"      , print_generic defaultStyle andCirc (qubit, qubit))
  , ("wire"     , print_generic defaultStyle wire qubit)
  , ("xnor"     , print_generic defaultStyle xnor (qubit, qubit))
  , ("qpe"      , print_generic defaultStyle qpe)
  , ("qft"      , print_generic defaultStyle qft (replicate 3 qubit))
  , ("diffusion", print_generic defaultStyle diffusion (replicate 3 qubit))
  ]


-- | Print the a diagram given its name and action that generates it
-- This is an uncurried function.
printDiagram :: (FilePath, IO ()) -> IO ()
printDiagram (name, m) = do
  let outputPath = defaultOutputPath ++ name ++ ".pdf"
  logPrint  name       outputPath
  captureTo outputPath m


-- | Run a given mode
runMode :: Mode -> IO ()

-- Generate all examples from the document
runMode GenAll = do
  createDirectoryIfMissing True     defaultOutputPath
  for_                     examples printDiagram

-- Print a given circuit
runMode (Circuit name out) = do
  handle <- fromFile name
  raw    <- hGetContents handle
  let oracle = (decode >=> readCSV >=> paramTableToOracle) raw
  case oracle of
    Left  err -> reportErr err
    Right or  -> do
      logPrint name out
      captureTo out $ print_generic defaultStyle (circuit or) (shape or, qubit)



-- | Print each example to its file
main :: IO ()
main = do
  mode <- customExecParser
    (prefs (showHelpOnError <> showHelpOnEmpty))
    ( info (parser <**> helper)
           (fullDesc <> header programHeader <> footer programFooter)
    )
  runMode mode
 where
  programHeader = "diagrams - Generate graphical representations of circuits."
  programFooter
    = "Generate diagrams from the document or print a predicate given as a truth table.\n"
