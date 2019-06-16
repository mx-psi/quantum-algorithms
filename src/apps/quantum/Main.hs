{-
Title: Main module
Description: Module that handles the command line interface for the 'quantum' executable
Author: Pablo Baeyens
-}
module Main where

import Options.Applicative
import Options.Applicative.Help.Pretty
import Data.Semigroup ((<>))

import Options
import Error

-- Algorithms
import Algorithms.Deutsch
import Algorithms.Grover
import Algorithms.Count
import Algorithms.Shor


-- | The command options. Distinguishes between oracle-based subcommands and
-- Shor's algorithm subcommands
data Command =
  OracleOptions { -- ^ Options for oracle-based program
    __filename :: Maybe FilePath, -- ^ The input filename
    __options :: Options, -- ^ The options
    __subProgram :: String -- ^ The subprogram command
  }
  | ShorOptions {
      __n :: Integer, -- ^ The integer to factorize
      __options :: Options -- ^ The options
      }


-- | Parsers for various flags
getVerbose, getCheck, getDebug :: Parser Bool
getVerbose = switch (long "verbose" <> short 'v' <> help "Be more verbose")
getCheck = not <$> switch (long "no-check" <> help "Disable output checking")
getDebug = switch
  (  long "debug"
  <> short 'd'
  <> help "Show useful debug information"
  <> internal
  )


-- | Parser for output filepath
getOutFile :: Parser FilePath
getOutFile = strOption
  ( long "output" <> short 'o' <> metavar "OUTPUT" <> value "out.pdf" <> help
    "Write circuit to OUTPUT (defaults to 'out.pdf')"
  )


-- | Parser for options
getOptions :: Parser Options
getOptions = Options <$> getVerbose <*> getOutFile <*> getCheck <*> getDebug


-- | Get input file
getInputFile :: Parser (Maybe FilePath)
getInputFile = optional $ argument str (metavar "FILE" <> help "Oracle file")

-- | Parser builder for oracle-based commands
oracleOptions :: String -> Parser Command
oracleOptions name =
  OracleOptions <$> getInputFile <*> getOptions <*> pure name <**> helper

-- | Parser for Shor's algorithm
shorOptions :: Parser Command
shorOptions =
  ShorOptions
    <$>  argument auto (metavar "N" <> help "Integer to factor")
    <*>  getOptions
    <**> helper

-- | General usage information for oracle-based programs
oracleInfo :: Doc
oracleInfo =
  text "Read the truth table of a predicate"
    </>  text "formatted as an unquoted csv file."
    </>  text "When FILE is missing it reads from standard input."
    <>   hardline
    <$$> text "Example: XOR should be given as:"
    <$$> string "\n  00,0\n  01,1\n  10,1\n  11,0\n"

-- | Information about Shor's algorithm
shorInfo :: Doc
shorInfo =
  text "Describe the process required to factorize N."
    </> text "Assess the amount of resources needed and print needed circuits."
    </> text "Warning: if N is too big the program will not work."

-- | Main parser
parser :: Parser Command
parser = subparser
  (  oracleCommand "deutsch" "Deutsch-Jozsa's algorithm"  simulatable
  <> oracleCommand "grover"  "Grover's algorithm"         simulatable
  <> oracleCommand "count"   "Quantum counting algorithm" printable
  <> command
       "shor"
       ( info
         shorOptions
         ( fullDesc <> progDesc "Shor's algorithm" <> footerDoc
           (Just shorInfo)
         )
       )
  )
 where
  oracleCommand name brief extra = command
    name
    ( info
      (oracleOptions name)
      (fullDesc <> progDesc brief <> footerDoc (Just (oracleInfo </> extra)))
    )
  simulatable =
    text "Print circuit representation to a pdf and simulate its execution."
  printable =
    text "Print circuit representation and assess required resources."

-- | Run the appropiate subprogram
runSubProgram (OracleOptions filename options subprog)
  | subprog == "deutsch" = deutschMain options filename
  | subprog == "grover"  = groverMain options filename
  | subprog == "count"   = countMain options filename
  | otherwise            = impossible "runSubProgram"
runSubProgram (ShorOptions n options) = shorMain options n


-- | Main program
-- Parse options and run subprogram
main = do
  command <- customExecParser
    (prefs (showHelpOnError <> showHelpOnEmpty <> noBacktrack))
    ( info (parser <**> helper)
           (fullDesc <> header programHeader <> footer programFooter)
    )
  runSubProgram command
 where
  programHeader = "quantum - simulation and depiction of quantum algorithms"
  programFooter =
    "See the attached 'README.md' file for further usage instructions."
