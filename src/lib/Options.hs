{-
Title: Options module
Description: Module that handles executable options
Author: Pablo Baeyens
-}
module Options(
  Options(..)
  , printLog
  , LogType(..)
  , defaultOptions
  )
where

import System.IO
import Control.Monad

-- | Options for a given command
data Options = Options {
  verbose :: Bool, -- ^ verbosity flag
  outFile :: FilePath, -- ^ where to write the circuit
  check :: Bool, -- ^ check input correctness
  debug :: Bool -- ^ Show debug information
  }
  deriving Show


-- | The default set of options
defaultOptions :: Options
defaultOptions =
  Options {verbose = False, outFile = "out.pdf", check = False, debug = False}


-- | Type of logging
data LogType = Debug | Verbose | Check

-- | Logging option. Prints a logging message of a certain type
printLog :: Options -> LogType -> String -> IO ()
printLog opts Debug   = when (debug opts) . putStrLn . ("[debug]   " ++)
printLog opts Verbose = when (verbose opts) . putStrLn . ("[verbose] " ++)
printLog opts Check   = when (check opts) . putStrLn . ("[check] " ++)
