{-
Title: Error Module
Description: Module for error handling and printing
Author: Pablo Baeyens
-}
module Error(
  reportErr
  , warn
  , impossible
  ) where


import System.Exit (exitFailure)
import Text.Printf
import Options.Applicative.Help.Pretty
import Data.Semigroup ((<>))

-- | Reports an error and signals failure
reportErr :: String -> IO a
reportErr msg =
  putDoc (onred (brackets (text "error")) <+> text msg <> hardline)
    >> exitFailure

-- | Warn the user about a certain non-fatal problem
warn :: String -> IO ()
warn msg =
  putDoc (ondullyellow (brackets (text "warning")) <+> text msg <> hardline)

-- | State that an impossible action has happened
impossible :: String -> a
impossible = error . printf "[%s] The impossible happened\n"
