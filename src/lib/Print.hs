{-
Title: Print Module
Description: Module for printing of circuits
Author: Pablo Baeyens
-}
module Print(captureTo,
             printToFile,
             defaultOutputPath,
             defaultStyle,
             logPrint) where

import Quipper
import Quipper.Printing
import System.IO
import GHC.IO.Handle
import Control.Exception
import System.FilePath (pathSeparator)
import Graphics.EasyRender
import Data.Semigroup ((<>))
import Options.Applicative.Help.Pretty

-- | The default style used for circuits
defaultStyle :: Format
defaultStyle = CustomStyle (pdf { subroutineshape = False })

-- | Capture output to a file
-- This function uses GHC's internal libraries
-- and therefore is GHC-only
captureTo :: FilePath -> IO a -> IO a
captureTo file action = withFile
  file
  WriteMode
  (\handle -> bracket (capture handle) restore (const action))
 where
  -- Capture stdout to a given handle and return a duplicate of stdout
  capture handle = do
    buffering  <- hGetBuffering stdout
    stdout_dup <- hDuplicate stdout
    hDuplicateTo handle stdout
    pure (stdout_dup, buffering)

  -- Restore stdout
  restore (stdout_dup, buffering) = do
    hDuplicateTo  stdout_dup stdout
    hSetBuffering stdout     buffering
    hClose stdout_dup


-- | Print a circuit to a file
printToFile
  :: (QCData qa, QCData qb)
  => FilePath
  -> Format
  -> (qa -> Circ qb)
  -> qa
  -> IO ()
printToFile _ Preview _ = error "[printToFile] Can't print 'Preview' to a file"
printToFile file format circ = captureTo file . print_generic format circ


-- | Log that a certain file is being printed
logPrint :: String -> FilePath -> IO ()
logPrint title filePath = putDoc
  (   onblue (brackets (text "print"))
  <+> text "Printing "
  <>  text title
  <>  text " to '"
  <>  text filePath
  <>  text "'."
  <>  hardline
  )

-- | Default path to store output files
defaultOutputPath :: FilePath
defaultOutputPath = "img" ++ [pathSeparator]
