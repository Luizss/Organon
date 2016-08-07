module MakePDF where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative)
import Data.Char
import Text.Printf
import Data.Traversable hiding (mapM)
import Data.Foldable hiding (concat,foldl1,foldl,and,elem,mapM_,or)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import Data.List 
import System.FilePath.Find as F
import System.Process

import Data
import Helper
import Parsers
import Find
import Read

makePDFTemp :: FilePath -> () -> IO ()
makePDFTemp inputFile () = do
  createDirectoryIfMissing' "./Results/Temp"
  h <- runCommand 
       $ "cd ./Results/Temp && xelatex --shell-escape " ++ inputFile
       -- $ "xelatex --output-dir=./Results/Temp " ++ inputFile
  waitForProcess h
  g <- runCommand 
       $ "cd ./Results/Temp && xelatex --shell-escape " ++ inputFile
      -- $ "xelatex --output-dir=./Results/Temp " ++ inputFile
  waitForProcess g
  return ()
  where createDirectoryIfMissing' = createDirectoryIfMissing True
  
makePDFs :: [FilePath] -> IO ()
makePDFs = mapM_ makePDF

makePDF :: FilePath -> IO ()
makePDF inputFile = do
  let as   = divideWith '/' inputFile
      dir  = head as
      file = head (tail as)
  h <- runCommand 
       $ intercalate " && "
       [ "cd Results/" ++ dir
       , "xelatex --shell-escape " ++ file ++ ".tex" 
       , "mv " ++ file ++ ".pdf ../PDFs/"]
       -- $ "xelatex --output-dir=./Results/PDFs " ++ dir ++ inputFile ++ ".tex"
  waitForProcess h
  g <- runCommand 
       $ intercalate " && "
       [ "cd Results/" ++ dir
       , "xelatex --shell-escape " ++ file ++ ".tex" 
       , "mv " ++ file ++ ".pdf ../PDFs/"]
       -- $ "xelatex --output-dir=./Results/PDFs " ++ dir ++ inputFile ++ ".tex"
  waitForProcess g
  return ()
  where createDirectoryIfMissing' = createDirectoryIfMissing True

displayPDF :: FilePath -> () -> IO ()
displayPDF inputFile () = do
  h <- runCommand $ "evince " ++ inputFile
  waitForProcess h
  return ()