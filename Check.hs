module Check where

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
import Control.Concurrent

import Data
import Helper
import Parsers
import Find
import Read
import MakePDF
import Render

checkTopics :: [String] -> IO ()
checkTopics args = do
  let dir = "./Topics/"
  errorIfDirectoryDoesNotExists dir
  case args of
    [arg]    -> checkTopics' "article" (dir++arg)
    (fmt:as) -> mapM_ (checkTopics' fmt . (dir++)) as

checkTopics' :: Format -> String -> IO ()
checkTopics' fmt file = do
  blocks <- readAndParse file
  let blocks' = preProcessBlock blocks
      txt     = toStringBlocks fmt blocks'
  textToPDF fmt txt
  return ()

textToPDF :: Format -> String -> IO ()
textToPDF fmt txt = do
  a <- renderText fmt txt
  b <- makePDFTemp "temp.tex" a
  displayPDF "./Results/Temp/temp.pdf" b
  return ()

renderText :: Format -> String -> IO ()
renderText fmt txt = do
  writeFile "./Results/Temp/temp" txt --
  renderFile fmt "./Results/Temp/temp" "./Results/Temp/temp"
  
toStringBlocks :: Format -> Blocks -> String
toStringBlocks fmt = intercalate "\n\n" 
                     . putHead fmt 
                     . map toStringBlock

putHead :: Format -> [String] -> [String]
putHead fmt as = case fmt of 
  "unb" ->   "--- unb-title Checking"
           : "--- unb-capa"
           : "--- unb-inicio" 
           : as
  _     -> "--- title Checking" : as

toStringBlock :: Block -> String
toStringBlock (Block (tops,text)) = 
  intercalate "\n"
  [ "{par " ++ sayList' tops ++ ":"
  , "| " ++ text
  , "}"]

checkStructure :: [String] -> IO ()
checkStructure args = do
  let dir = "./Texts/"
  errorIfDirectoryDoesNotExists dir
  case args of
    [arg]    -> checkStructure' "article" (dir++arg)
    (fmt:as) -> mapM_ (checkStructure' fmt . (dir++)) as
    
checkStructure' :: Format -> FilePath -> IO ()
checkStructure' fmt file = do
  a <- renderFile fmt file "./Results/Temp/temp"
  b <- makePDFTemp "temp.tex" a
  displayPDF "./Results/Temp/temp.pdf" b
  return ()
  
checkTopicsCompilation :: [String] -> IO ()
checkTopicsCompilation args = do
  let dir = "./Topics/"
  errorIfDirectoryDoesNotExists dir
  case args of
    [arg]    -> checkTopicsCompilation' "article" (dir++arg)
    (fmt:as) -> mapM_ (checkTopics' fmt . (dir++)) as

checkTopicsCompilation' :: Format -> String -> IO ()
checkTopicsCompilation' fmt file = do
  blocks <- readAndParse file
  let blocks' = preProcessBlock blocks
      txt     = toStringBlocks fmt blocks'
  a <- renderText fmt txt
  b <- makePDFTemp "temp.tex" a
  return ()
