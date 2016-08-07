module FindCommands where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative)
import Data.Char
import Text.Printf
import Data.Traversable hiding (mapM)
import Data.Foldable hiding (concat,foldl1,foldl,and,elem,mapM_,or,find)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import Data.List hiding (find) --(isPrefixOf,findIndices,union)
import System.FilePath.Find as F hiding (find)
import Data.Maybe

import Data
import Helper
import Find
import Check

{- Auxiliar -}

toText :: [(Topics,[(ID,Content)])] -> String
toText found = intercalate "\n\n" $ map f found where
  f (topics,idsAndConts) =
    "-------> " ++ sayList topics ++ "\n\n"
    ++ intercalate "\n\n" (map showIC idsAndConts)
  showIC (id,cont) = 
    "--- ID: " ++ show id ++ "\n\n"
    ++ cont

toTextID :: [(Topics,ID,Content)] -> String
toTextID = intercalate "\n\n" . map toText where
  toText (topics,id,content) =
    "------> ID: " ++ show id ++ "\n\n"
    ++ "--- " ++ sayList topics ++ "\n\n"
    ++ content

toStructure :: [(Topics,[(ID,Content)])] -> String
toStructure found = intercalate "\n\n" 
                    $ putTitle 
                    $ map f found where
  f (topics,idsAndConts) =
    "--- section1 " ++ sayList' topics ++ ":\n\n"
    ++ intercalate "\n\n" (map showIC idsAndConts)
  showIC (id,cont) = 
    "{ par " ++ show id ++ " | "  ++ cont ++ "}"
    
toStructureID :: [(Topics,ID,Content)] -> String
toStructureID = intercalate "\n\n" . putTitle . map toText where
  toText (topics,id,content) =
    "--- section1 ID: " ++ show id ++ "\n\n"
    ++ "{ par " ++ sayList' topics ++ ": | " ++ content ++ " }"

putTitle = ("--- title Topics":)

{- Finds -}

findComm :: Topics -> IO ()
findComm topics = do
  found <- find topics
  let text = toText found
  if and (map isSpace text)
    then putStrLn "No topics."
    else putStrLn text

findAllComm :: Topics -> IO ()
findAllComm topics = do
  found <- findAll topics
  let text = toText found
  if and (map isSpace text)
    then putStrLn "No topics."
    else putStrLn text

findIdComm :: [String] -> IO ()
findIdComm idsText = do
  let ids = map read idsText :: [Integer]
  found <- findID ids
  let text = toTextID found
  if and (map isSpace text)
    then putStrLn "Nothing found."
    else putStrLn text
    
getComm :: Topics -> IO ()
getComm topics = do
  found <- find topics
  let text = toStructure found
  if and (map isSpace text)
    then putStrLn "No topics."
    else textToPDF "article" text

getAllComm :: Topics -> IO ()
getAllComm topics = do
  found <- findAll topics
  let text = toStructure found
  putStr text
  if and (map isSpace text)
    then putStrLn "No topics."
    else textToPDF "article" text

getIdComm :: [String] -> IO ()
getIdComm idsText = do
  let ids = map read idsText :: [Integer]
  found <- findID ids
  let text = toStructureID found
  if and (map isSpace text)
    then putStrLn "Nothing found."
    else textToPDF "article" text