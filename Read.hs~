module Read where

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
import Data.List (isPrefixOf,findIndices,union)
import System.FilePath.Find as F

import Data
import Helper
import Parsers
import Find

{- Top Level -}

readComm args = do
  
  let dir = "./Topics/"
  
  errorIfDirectoryDoesNotExists dir
  
  mapM_ (readTopicFile dir) args
  
  where
    
    readTopicFile dir file = do
      blocks <- readAndParse (dir ++ file)
      let blocks' = preProcessBlock blocks
      putBlocksInKnowledge blocks'
      renameFile (dir ++ file) (dir ++ file ++ "READ")
      --print blocks' -- test

    
{- Pre-Processing -}

preProcessBlock :: Blocks -> Blocks
preProcessBlock tops = 
      
    stripOutEmptyTexts  
  . concat
  . splitList
  . zipIndexesShifted
  . putZeroFirstIf
  . putLengthLast
  . findIndices isAlways
  $ tops
      
  where
    
    isAlways (Block ts) = case ts of
      (("always":_),"") -> True
      (("always":_), x) -> error "Error: Block after 'always'"
      _                 -> False

    putLengthLast xs = xs ++ [length tops]
    
    putZeroFirstIf id@(0:_) = id
    putZeroFirstIf id@(n:_) = 0:id
    
    zipIndexesShifted xs = zip xs (tail xs)
           
    splitList = 
      map 
      (\(l,r) -> let list = take (r-l) (drop l tops)
                     x = head list
                 in if isAlways x
                    then let Block ("always":ts,_) = x
                         in tail 
                            $ map 
                            (\(Block (ts',as)) -> Block (ts' `union` ts,as)) 
                            list
                    else list)
      
    stripOutEmptyTexts = filter (\(Block (ts,cont)) -> cont /= "")
        
{- Criação dos arquivos dentro da pasta Knowledge -}

putBlocksInKnowledge :: Blocks -> IO ()
putBlocksInKnowledge = mapM_ putBlock

  where 
    
    putBlock (Block (topics,cont)) = do
      bools <- mapM (putTopic cont) topics
      return ()
      
    putTopic :: Content -> Topic -> IO Bool
    putTopic cont topic = do
      res <- checkTopicExistence topic
      case res of
        False -> createTopic topic >> putTopic cont topic
        True  -> insertContent topic cont
    
    checkTopicExistence topic = do
      ans <- findBlock topic
      case ans of
        []  -> return False
        [f] -> return True
        _   -> error "Two or more equal topics. WTF."

    createTopic topicName = writeFile' 
                            ("./Knowledge/" ++ topicName)
                            ("--- always " ++ topicName ++ "\n\n")
    
    insertContent :: Topic -> Content -> IO Bool
    insertContent topic content = do
      found <- findBlockOrClass topic
      case found of
        ([filePath], []) -> insertContentIfIsNotDoubled 
                            filePath 
                            topic 
                            content
        ([],[direcPath]) -> do
          let path = direcPath ++ "/index" 
          initFileIfDoesNotExist path topic
          insertContentIfIsNotDoubled path topic content
        _ -> error "?!?!?"
        
    initFileIfDoesNotExist path topic = do
      doesIt <- doesFileExist path
      case doesIt of
        False -> writeFile' path ("--- always " ++ topic ++ "\n\n")
        True  -> return ()
    
    insertContentIfIsNotDoubled path topic cont = do
      
      blocks <- preProcessBlock <$> readAndParse path
      
      let equals = length $ filter equalContent blocks
          equalContent (Block (_,cont')) =
            noSpacesAtBorders cont' == noSpacesAtBorders cont
            
      case equals of
        0 -> do interactWithFile path concatenateNewContent
                putStrLn $ "(" ++ cont ++ ") " ++ "read in " ++ path
                return True
        1 -> return False
        _ -> error "waaaaaaaaaaaaaaaaaaaat"
    
      where 
        
        concatenateNewContent oldCont = do
          newCont <- style topic cont
          return $ noSpacesAtEnd oldCont ++ "\n\n" ++ newCont
          
        style topic content = do
          id <- getNextID
          return 
            $ "--- ID: " ++ show id ++ "\n\n"
            ++ content ++ "\n\n"
            
    findBlock topic = do
      errorIfDirectoryDoesNotExists "./Knowledge"
      paths <- F.find 
               always 
               (fileName ==? topic)
               "./Knowledge"
      return paths
  
    findBlockOrClass topic = do
      errorIfDirectoryDoesNotExists "./Knowledge"
      file  <- F.find
               always
               (fileName ==? topic &&? fileType ==? RegularFile)
               "./Knowledge"
      direc <- F.find
               always
               (fileName ==? topic &&? fileType ==? Directory)
               "./Knowledge"
      errorIfDoesNotMakeSense file direc
      return (file,direc)