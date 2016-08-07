module ToClass where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative,(<$))
import Data.Char
import Text.Printf
import Data.Traversable hiding (mapM)
import Data.Foldable hiding (concat,foldl1,foldl,and,elem,mapM_,or,concatMap)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import Data.List -- (isPrefixOf,findIndices,union,)
import System.FilePath.Find as F
import Data.Maybe
import System.Process
import Control.Monad
import GHC.IO.Exception

import Data
import Helper
import Find
import Parsers

--- class = (U todos) U index
--- index = class - (U todos)

{- Helpers -}

isClass :: Topic -> IO (Bool,FilePath)
isClass master = caseOf <$> findBlockOrClass master where
  
  caseOf tup = case tup of
    ([file], []) -> ( False, father file  )
    ([],[direc]) -> ( True , father direc ) 
    _            -> error "error is class"
    
  father = reverse . tail . dropWhile (/='/') . reverse

moveTopicsTo :: FilePath -> Topics -> IO ()
moveTopicsTo dir [] = return ()
moveTopicsTo dir topics = do
  
  path <- mapM findTopic topics
  h <- runCommand $ "mv " ++ intercalate " " path ++ " " ++ dir
  a <- waitForProcess h
  a `seq` return ()
  
readAndParseSlaves :: Topics -> IO Blocks
readAndParseSlaves topics = do
  
  results <- findAll topics
  
  let results' :: [(ID,Content)]
      results' = concat $ map snd results
      mapFst f tups = map (\(a,b) -> (f a, b)) tups 
      blocks = map Block $ mapFst (\id -> ["id:",show id]) results'
      
  return blocks

checkTopicsFolder :: FilePath -> Topics -> IO ()
checkTopicsFolder fatherPath = mapM_ checkTopicFolder where
  
  checkTopicFolder topic = do
    
    path <- getPath <$> findTopic topic
    
    if path == fatherPath
      then return ()
      else error $ "Topic " ++ topic ++ " is not on " ++ fatherPath

removeDir :: FilePath -> a -> IO ()
removeDir path _ = removeDirectory path

removeAllTils :: FilePath -> a -> IO ()
removeAllTils dir _ = do
  tils <- findAllTilFiles dir
  case tils of
    [] -> return ()
    _ -> do
      h <- runCommand $ "rm " ++ intercalate " " tils
      a <- waitForProcess h
      a `seq` return ()

parseEverythingFromDirectory :: FilePath -> IO Blocks
parseEverythingFromDirectory path = do
  let name = getFileName path
  readAndParseSlaves [name]

{- From Topic ToClass -}

toClass :: [Topic] -> IO ()
toClass              [] = error "Too few arguments."
toClass        [master] = error "Too few arguments."
toClass (master:slaves) = do
  
  (isIt,fatherPath) <- isClass master
      
  case isIt of
    
    True  -> do
      
      let masterPath = fatherPath ++ "/" ++ master
          indexFile  = masterPath ++ "/index"
          
      doesIt <- doesFileExist indexFile
      case doesIt of
        
        False -> do
          
          checkTopicsFolder fatherPath slaves
      
          moveTopicsTo masterPath slaves
          putStrLn $ 
            "Topics " 
            ++ sayList slaves 
            ++ " moved from " 
            ++ fatherPath 
            ++ " to " 
            ++ masterPath
            
        True -> do
        
          parsedMaster <- tail <$> readAndParse indexFile --masterPath
          parsedSlaves <- readAndParseSlaves slaves
        
          let index     = nub $ nub parsedMaster \\ parsedSlaves 
              indexText = readDeParse (pred:index)
              pred      = Block (["always",master],"")
          
          checkTopicsFolder fatherPath slaves
      
          moveTopicsTo masterPath slaves
          putStrLn $ 
            "Topics " 
            ++ sayList slaves 
            ++ " moved from " 
            ++ fatherPath 
            ++ " to " 
            ++ masterPath

          case index of
            [] -> removeFile indexFile
            _  -> do
              writeFile indexFile indexText
              putStrLn 
                $ "Success. " 
                ++ master 
                ++ " is now a class with " 
                ++ sayList slaves 
                ++ "."
    
    False -> do
      
      let masterPath = fatherPath ++ "/" ++ master
          
      parsedMaster <- tail <$> readAndParse masterPath
      parsedSlaves <- readAndParseSlaves slaves
      
      let index     = nub $ nub parsedMaster \\ parsedSlaves 
          indexText = readDeParse (pred:index)
          pred = Block (["always",master],"")
          
      checkTopicsFolder fatherPath slaves
      
      removeFile masterPath
        
      createDirectory masterPath
      putStrLn $ masterPath ++ " folder created."
      
      moveTopicsTo masterPath slaves
      putStrLn $ 
        "Topics " 
        ++ sayList slaves 
        ++ " moved from " 
        ++ fatherPath 
        ++ " to " 
        ++ masterPath

      case index of
        [] -> return () 
        _  -> do
          writeFile (masterPath ++ "/index") indexText
          putStrLn 
            $ "Success. " 
            ++ master 
            ++ " is now a class with " 
            ++ sayList slaves 
            ++ "."

{- From Class ToTopic -}
          
toTopic :: Topics -> IO ()
toTopic [] = error "No topics."
toTopic ts = mapM_ toTopicOne ts

toTopicOne :: Topic -> IO ()
toTopicOne master = do
  
  (isIt,fatherPath) <- isClass master
  
  case isIt of
    False -> putStrLn $ "Topic " ++ master ++ " is already a topic."
    True  -> do
      
      let masterPath = fatherPath ++ "/" ++ master
          
      parseds <- parseEverythingFromDirectory masterPath
      
      let classBlocks = nub parseds
          classText   = readDeParse (pred:classBlocks)
          pred        = Block (["always",master],"")
          
      removeFileIfExists (masterPath ++ "/index")
      
      topics <- getTopicsFromDirectory masterPath
      
      a <- moveTopicsTo fatherPath topics
      putStrLn ("Topics " ++ sayList topics ++ " moved.")
      
      b <- removeAllTils masterPath a
      
      removeDir masterPath b
      putStrLn (masterPath ++ " folder removed.")
      
      writeFile (fatherPath ++ "/" ++ master) classText
      putStrLn $ masterPath ++ " file created."
      
      putStrLn $ "Success. " ++ master ++ " is now a topic."
      
      return ()