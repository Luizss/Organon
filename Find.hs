{-# LANGUAGE TupleSections #-}

module Find where

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

import Data
import Parsers
import Helper

findAll :: [Topic] -> IO [(Topics,[(ID,Content)])]
findAll = 
  
  (final . groupByTopic . group . preprocess . concat <$>) . mapM findTopic
  
  where
    
    findTopic :: Topic -> IO [(Topics,Blocks)]
    findTopic topic = do
      x <- findBlockOrClass topic
      errorIfDoesNotMakeSense' x
      case x of
        ([file], []) -> if "~" `isSuffixOf` file
                        then return [] 
                        else do
                          res <- readAndParse file
                          return $ only (only topic, tail res)
        ([],[direc]) -> do 
          -- mudar para leitura de todos arquivos do diretório
          files <- findAllFilesFromDirectory direc
          res <- mapM readAndParse files {-(direc ++ "/index")-}
          return (map g res)
        ([],   []) -> error $ "There is no topic " ++ topic
    
    g (x:xs) = let topicName = case x of
                     Block (["always",t],_) -> t
                     _ -> error "block without always"
               in (only topicName, xs)

    preprocess = 
      concat
      . map
      (\(topics,idsAndConts) 
       -> map (\(i,c) -> TIC (topics,i,c)) idsAndConts)
      . mapInSec (map f)
      where 
        f = \(Block (["id:",id],cont)) 
            -> (read id :: Integer,cont)
    
    group xs = 
      let repeated = getDuplicatesGrouped xs
      in case repeated of
        [] -> xs
        rs -> (map (
              (foldl1
               (\(TIC (ts,id,c)) (TIC (ts',_,_)) 
                -> TIC (nub (ts ++ ts'),id,c))) 
              ) rs) ++ (nub xs \\ concat (map nub rs))
    
    
    groupByTopic = groupBy g . sortBy s
      where s (TIC (ts,_,_)) (TIC (ts',_,_)) = compare ts ts'
            g (TIC (ts,_,_)) (TIC (ts',_,_)) = ts == ts'
            
    final = map (\list@(TIC (ts,_,_):_) -> (ts,map f list))
      where f (TIC (_,id,cont)) = (id,cont)
    
    mapInSec f = map (\(a,b) -> (a, f b))

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
      
find :: [Topic] -> IO [(Topics,[(ID,Content)])]
find topics = do
  res <- findAll topics
  mat <- preProcessForFind topics
  return 
    $ map (\(ts,b) -> (sort ts,b)) 
    $ filter (hasTopics mat) res
  where hasTopics xs (tops,_) = and $ map (f tops) xs
        f ts xs = case ts `intersect` xs of
          [] -> False
          _  -> True

preProcessForFind = traverse f where
  
  f topic = do 
    a <- findBlockOrClass topic
    case a of
      ([file], []) -> return [topic]
      ([],[direc]) -> allTopicsFromDir direc
      
  allTopicsFromDir direc = do 
    paths <- getAllTopicsFromClass direc
    return (map getFileName paths)

getFileName = reverse . takeWhile (/='/') . reverse

findID :: [ID] -> IO [(Topics,ID,Content)]
findID ids = do
  
  files  <- findAllKnowledgeFiles
  blocks <- mapM readAndParse files
  let tics = preprocess blocks
      important = filter (\(_,id,_) -> id `elem` ids) tics
  return (joinTopics (groupByID important))
  
  where 
    
    findAllKnowledgeFiles = 
      filter (not . finishesWithTil)
      <$> F.find always (fileType ==? RegularFile) "./Knowledge"
    
    finishesWithTil s = "~" `isSuffixOf` s
    
    preprocess :: [Blocks] -> [(Topics,ID,Content)]
    preprocess = concat . map (\xs -> map (f (parse (head xs))) (tail xs))
  
    parse (Block (["always",top],_)) = top
    
    f :: Topic -> Block -> (Topics,Integer,Content)
    f top (Block (["id:",id],cont)) = ([top],read id :: Integer,cont)
    f _  a = error (show a)
    
    mapInSec f = map (\(a,b) -> (a, f b))
    
    groupByID = groupBy (\(_,id,_) (_,id',_) -> id == id')
                . sortBy (\(_,id,_) (_,id',_) -> id `compare` id')
    
    joinTopics = map (foldl (\(ts,_,_) (ts',id,c) -> (ts++ts',id,c)) ([],undefined,undefined))
    
findAllFilesFromDirectory :: FilePath -> IO [FilePath]
findAllFilesFromDirectory dir = 
  filter (not . finishesWithTil)
  <$> 
  F.find 
  always 
  (fileType ==? RegularFile)
  dir
  where finishesWithTil s = "~" `isPrefixOf` (reverse s)
        
findAllTilFiles :: FilePath -> IO [FilePath]
findAllTilFiles dir = 
  filter (finishesWithTil)
  <$> 
  F.find 
  always 
  (fileType ==? RegularFile)
  dir
  where finishesWithTil s = "~" `isPrefixOf` (reverse s)

getAllTopicsFromClass className = 
  filter (not . finishesWithTil)
  <$> 
  F.find 
  always 
  (fileName /=? "index")
  className
  where finishesWithTil s = "~" `isPrefixOf` (reverse s)
        
findTopic :: Topic -> IO FilePath
findTopic topic = do
  found <- F.find 
           always 
           (fileName ==? topic)
           "./Knowledge"
  case found of
    []  -> error $ "No topic " ++ topic ++ "(???)" 
    [x] -> return x
    _   -> error $ "Topic " ++ topic ++ " doubled."
                    
findTopic' :: Topic -> IO FilePath
findTopic' topic = do
  found <- F.find 
           always 
           (fileName ==? topic)
           "./Knowledge"
  case found of
    []  -> return ""
    [x] -> return x
    _   -> error $ "Topic " ++ topic ++ " doubled."

getPath :: FilePath -> FilePath
getPath = reverse . tail . dropWhile (/='/') . reverse

getTopicsFromDirectory :: FilePath -> IO Topics
getTopicsFromDirectory dir = do
  paths <- F.find (filePath ==? dir) (fileName /=? "index") dir
  return 
    $ filter 
    (not . ("~"`isSuffixOf`))
    [getFileName p | p <- paths , p /= dir] 

getAllTopics :: IO Topics
getAllTopics = do
  paths <- F.find always (fileName /=? "index") "./Knowledge"
  return
    $ filter
    (not . ("~"`isSuffixOf`))
    [getFileName p | p <- paths , p /= "./Knowledge"]
    
getNextID :: IO Integer
getNextID = do
  topics <- getAllTopics
  all    <- findAll topics
  let ids = extractIDs all
      id  = getUnusedID $ nub $ sort ids
  print (sort ids)
  print [0..15]
  return id
  
  where
    
    extractIDs :: [(Topics,[(ID,Content)])] -> [ID]
    extractIDs = concat . map extractID
    
    extractID :: (Topics,[(ID,Content)]) -> [ID]
    extractID (topics,list) = map extractFromTuple list
    
    extractFromTuple :: (ID,Content) -> ID
    extractFromTuple (id,_) = id
    
getUnusedID :: [Integer] -> Integer
getUnusedID ids = headIf ids
                  $ dropWhile (\(bool,_) -> bool == False)
                  $ zipWith (\x y -> ( x /= y, x ) ) [0..]
                  $ ids

headIf :: [Integer] -> [(Bool,Integer)] -> Integer
headIf []  [] = 0
headIf ids [] = last ids + 1
headIf _   xs = (\(bool,int) -> int) $ head xs