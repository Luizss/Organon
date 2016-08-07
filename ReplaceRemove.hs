{-# LANGUAGE ViewPatterns #-}

module ReplaceRemove where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative)
import Data.Char
import Text.Printf
import Data.Traversable
import Data.Foldable hiding (concat,foldl1,foldl,and,mapM_)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import System.Process
import Data.List (intercalate,intersect,nub,union)

import Data
import Helper
import Find
import Parsers
import ToClass

replace :: [FilePath] -> IO ()
replace [] = error "No file to replace."
replace xs = mapM_ replaceOne xs

replaceOne :: FilePath -> IO ()
replaceOne filepath' = do
  
  let filepath = ("./Texts/" ++ filepath')
  
  replaced <- readAndParse filepath
  let idsAndContent = findIDsOnFile replaced
      
  forM_ idsAndContent $ \(id,newCont) -> do
    
    (topics,_,cont) <- findOneID id
    
    forM topics $ \topic -> do
      isIt   <- isClass topic
      file   <- findTopic topic
      
      let file' = case isIt of
            (True  ,_) -> file ++ "/index"
            (False ,_) -> file

      parsed <- readAndParse file'

      let result = replaceContent (id,newCont) parsed
          text = readDeParse result
          
      writeFile (file' ++ ".new") text
      
      renameFile file' (file' ++ ".old")
      renameFile (file' ++ ".new") file'
      
      removeFile (file' ++ ".old")
      
  where
    
    findIDsOnFile :: Blocks -> [(ID,Content)]
    findIDsOnFile = map findIDsOnBlock
    
    findIDsOnBlock :: Block -> (ID,Content)
    findIDsOnBlock (Block (topics,content)) = case topics of
      [map toLower -> "id:", id] -> (read id,content)
      _                          -> error "Error on findIDsOnBlock"
      
    replaceContent :: (ID,Content) -> Blocks -> Blocks
    replaceContent (id,newContent) = map replaceIfIDEquals where
      replaceIfIDEquals id@(Block (["always",_],_)) = id
      replaceIfIDEquals block = 
        let (id',content) = findIDsOnBlock block
        in if id == id'
           then Block ( toTopics id', newContent )
           else Block ( toTopics id', content )

                
    toTopics :: ID -> Topics
    toTopics id = ["ID:",show id]
    
remove :: [String] -> IO ()
remove [] = error "No IDs to remove."
remove idTexts = let ids = map read idTexts :: [Integer]
                 in mapM_ removeOne ids
                    
removeOne :: ID -> IO ()
removeOne id = do
  
    (topics,_,cont) <- findOneID id
    
    forM_ topics $ \topic -> do
      isIt   <- isClass topic
      file   <- findTopic topic
      
      let file' = case isIt of
            (True  ,_) -> file ++ "/index"
            (False ,_) -> file

      parsed <- readAndParse file'

      let result = removeContent id parsed
          text   = readDeParse result
          
      writeFile (file' ++ ".new") text
      
      renameFile file' (file' ++ ".old")
      renameFile (file' ++ ".new") file'
      
      removeFile (file' ++ ".old")
      
  where
    
    findIDsOnFile :: Blocks -> [(ID,Content)]
    findIDsOnFile = map findIDsOnBlock
    
    findIDsOnBlock :: Block -> (ID,Content)
    findIDsOnBlock (Block (topics,content)) = case topics of
      [map toLower -> "id:", id] -> (read id,content)
      _                          -> error "Error on findIDsOnBlock"
    
    removeContent :: ID -> Blocks -> Blocks
    removeContent id = filter replaceIfIDEquals where
      replaceIfIDEquals (Block (["always",_],_)) = True
      replaceIfIDEquals block = 
        let (id',content) = findIDsOnBlock block
        in if id == id'
           then False
           else True

addTag    []     = error "No inputs"
addTag [tag]     = error "No ids given"
addTag (tag:ids) = let ids' = map read ids :: [Integer]
                   in mapM_ (addTagOne tag) ids'

addTagOne :: Topic -> ID -> IO ()
addTagOne tag id = do
  (topics,_,content) <- findOneID id
  file <- findTopic' tag
  print file
  let dir = "./Knowledge/"
  case file of
    "" -> do
      let text = readDeParse [Block (["always",tag],"")
                             ,Block (toTopics id,content)]
      writeFile' (dir ++ tag) text
    _  -> do
      isIt <- isClass tag    
      case fst isIt of
        True -> do
          topics' <- getTopicsFromDirectory file
          case topics `intersect` topics' of
            [] -> addtagtofile (file ++ "/index") content
            _  -> return () -- do nothing
        False -> addtagtofile file content

  where
    
    addtagtofile file content = do
      createFileIfDoesNotExist file tag
      parsed <- readAndParse file
      let result = addTagToBlocks id content parsed
          text   = readDeParse result
      replaceFile file text
                   
    addTagToBlocks id content parsed 
      = let newblock = Block (toTopics' id,content)
        in map idsCaps $ nub (parsed `union` [newblock])
    toTopics' :: ID -> Topics
    toTopics' id = ["id:",show id]
    toTopics :: ID -> Topics
    toTopics id = ["ID:", show id]
    idsCaps :: Block -> Block
    idsCaps (Block (["id:",id],cont)) = Block (["ID:",id],cont)
    idsCaps b = b
    
removeTag []        = error "No inputs"
removeTag [tag]     = error "No ids given"
removeTag (tag:ids) = let ids' = map read ids :: [Integer]
                      in mapM_ (removeTagOne tag) ids'

removeTagOne :: Topic -> ID -> IO ()
removeTagOne tag id = do
  (topics,_,content) <- findOneID id
  file <- findTopic' tag
  case file of
    "" -> error $ "There is no topic " ++ tag ++ "."
    _  -> do
      isIt <- isClass tag    
      case fst isIt of
        True  -> do
          topics' <- getTopicsFromDirectory file
          let intersect' = topics `intersect` topics'
          case intersect' of
            [] -> return ()               
            xs -> let removeTagOne' id tag = removeTagOne tag id
                  in mapM_ (removeTagOne' id) xs
          removetagtofile (file ++ "/index")
        False -> removetagtofile file
      
  where
    
    removetagtofile file = do
      createFileIfDoesNotExist file tag
      parsed <- readAndParse file
      let result = removeTagToBlocks id parsed
          text   = readDeParse result
      replaceFile file text
    
    removeTagToBlocks id = filter (diffID id)
    
    diffID :: ID -> Block -> Bool
    diffID id (Block (topics,cont)) = case topics of
      ["always",_] -> True
      [_,idtxt] -> let idint = read idtxt :: Integer
                   in not (id == idint)

findOneID id = headWithError <$> findID (only id)

headWithError [] = error "No ID found."
headWithError xs = head xs

replaceFile file text = do
  
  writeFile' (file ++ ".new") text
      
  renameFile file (file ++ ".old")
  renameFile (file ++ ".new") file
    
  removeFile (file ++ ".old")