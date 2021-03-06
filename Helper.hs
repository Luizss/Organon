module Helper where

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
import Data.List hiding (find) --(isPrefixOf,findIndices,union)
import System.FilePath.Find as F
import Data.Maybe

import Data

-- Organon

only :: a -> [a]
only a = [a]

after :: Char -> Char -> Parser String
a `after` b = string (a : [b])

manyExcept :: [Char] -> Parser String
manyExcept = many . noneOf

many1Except :: [Char] -> Parser String
many1Except = many1 . noneOf

manyExceptWithoutSpacesAtBorders :: [Char] -> Parser String
manyExceptWithoutSpacesAtBorders ls = do
  a <- many (noneOf ls)
  return (noSpacesAtBorders a)

fromMaybeToList :: Maybe a -> [a]
fromMaybeToList (Just a) = [a]
fromMaybeToList Nothing  = []

-- Configurations

fromEither (Left  e) = error "From Either"
fromEither (Right r) = r

-- PreRender

noRedundancy :: Tree a -> Tree a
noRedundancy (Branch [x]) = noRedundancy x
noRedundancy (Leaf     x) = Branch [Leaf x]
noRedundancy            t = t

noRedundancy'' :: Tree a -> Tree a
noRedundancy'' (Branch [Branch [x]]) = Branch [x]
noRedundancy'' (Branch [x]) = noRedundancy x
noRedundancy'' (Leaf     x) = Branch [Leaf x]
noRedundancy''            t = t

noRedundancy' :: [Tree a] -> Tree a
noRedundancy' = noRedundancy . Branch

noRedundancy''' :: [Tree a] -> Tree a
noRedundancy''' = noRedundancy'' . Branch

noSpacesAtBorders :: String -> String
noSpacesAtBorders = 
  reverse 
  . (dropWhile isSpace) 
  . reverse 
  . (dropWhile isSpace)

optionList thing =
  optionMaybe thing >>= \x -> 
  case x of
    
    Nothing -> return []
    Just a  -> return [a]
                             
fromBranch (Branch xs) = xs
fromBranch _ = error "wut?"

-- Read

noSpacesAtEnd = reverse . dropWhile isSpace . reverse
noSpacesAtHead = dropWhile isSpace
    
interactWithFile file f = do
  contents <- Strict.run $ Strict.readFile file
  result   <- f contents
  Strict.run $ Strict.writeFile file result

writeFile' file = Strict.run . Strict.writeFile file

errorIfDirectoryDoesNotExists dir = do
  ans <- doesDirectoryExist dir
  case ans of
    False -> error 
             $ dir ++ " does not exist here. Make sure to use organon init."
    True  -> return ()

errorIfFileDoesNotExists file msg = do
  ans <- doesFileExist file
  case ans of
    False -> error msg
    True  -> return ()

errorIfDoesNotMakeSense []  [] = return ()
errorIfDoesNotMakeSense [x] [] = return ()
errorIfDoesNotMakeSense xs  [] = error "WTF2"
errorIfDoesNotMakeSense [] [x] = return ()
errorIfDoesNotMakeSense []  xs = error "wotf2"
errorIfDoesNotMakeSense x    y = error "WTF"

errorIfDoesNotMakeSense' ([]  ,[]) = return ()
errorIfDoesNotMakeSense' ([x] ,[]) = return ()
errorIfDoesNotMakeSense' (xs  ,[]) = error "WTF2"
errorIfDoesNotMakeSense' ([] ,[x]) = return ()
errorIfDoesNotMakeSense' ([]  ,xs) = error "wotf2"
errorIfDoesNotMakeSense' (x    ,y) = error "WTF"

-- find

getDuplicatesGrouped :: (Ord a, Eq a) => [a] -> [[a]]
getDuplicatesGrouped = filter isOnly .  group . sort
  where safeSnd = listToMaybe . drop 1
        isOnly [x] = False
        isOnly _   = True

errorIfThereAreNoBlocks :: [a] -> Topics -> IO ()
errorIfThereAreNoBlocks blocksFound topics = 
  case blocksFound of 
    [] -> error 
          $ "There are no blocks found for "
          ++ intercalate ", " topics
    _ -> return ()
    
mapOnlyTree fTree fileS = map textOnly fileS
  where textOnly    (Text    xs) = 
          Text $ fromBranch $ fTree (Branch xs)
        textOnly id@(Struct s a) = id

mapOnlyText fText fileS = map textOnly fileS
  where textOnly    (Text    xs) = Text $ fText xs
        textOnly id@(Struct s a) = id
        
traverseOnlyTree :: (Tree String -> IO (Tree String)) 
                    -> FileStructure String 
                    -> IO (FileStructure String)
traverseOnlyTree fTree fileS = traverse textOnly fileS
  where textOnly    (Text    xs) = 
          Text <$>  fromBranch <$> fTree (Branch xs)
        textOnly id@(Struct s a) = return id

traverseOnlyText :: ([Tree String] -> IO [Tree String])
                    -> FileStructure String 
                    -> IO (FileStructure String)
traverseOnlyText fText fileS = traverse textOnly fileS
  where textOnly    (Text    xs) = Text <$> fText xs
        textOnly id@(Struct s a) = return id

insertArguments :: (Functor f, Functor g)
                   => f (g a) -> f (g (Result a))
insertArguments = fmap (fmap Argument)

filterStructure (Text      []) = False
filterStructure (Struct "" "") = False
filterStructure _              = True
    
translatorToLower    (Struct t a) = Struct (map toLower t) a
translatorToLower id@(Text     _) = id

-- Save File

{-
saveFile = "savefile"


getNextID :: IO Integer
getNextID = read <$> do
  errorIfDoesNotExist saveFile
  Strict.run $ Strict.readFile saveFile
        
putNextID :: Integer -> IO ()
putNextID int = do
  errorIfDoesNotExist saveFile
  let newContent = show int
  writeFile saveFile newContent
  return ()
  
withNextID :: (Integer -> Integer) -> IO ()
withNextID f = do
  id <- getNextID 
  let newID = f id
  putNextID newID
-}

errorIfDoesNotExist file = do
  doesIt <- doesFileExist file
  if doesIt
    then return ()
    else error $ "File " ++ file ++ " does not exist."

removeFileIfExists file = do
  doesit <- doesFileExist file
  case doesit of
    True  -> removeFile file
    False -> return ()

sayList :: [String] -> String
sayList   [] = []
sayList  [s] = s
sayList list = intercalate ", " (init list) ++ " and " ++ last list

sayList' :: [String] -> String
sayList'   [] = []
sayList'  [s] = s
sayList' list = intercalate ", " list

createFileIfDoesNotExist file topic = do
  doesIt <- doesFileExist file
  case doesIt of
    True  -> return ()
    False -> writeFile file ("--- always " ++ topic)
    
divideWith :: Char -> String -> [String]
divideWith divisor txt = map noSpacesAtBorders 
                         $ divideWith' ([],txt) 
  where 
  
    divideWith' (left,right)
      | divisor `elem` right 
        && last right == divisor = divideWith' (left,init right)
      | divisor `elem` right = let (l,r) = break (==divisor) right
                               in divideWith' (left ++ [l],tail r)
      | otherwise = left ++ [right]

escapeBrackets :: String -> String
escapeBrackets = 
  concat . map 
  (\c -> 
    case c of
      '{' -> "\\{"
      '}' -> "\\}"
      c   -> [c]
  )