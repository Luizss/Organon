module Render where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative)
import Data.Char
import Text.Printf
import Data.Traversable
import Data.Foldable hiding (concat,foldl1,foldl,and,elem,mapM_)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import Data.List (isPrefixOf,intercalate)
import GHC.IO.Handle
import System.Process

import Data
import Helper
import PreRender
import Parsers
import Tree
import Find

render :: [String] -> IO ()
render args = do
  
  let dir    = "./Texts/"
      outDir = "./Results/"
      p = head . divideWith '-'
      
  mapM_ errorIfDirectoryDoesNotExists [dir,outDir]
  
  case args of
    
    []      -> error "Not enough arguments for Render"
    [x]     -> error "Not enough arguments for Render"
    [x,y]   -> renderFile x (dir ++ y) (outDir ++ p x ++ "/" ++ y)
    [x,y,z] -> renderFile x (dir ++ y) (outDir ++ p x ++ "/" ++ z)
    _       -> error "Too much arguments for Render"

renderFile :: Format -> FilePath -> FilePath -> IO ()
renderFile format file outputFile = do
  
  let dir = getPath outputFile
  doesIt <- doesDirectoryExist dir
  case doesIt of
    False -> createDirectory dir
    True  -> return ()
  
  preRendered <- preRenderFileForRendering format file
      
  evaled <- evalTrees format preRendered
  
  let preProcessForRendering =
        filter filterStructure
        . mapOnlyText (filterTree' (and . map isSpace))
        . mapOnlyTree (mapTree noSpacesAtBorders)
      
      generateTextResult = 
        (evalStructs format)
        . toLinearData
        . preProcessForRendering
  
  bodyResult <- reverseSpecialChars <$> generateTextResult evaled
  
  textResult <- wrapInFormat format bodyResult
  
  ext <- getExtension format
  
  writeFile (outputFile ++ "." ++ ext) textResult

wrapInFormat :: Format -> String -> IO String
wrapInFormat format text = do
  template <- templateDocument format
  return $ template ++ "\n\n" ++ text ++ "\n\n\\end{document}"

talkWithFormat :: String -> Format -> IO String
talkWithFormat headOrFoot format  = do
  let dir = "/home/.organon/Formats/Execs/"
      (progName:formatArgs) = divideWith '-' format
  doesIt <- doesFileExist (dir ++ progName)
  case doesIt of
    False -> error 
             $ "Format " 
             ++ progName
             ++ " does not exist in " 
             ++ dir ++ "."
    True -> do
      handles <- runInteractiveCommand 
                 $ dir ++ progName ++ " '" 
                 ++ headOrFoot ++ " " 
                 ++ intercalate " " formatArgs ++ "'"
      let (_,stdout,_,prog) = handles
      contents <- hGetContents stdout
      waitForProcess prog
      return contents

getExtension :: Format -> IO String
getExtension = talkWithFormat "extension"

templateDocument :: Format -> IO String
templateDocument = talkWithFormat "template"

{-headDocument :: Format -> IO String
headDocument = headOrFootDocument "head"

footDocument :: Format -> IO String
footDocument = headOrFootDocument "foot"-}


toLinearData :: FileStructure String -> LinearStructure String
toLinearData = map toLinear where
  toLinear (Struct    p a) = LStruct p a
  toLinear (Text [Leaf s]) = LText s

evalStructs :: Format -> LinearStructure String -> IO String
evalStructs format = 
  
  (intercalate "\n\n" <$>) <$> traverse evalStruct 

  where
    
    evalStruct (LStruct p a) = eval format (p ++ " " ++ a)
    evalStruct (LText     s) = return s

evalTrees :: Format -> FileStructure String -> IO (FileStructure String)
evalTrees format fil = do
  
  r <- traverseOnlyText evalTrees' fil
  return $ map joinLeaves r
  
  where 
    
    evalTrees' :: [Tree String] -> IO [Tree String]
    evalTrees' = traverse evalTree
        
    evalTree   (Leaf   x) = return $ Leaf x
    evalTree b@(Branch _) = lastDeBranch <$> evalMostTree format b
        
    joinLeaves :: Structure String -> Structure String
    joinLeaves id@(Struct  _ _) = id
    joinLeaves    (Text leaves) = 
          Text 
          [foldl concatLeaves (Leaf "") leaves]
    
    concatLeaves (Leaf x) (Leaf y) = Leaf (x ++ y)
    -- TODO : 

lastDeBranch (Branch [Leaf x]) = Leaf x
lastDeBranch _  = error "lastdebranch"

--------------------------------------------
        
evalLevel :: Format
             -> Int 
             -> Tree (Result String) 
             -> IO (Tree (Result String))
evalLevel format n = traverseWithLevel n eval' 
                     . putLevelInWholeTree
  where 
    
    eval' r = Result <$> case r of
      Argument x -> eval format x
      Result   x -> error $ x ++ "evallevel"
          
    traverseWithLevel :: Int -> (a -> IO a) -> Tree (a,Int) -> IO (Tree a)
    traverseWithLevel n f = traverse g where
      g (x,i) = if i == n
                then f x
                else return x

-- Transforma (Tree a) em (Tree (Result a))
fromTreeToEvalTree :: Tree a -> Tree (Result a)
fromTreeToEvalTree = mapTree Argument

-- Transforma (Tree (Result a)) em (Tree a)
fromEvalTreeToTree :: Tree (Result a) -> Tree a
fromEvalTreeToTree (Leaf (Result   x)) = error "Tree was not fully evaluated :("
fromEvalTreeToTree (Leaf (Argument x)) = Leaf x
fromEvalTreeToTree (Branch         xs) = Branch (map fromEvalTreeToTree xs)

evalMostTree :: Format -> Tree String -> IO (Tree String)
evalMostTree format tree = 
  
  fromEvalTreeToTree 
  <$> go (maxLevel tree) (fromTreeToEvalTree tree)
    
  where 
    
    go :: Int -> Tree (Result String) -> IO (Tree (Result String))
    go 0 t = return t
    go n t = do 
      t' <- evalLevel format n t
      let f = joinNeighbors concatResults . resultsAsArguments
      go (n-1) (f t')
    
    resultsAsArguments 
      :: Tree (Result String) -> Tree (Result String)
    resultsAsArguments (Branch [Leaf (Result x)]) 
      = Leaf (Argument x)
    resultsAsArguments (Branch xs) 
      = Branch $ map resultsAsArguments xs
    resultsAsArguments (Leaf (Result x)) = error x
    resultsAsArguments l@(Leaf (Argument x)) = l
    
    concatResults (Argument x) (Argument y) 
      = Argument (x ++ " " ++ y)
    concatResults x y 
      = error (show x ++ show y ++ "lalal")

eval :: Format -> String -> IO String
eval format s = do
  
  let execDir = "/home/.organon/Programs/Execs/"
      progWithArgs = noSpacesAtBorders s
      progName = head $ words progWithArgs
      progArgs = noSpacesAtBorders $ dropWhile (/=' ') progWithArgs
      specialChar = map changeChars
      changeChars x = case x of
        '\'' -> 'æ' -- altgr a
        -- '|'  -> 'ł' -- altgr l
        -- ';'  -> '©' -- altgr c
        c    -> c
  
  doesIt <- doesFileExist (execDir ++ progName)
  
  case doesIt of
    
    False -> error 
             $ "Program " 
             ++ progName 
             ++ " does not exist in " 
             ++ execDir
             
    True -> do
  
      handles <- runInteractiveCommand 
                 $ execDir ++ progName
                 ++ " '" 
                 ++ specialChar (format ++ " " ++ progArgs) 
                 ++ "'"
                 
      let (_,stdout,_,prog) = handles
      contents <- hGetContents stdout
      waitForProcess prog
      return (specialCharOutput contents) --init

specialCharOutput = map f where
  f c = case c of
    '|' -> 'ł'
    ';' -> '©'
    c -> c

reverseSpecialChars = map f where
  f c = case c of
    'ł' -> '|'
    '©' -> ';'
    c -> c

----------------------------------

tr fil out = renderFile "latex" fil out