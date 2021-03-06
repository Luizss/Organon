module PreRender where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile,sequence)
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
import Data.List hiding (find)

import Data
import Helper
import Tree
import Variables
import Parsers
import Get

{- Top Level -}

{-preRender :: [String] -> IO ()
preRender args = do
  
  let dir    = "./Texts/"
      
  errorIfDirectoryDoesNotExists dir
  
  case args of
    
    []      -> error "Not enough arguments for PreRender"
    [x]     -> error "Not enough arguments for PreRender"
    [x,y]   -> preRenderFile x (dir ++ y) (dir ++ y ++ "PRE")
    [x,y,z] -> if y /= z 
               then preRenderFile x (dir ++ y) (dir ++ z)
               else error "Input File == Output File "
    _       -> error "Too much arguments for PreRender"-}

preRender :: [String] -> IO ()
preRender args = do
  
  let dir    = "./Texts/"
      
  errorIfDirectoryDoesNotExists dir
  
  case args of
    
    []    -> error "Not enough arguments for PreRender"
    [x]   -> preRenderFile "article" (dir ++ x) (dir ++ x ++ "PRE")
    [x,y] -> if x /= y
             then preRenderFile "article" (dir ++ x) (dir ++ y)
             else error "Input File == Output File "
    _     -> error "Too much arguments for PreRender"
    
-- Função para comando prerender
preRenderFile :: String -> FilePath -> FilePath -> IO ()
preRenderFile format file outputFile = do
  
  fileStructRes <- preRenderFileStructure format file
  
  let showPreRender = deParseBeautifullyFS . preProcessPreRender
  
  writeFile outputFile (showPreRender fileStructRes)

-- função para comando render
preRenderFileForRendering :: String -> FilePath -> IO (FileStructure String)
preRenderFileForRendering format file = do
  
  preProcess 
  <$> parseWithoutConservationFromString
  <$> deParseFS 
  <$> preRenderFileStructure format file

{- Pre RenderFile Structure -}

preRenderFileStructure :: Format -> FilePath -> IO (FileStructure String)
preRenderFileStructure format file = do

  res <- evalInsertFile file 
  let fileStruct = parseFromString res
    
  let fileStruct' = preProcess fileStruct
      
  evalAllGets format fileStruct'


preProcess :: FileStructure String -> FileStructure String
preProcess = 
  
  map translatorToLower
  . filter filterStructure
  . mapOnlyTree (joinNeighbors (++))
  
evalAllGets :: Format -> FileStructure String -> IO (FileStructure String)
evalAllGets format = go where
  
  go fileS 
  
    | thereAreGetsIn fileS = do
      
      res <- evalGets format fileS
      let text    = deParse res
          parsed  = parseFromString text
          fileS'  = preProcess parsed
      go fileS'
      
    | otherwise = return fileS                   
                     
  thereAreGetsIn = or . map areThereGets
        
  areThereGets (Struct _ _) = False
  areThereGets (Text trees) = 
    or $ map (foldTree (||) . mapTree hasGet) trees
          
  hasGet s
    |"get"    `isPrefixOf` s = True
    | otherwise              = False     

deParse :: FileStructure (Result String) -> String
deParse = 
  
  noSpacesAtBorders 
  . concat
  . map deParseStructure
  
  where 
    
    deParseStructure (Struct a s) 
      = "--- " ++ a ++ " " ++ s ++ "\n\n"
    deParseStructure (Text trees) 
      = concat $ map tree2string trees
        
    tree2string (Branch [Leaf (Result   s)]) = s
    tree2string (Branch ss@((Leaf (Argument s)):xs))
      = "{" ++ (intercalate "" $ map tree2string ss) ++ "}"
    tree2string (Leaf (Result   s)) = s    
    tree2string (Leaf (Argument s)) = s

deParseFS :: FileStructure String -> String
deParseFS = deParse . insertArguments

evalGets :: Format -> FileStructure String -> IO (FileStructure (Result String))
evalGets format = 
  
  traverse findGets 
  . everyStringAsArgument
  
  where 
    
    everyStringAsArgument = fmap (fmap Argument)
    
    findGets id@(Struct a s) = return id
    findGets    (Text trees) = do
      treeRes <- sequence $ 
                 mapAboveLevelIfAreJustLeavesM 
                 1
                 applyIfIsGet 
                 (Branch trees)
      return $ Text $ fromBranch treeRes
      
    applyIfIsGet :: Result String -> Bool -> IO (Result String)
    applyIfIsGet (Argument s) getHasBranches
    
      | "getAll" `isPrefixOf` s = do errorIfGetHasBranches
                                     evalGetAll format (words' s)
                                     
      | "getID"  `isPrefixOf` s = do errorIfGetHasBranches
                                     evalGetID format (words' s)
                                     
      | "get"    `isPrefixOf` s = do errorIfGetHasBranches
                                     evalGet format (words' s)
                                     
      | otherwise               = do return (Argument s)
        
      where errorIfGetHasBranches 
              | getHasBranches = error "GET HAS BRANCHES!!!"
              | otherwise      = return ()
        
    words' :: String -> [String]
    words' = tail . words
    
{- New -}
    
deParseBeautifully :: FileStructure (Result String) -> String
deParseBeautifully = 
  
  noSpacesAtBorders 
  . intercalate "\n\n"
  . map deParseStructure
  
  where 
    
    deParseStructure (Struct a s) = 
      "--- " ++ a ++ " " ++ s
    deParseStructure (Text trees) = concat $ map tree2string trees
    
    tree2string (Branch    [Leaf (Result   s)]) = s
    tree2string (Branch ss@((Leaf (Argument s)):xs))
    
       {-| "section" `isPrefixOf` (noSpacesAtHead s) = 
         "{" ++ s ++ "\n\n"
         ++ (intercalate "" $ map tree2string xs) 
         ++ "}"
        
      | "subsection" `isPrefixOf` (noSpacesAtHead s) = 
        "{" ++ s ++ ""
        ++ (intercalate "" $ map tree2string xs) 
        ++ "\n\n}\n\n"
        
      | otherwise =
        "{" ++ (intercalate " " $ map tree2string ss) ++ "}"-}
      
      | otherwise =
        "{" ++ (intercalate " " $ map tree2string ss) ++ "}"
    
    tree2string (Leaf (Result   s)) = s    
    tree2string (Leaf (Argument s)) = s

deParseBeautifullyFS :: FileStructure String -> String
deParseBeautifullyFS = deParseBeautifully . insertArguments

preProcessPreRender :: FileStructure String -> FileStructure String
preProcessPreRender =   
  
    filter filterStructure
  . mapOnlyText filterEmptyTextsIf
  . scanStructure
  . mapOnlyTree (joinNeighbors (++))
  
  where 
    
    filterEmptyTextsIf  [] = []
    
    filterEmptyTextsIf [a] = 
      filterTree' (and . map isSpace) [a]
    
    filterEmptyTextsIf [a,b] = 
      filterTree' (and . map isSpace) [a,b]
    
    filterEmptyTextsIf (Branch xs : Leaf s : Branch ys : rs)
      = Branch xs : Leaf s : filterEmptyTextsIf (Branch ys : rs)
        
    filterEmptyTextsIf (a : b : c : rs)
      = filterTree' (and . map isSpace) [a]
        ++ filterEmptyTextsIf (b:c:rs)
    
    scanStructure (Text s : Struct p a : xs) = 
      
      Text (noSpacesAtEnd' s) : 
      scanStructure (Struct p a : xs)
      
    scanStructure (Struct p a : Text s : xs) = 
      
      Struct p a : 
      scanStructure (Text (noSpacesAtHead' s) : scanStructure xs)
      
    scanStructure xs = xs
    
    noSpacesAtEnd' [] = []
    noSpacesAtEnd' xs = case last xs of
      Leaf x -> init xs ++ [Leaf (noSpacesAtEnd x)]
      branch -> xs
    
    noSpacesAtHead' [] = []
    noSpacesAtHead' (Leaf x:xs) = Leaf (noSpacesAtHead  x) : xs
    noSpacesAtHead' xs = xs

{- Teste -}
            
preTest file output = preRenderFile "latex" file output

