module Parsers where

import Variables
import Helper
import Data

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
import Data.List --(isPrefixOf,findIndices,union)
import System.FilePath.Find as F

-- read

readAndParse :: FilePath -> IO Blocks
readAndParse = (fromEither <$>) . parseFromFile readModeParser
  
  where
    
    readModeParser :: Parser Blocks
    readModeParser = do
  
      spaces
      blocks <- many block
  
      if blocks == []
        then error "There are no topics in file.a sd as d"
        else return blocks
         
    block :: Parser Block
    block = do
      
      tops <- topics
      cont <- content
      return (Block (tops,cont))
      
    topics = do
      
      string (replicate n '-')
      optional (many (char '-'))
      many (char ' ')
      
      topics <- many (noneOf " \n\r") `sepBy` (char ' ')
      
      return $ map (map toLower) $ filter (/="") topics

    content :: Parser Content
    content = 
      noSpacesAtBorders
      <$> concat 
      <$> many (choice [ try minuses
                       , try normalstrings
                       , superstrings
                       , text ])
      
    minuses = do
      char '-'
      a <- optionList (char '-')
      b <- noneOf "-"
      return ("-" ++ a ++ [b])
          
    normalstrings = do
      char '"'
      a <- optionList (char '"')
      b <- optionList (char '"')
      c <- noneOf ['"']
      return (['"'] ++ a ++ b ++ [c])
          
    superstrings = do
      string (replicate m '"')
      optional (many (char '"'))
      res <- manyTill
             (noneOf "")
             (do try (string (replicate m '"'))
                 optional (many (char '"')))
      return ("\"\"\"\"" ++ res ++ "\"\"\"\"")
      -- conservação dos """" para futuro parsing
      
    text = many1 (noneOf "\"-")
        
readDeParse :: Blocks -> String
readDeParse = intercalate "\n\n" . map deParseBlocks where
  deParseBlocks (Block (tops,"")) =
    "--- " ++ intercalate " " tops
  deParseBlocks (Block (tops,cont)) =
    "--- " ++ intercalate " " tops ++ "\n\n" ++ cont

-- PreRender

parseFromString :: String -> FileStructure String
parseFromString = fromEither . parse preRenderParser ""
  where fromEither (Left err) = error (show err)
        fromEither (Right a) = a
        
parseWithoutConservationFromString :: String -> FileStructure String
parseWithoutConservationFromString = fromEither 
                                     . parse preRenderParserWithoutConservation ""
  where fromEither (Left err) = error (show err)
        fromEither (Right a) = a


parseAndRead :: FilePath -> IO (FileStructure String)
parseAndRead = (fromEither <$>) . parseFromFile preRenderParser
    
preRenderParser :: Parser [Structure String]
preRenderParser = do
  
  spaces
  elements <- many element
  
  if elements == []
    then error "There are no structure in this file."
    else return elements
         
  where
    
    element :: Parser (Structure String)
    element = structureEval <|> (Text <$> textTree)
      
    structureEval :: Parser (Structure String)
    structureEval = do
      
      string (replicate n '-')
      optional (many (char '-'))
      many (char ' ')
      
      structureName <- many (noneOf " \n\r")
      structureArguments <- many (noneOf "\n\r")
      
      return $ Struct structureName (noSpacesAtBorders structureArguments)

    textTree :: Parser [Tree String]
    textTree = many1 (choice [ try minuses
                             , try normalstrings
                             , superstrings
                             , mathLatex
                             , text
                             , branch ])
                
    minuses = Leaf <$> do
      char '-'
      a <- optionList (char '-')
      b <- noneOf "-"
      return ("-" ++ a ++ [b])
          
    normalstrings = Leaf <$> do
      char '"'
      a <- optionList (char '"')
      b <- optionList (char '"')
      c <- noneOf ['"']
      return (['"'] ++ a ++ b ++ [c])
          
    superstrings = Leaf <$> do
      string (replicate m '"')
      optional (many (char '"'))
      res <- manyTill 
             (noneOf "")
              (do try (string (replicate m '"'))
                  optional (many (char '"')))
      return $ "\"\"\"\"" ++ res ++ "\"\"\"\""
    
    mathLatex = Leaf <$> do
      char '$'
      o <- optionList (char '$')
      a <- manyExcept "$"
      char '$'
      c <- optionList (char '$')
      case (o,c) of
        ([],  []) -> return ("$"  ++ a ++ "$")
        ([], [x]) -> error "You forgot one initial $"
        ([x], []) -> error "You need to close the $"
        _ -> return ("$$" ++ a ++ "$$")
        
    branch = Branch <$> 
             between 
             ( char openSym )  
             ( char closeSym ) 
             ( textTree )
        
    text = Leaf
           <$> many1Except 
           [ openSym
           , closeSym
--           , escapeSym
           , mathSym
--           , escapeRegionSym
           , '"'
           , '-' ]
           
preRenderParserWithoutConservation :: Parser [Structure String]
preRenderParserWithoutConservation = do
  
  spaces
  elements <- many element
  
  if elements == []
    then error "There are no structure in this file."
    else return elements
         
  where
    
    element :: Parser (Structure String)
    element = structureEval <|> (Text <$> textTree)
      
    structureEval :: Parser (Structure String)
    structureEval = do
      
      string (replicate n '-')
      optional (many (char '-'))
      many (char ' ')
      
      structureName <- many (noneOf " \n\r")
      structureArguments <- many (noneOf "\n\r")
      
      return $ Struct structureName (noSpacesAtBorders structureArguments)

    textTree :: Parser [Tree String]
    textTree = many1 (choice [ try minuses
                             , try normalstrings
                             , superstrings
                             , mathLatex
                             , text
                             , branch ])
                
    minuses = Leaf <$> do
      char '-'
      a <- optionList (char '-')
      b <- noneOf "-"
      return ("-" ++ a ++ [b])
          
    normalstrings = Leaf <$> do
      char '"'
      a <- optionList (char '"')
      b <- optionList (char '"')
      c <- noneOf ['"']
      return (['"'] ++ a ++ b ++ [c])
          
    superstrings = Leaf <$> do
      string (replicate m '"')
      optional (many (char '"'))
      res <- manyTill 
             (noneOf "")
              (do try (string (replicate m '"'))
                  optional (many (char '"')))
      return $ noSpacesAtBorders res
      
    mathLatex = Leaf <$> do
      char '$'
      o <- optionList (char '$')
      a <- manyExcept "$"
      char '$'
      c <- optionList (char '$')
      case (o,c) of
        ([],  []) -> return ("$"  ++ noSpacesAtBorders a ++ "$")
        ([], [x]) -> error "You forgot one initial $"
        ([x], []) -> error "You need to close the $"
        _ -> return ("$$" ++ noSpacesAtBorders a ++ "$$")
        
    branch = Branch <$> 
             between 
             ( char openSym )  
             ( char closeSym ) 
             ( textTree )
        
    text = Leaf
           <$> many1Except 
           [ openSym
           , closeSym
--           , escapeSym
           , mathSym
--           , escapeRegionSym
           , '"'
           , '-' ]

--------------------

data Insert = Insert FilePath | Str String deriving Show

evalInsertFile :: FilePath -> IO String
evalInsertFile filepath = do
  insertResult <- fromEither <$> parseFromFile evalInsert filepath
  strings <- mapM toString insertResult
  return $ concat strings
  where toString :: Insert -> IO String
        toString (Str     str) = return str
        toString (Insert file) = do
          contents <- Strict.run $ Strict.readFile file
          return (noSpacesAtBorders contents)

evalInsert :: Parser [Insert]
evalInsert = many1 (try minuses <|> insert <|> text)
  where 
    
    text = Str <$> many1 (noneOf "-")
        
    minuses = Str <$> do
      char '-'
      a <- optionList (char '-')
      b <- noneOf "-"
      return ("-" ++ a ++ [b])
          
    insert = do
          
      string (replicate n '-')
      a <- head <$> optionList (many (char '-'))
      many (char ' ')          
          
      name <- many (noneOf " \n\r")
      body <- many (noneOf "\n\r")
          
      if map toLower name == "insertfile"
        then return $ Insert $ noSpacesAtBorders body
        else return $ Str    $ "---" ++ a ++ " " ++ name ++ body
                 
parse' :: String -> [Insert]
parse' = fromEither . parse evalInsert ""
  where fromEither (Left err) = error (show err)
        fromEither (Right a) = a
