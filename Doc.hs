module Doc where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Data.Either
import Prelude hiding (readFile)
import Control.Applicative ((<$>),(<*>),Applicative)
import Data.Char
import Text.Printf
import Data.Traversable
import Data.Foldable hiding (concat,foldl1,foldl,and,mapM_,find)
import System.Console.Haskeline
import System.Console.CmdArgs
import qualified System.IO.Strict as Strict
import System.Directory
import System.Environment
import System.Process
import System.IO
import Data.List

import Helper

--documentationUse = mapM documentationUse1

type Prog  = String
type Use   = [String]
type Doc   = String

type Bloc  = (Prog,Use,Doc)
type Blocs = [Bloc]

documentationTemplate :: 
  (FilePath -> IO Blocs) 
  -> (Bloc -> String) 
  -> FilePath 
  -> Prog 
  -> IO ()
documentationTemplate parseDoc showF file prog = do
  errorIfFileDoesNotExists file 
    ("There is no " ++ file ++ ". Make sure to use 'init'.")
  parsed <- parseDoc file
  let result = findProg prog parsed
  case result of
    Nothing -> 
      putStrLn $ "'" ++ prog ++ "' does not contain documentation."
    Just rs -> putStrLn $ showF rs

documentationTemplate' a b c d = 
  documentationTemplate a b c d >> putStr "\n"

documentationUse :: [Prog] -> IO ()                    
documentationUse [x] 
  = documentationTemplate
    parseDoc
    showUse
    "./Documentation/programs" x
documentationUse xs = do
  mapM_ (documentationTemplate'
         parseDoc
         showUse
         "./Documentation/programs") (init xs)
  documentationUse [last xs]

documentation [x] =
  documentationTemplate
  parseDoc
  (showDoc "Uses")
  "./Documentation/programs" x
documentation xs = do
  mapM_ (documentationTemplate'
         parseDoc
         (showDoc "Uses")
         "./Documentation/programs") (init xs)
  documentation [last xs]

documentationFormat [x] =
  documentationTemplate
  parseDocFmt
  (showDoc "Programs")
  "./Documentation/formats" x
documentationFormat xs = do
  mapM_ (documentationTemplate'
         parseDocFmt
         (showDoc "Programs")
         "./Documentation/formats") (init xs)
  documentationFormat [last xs]

showUse :: Bloc -> String
showUse (prog,uses,_) = 
  intercalate "\n" 
  $ ("Uses for " ++ prog ++ ":") : uses

showDoc :: String -> Bloc -> String
showDoc txt (prog,uses,body) = 
  intercalate "\n" $ 
  [prog ++ ":" 
  , ""
  , txt ++ ":"
  , "" ]
  ++ map putChar uses ++
  [ "" , body ]
  where putChar s = "=> " ++ s
  
findProg :: Prog -> Blocs -> Maybe Bloc
findProg prog = find (\(p,_,_) -> p == prog)

----

parseDoc :: FilePath -> IO Blocs
parseDoc = (fromEither <$>) . parseFromFile docParser

parseDocFmt :: FilePath -> IO Blocs
parseDocFmt = (fromEither <$>) . parseFromFile docFmtParser

----

docParser :: Parser Blocs
docParser = do
  
  spaces
  blocs <- many (bloc use)
  
  if blocs == []
    then error "There are no doc in doc file."
    else return blocs
         
  where
    
    use :: Parser [String]
    use = many $ do
  
      txt <- between (char '(') (char ')') (many (noneOf ")"))
      spaces
      return txt


docFmtParser :: Parser Blocs
docFmtParser = do
  
  spaces
  blocs <- many (bloc use)
  
  if blocs == []
    then error "There are no doc in doc file."
    else return blocs
         
  where
    
    use :: Parser [String]
    use = do
      between (char '(') (char ')')
        (many (noneOf ",) ") `sepBy` (char ','))

----

bloc :: Parser [String] -> Parser Bloc
bloc use = do
  
  spaces
  char '@'
  progName <- many1 (noneOf ":")
  char ':'
  spaces
  use' <- use
  spaces
  body <- many (noneOf "@")
  return (fProg progName, fUse use', fBody body)
  
  where fProg = noSpacesAtBorders
        fUse  = map noSpacesAtBorders
        fBody = noSpacesAtBorders

-----
  
test = parse docParser "" $ 
       intercalate "\n" $
       [ ""
       , "@bloc:"
       , "(itsitsitsitistististist)" 
       , "(asdsasfdasfasfasfa)"
       , "asd asfsd foi sdf sdfh kfd "
       , "asdasfasdfasdfasfasdf"
       , "asdadfasdfasdf"
       , "" ]
       
       ++
       
       [ ""
       , "@bloc:"
       , "(itsitsitsitistististist)" 
       , "(asdsasfdasfasfasfa)"
       , "asd asfsd foi sdf sdfh kfd "
       , "asdasfasdfasdfasfasdf"
       , "asdadfasdfasdf"
       , "" ]
       
test2 = parse docFmtParser "" $ 
       intercalate "\n" $
       [ ""
       , "@bloc:"
       , "(itsitsitsitistististist)" 
       , "(asdsasfdasfasfasfa)"
       , "asd asfsd foi sdf sdfh kfd "
       , "asdasfasdfasdfasfasdf"
       , "asdadfasdfasdf"
       , "" ]
       
       ++
       
       [ ""
       , "@bloc:"
       , "(asduansf,asfuasndf,asdfasf-asdfas,sdfasd-adfasdf,sdf asdfa sdf asd fa sdf,asd fa sd gas gd)" 
       , "(asdsasfdasfasfasfa)"
       , "asd asfsd foi sdf sdfh kfd "
       , "asdasfasdfasdfasfasdf"
       , "asdadfasdfasdf"
       , "" ]