module Main where

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
import System.IO
import Data.List

import Read
import PreRender
import Find
import Render
import ToClass
import FindCommands
import ReplaceRemove
import MakePDF
import Check
import Doc

main  = do
  
  args <- getArgs
  let (command,arguments) = parseArgs args
  applyCommand command arguments
  
  where
    
    parseArgs []   = ("help"   ,        [])
    parseArgs [x]  = (x        ,        [])
    parseArgs list = (head list, tail list)

applyCommand comm args = case map toLower comm of
  
  "help"      -> helpMessage
  "init"      -> initOrganon
  
  "read"      -> readComm args
  "prerender" -> preRender args
  "render"    -> render args
  
  "class"     -> toClass args
  "topic"     -> toTopic args
  
  "find"      -> findComm args
  "findall"   -> findAllComm args
  "findid"    -> findIdComm args
  "get"       -> getComm args
  "getall"    -> getAllComm args
  "getid"     -> getIdComm args
  
  "replace"   -> replace args
  "remove"    -> remove args
  "addtag"    -> addTag args
  "removetag" -> removeTag args
    
  "makepdf"   -> makePDFs args
  
  "check"     -> checkTopicsCompilation args
  "checkt"    -> checkTopics args
  "checks"    -> checkStructure args
  
  "doc"       -> documentation args
  "use"       -> documentationUse args
  "docf"      -> documentationFormat args
  
  _           -> error "Command not found"

-- help
helpMessage :: IO ()
helpMessage = 
  putStrLn 
  $ intercalate "\n" 
  [ "organon [COMMAND] [ARGUMENTS]"
  , ""
  , "COMMANDS:"
  , ""
  , "init => cria o ambiente organon com todos os arquivos e pastas necessários"
  , "read [files on /Topics] => put topics file into /Knowledge in a organized way"
  , "prerender [file on /Texts] => result in /Texts with a PRE tag"
  , "prerender [file on /Texts] [output file name] => result in /Texts/[output file name]"
  , "render [format] [file on /Texts] => result in /Results/[file name]"
  , "render [format] [file on /Texts] [output file name] => result in /Results/[output file name]"
  , "class [master topic] [topics inside] => make a normal topic a class topic"
  , "topic [topics] => transform all class topics in normal topics"
  , "find [subjects] => find topics with all of the subjects"
  , "findall [subjects] => find topics with any of the subjects"
  , "findid [id's] => find topics from ID's"
  , "get [subjects] => find topics with all of the subjects and show pdf"
  , "getall [subjects] => find topics with any of the subjects and show pdf"
  , "getid [id's] => find topics from ID's and show pdf"
  , "replace [file on /Texts] => file with ids and new text will replace the original texts with the ids"
  , "remove [ids] => remove topics with the respective ids"
  , "addtag [tag] [ids] => add a tag to topics with ids"
  , "removetag [tag] [ids] => remove a tag to topics with ids"
  , "makepdf [files on /Results] => results on /Results/PDFs"
  , "check [files on /Topics] => check a topics file compilation"
  , "checkt [files on /Topics] => check a topics file showing its PDF"
  , "checks [files on /Texts] => check a topics file showing its PDF"
  , "doc [progs] => documentation for programs"
  , "use [progs] => use cases for programs"
  , "docf [formats] => documentation for formats"
  ]
  
initOrganon :: IO ()
initOrganon = do
  
  --createDirectoryIfMissing' "~/.organon/Programs/Execs"
  --createDirectoryIfMissing' "~/.organon/Formats/Execs"
    
  createDirectoryIfMissing' "./External/Images"
  createDirectoryIfMissing' "./Knowledge"
  createDirectoryIfMissing' "./Texts"
  createDirectoryIfMissing' "./Topics"
  createDirectoryIfMissing' "./Results/PDFs"
  createDirectoryIfMissing' "./Results/Temp"
  createDirectoryIfMissing' "./Documentation"
  
  safelyCreateFile "./Documentation/formats" formatsText
  safelyCreateFile "./Documentation/programs" programsText
  safelyCreateFile "./help" helpText
  
  where
      
    createDirectoryIfMissing' = createDirectoryIfMissing True
            
    safelyCreateFile :: String -> String -> IO ()
    safelyCreateFile s c = do
      doesIt <- doesFileExist s
      if doesIt
        then return ()
        else writeFile s c
             
helpText = 
  intercalate "\n" 
  ["Organization of Folders",
   "",
   "~/.organon/Programs/Execs -> Executables *",
   "~/.organon/Programs       -> Source Codes",
   "~/.organon/Formats/Execs  -> Executables Formats *",
   "~/.organon/Formats        -> Source Codes Formats",
   "",
   "./External/Images -> Images",
   "./External        -> Anything external needed to render file",
   "./Knowledge       -> Organized topics",
   "./Texts           -> Structure files (for prerender or render) *",
   "./Topics          -> Topic files (for read) *",
   "./Results/PDFs    -> PDF results",
   "./Results         -> Results from render *",
   "./Documentation   -> Documentation for programs and formats",
   "",
   "* -> Program expects the right things to be in the specific places."]
  
programsText =
  intercalate "\n"
  [ "@program:"
  , ""
  , "(program <arg1> | <arg2>)"
  , "(program <arg1> | <arg2> | <arg3>)"
  , ""
  , "This program uses arguments to"
  , "obtain a result."]
  
formatsText =
  intercalate "\n"
  [ "@format:"
  , ""
  , "(program1,program2,program3,"
  ," program4,program5)"
  , ""
  , "This format is important"
  , "because it is."]