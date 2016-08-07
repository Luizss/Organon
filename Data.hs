{-# LANGUAGE 
DeriveFunctor,
DeriveTraversable,
DeriveFoldable,
DeriveDataTypeable #-}

module Data where

import Data.Traversable
import Data.Foldable

data Tree a = Branch [Tree a] | Leaf a
            deriving (Show,Functor,Eq,Traversable,Foldable)

type Translator = String
type  Arguments = String
type Program    = String

data Structure a = Struct Translator Arguments
                 | Text [Tree a]
                 deriving (Show,Eq,Functor,Traversable,Foldable)
                        
type FileStructure a = [Structure a]

data Linear a = LStruct Translator Arguments
              | LText a
              deriving (Show,Eq,Functor,Traversable,Foldable)

type LinearStructure a = [Linear a]

data Result a = Argument a
              | Result a 
              deriving (Show,Eq)
type    Topic = String

type   Topics = [Topic]
type  Content = String

newtype Block = Block (Topics, Content) deriving (Show,Eq)

type   Blocks = [Block]

type Format = String

data Configurations = Configurations {
  getPrograms  :: [ProgramInfo],
  getConfigs   :: [ConfigInfo],
  getKnowledge :: [KnowledgeInfo]
  } deriving Show

data ProgramInfo = ProgramInfo {
  programName :: String,
  programType :: ProgramType
  } deriving Show

data ProgramType = EvalInTheEnd 
                 | Inline 
                 deriving Show

data ConfigInfo = ConfigInfo {
  configName :: String
  } deriving Show

data KnowledgeInfo = KnowledgeInfo {
  topicName :: String,
  topicType :: TopicType
  } deriving Show

data TopicType = C [String] 
               | T 
               deriving Show

newtype TIC = TIC (Topics,Integer,Content) deriving Show

instance Eq TIC where
  TIC (_,i,_) == TIC (_,j,_) = i == j

instance Ord TIC where
  TIC (_,i,_) <= TIC (_,j,_) = i <= j

type ID = Integer