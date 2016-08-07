module Get where

import Data.Char
import Data.List hiding (find)

import Data
import Helper
import Find

evalGetTemplate :: (Topics -> IO [(Topics, [(ID, Content)])])
                   -> Format
                   -> [String]
                   -> IO (Result String)
evalGetTemplate findF format (keyword:topics) = do
      
  blocks <- findF topics
      
  errorIfThereAreNoBlocks blocks topics
      
  return $ Result $ case map toLower keyword of
        
    "sec"       -> section (toFormat1 blocks)
    "sec1"      -> section1 (toFormat1 blocks)
    
    "sub"       -> subsection (toFormat1 blocks)
    "sub1"      -> subsection1 (toFormat1 blocks)
    
    "item-sec"  -> itemsSec (toFormat1 blocks)
    "item-sub"  -> itemsSub (toFormat1 blocks)
    "item-sec1" -> itemsSec1 (toFormat1 blocks)
    "item-sub1" -> itemsSub1 (toFormat1 blocks)
    
    "item"      -> item (toFormat2 blocks)
    "text"      -> text (toFormat2 blocks)
    
    s -> error $ "wtf is " ++ s ++ " ???"

evalGetID :: Format -> [String] -> IO (Result String)
evalGetID format (keyword:ids) = do
          
  blocks <- findID (map read ids)
      
  errorIfThereAreNoBlocks blocks (map show ids)
      
  return $ Result $ case map toLower keyword of
    
    "sec"  -> section (toFormat3 blocks)
    "sec1" -> section1 (toFormat3 blocks)
    
    "sub"  -> subsection (toFormat3 blocks)
    "sub1" -> subsection1 (toFormat3 blocks)
    
    "desc" -> desc (toFormat4 blocks)
    "par"  -> par (toFormat4 blocks)
    
    "item" -> item (toFormat5 blocks)
    "text" -> text (toFormat5 blocks)
    
    s -> error $ "wtf is " ++ s ++ " ???"

evalGet :: Format -> [String] -> IO (Result String)
evalGet = evalGetTemplate find

evalGetAll :: Format -> [String] -> IO (Result String)
evalGetAll = evalGetTemplate findAll    

-------

--- all -> item-sec e sec
toFormat1 :: [(Topics,[(ID,Content)])] -> [(Topics,[Content])]
toFormat1 = map (\(a,b) -> (a,map snd b))

-- all -> item e text
toFormat2 :: [(Topics,[(ID,Content)])] -> [Content]
toFormat2 = concat . map (\(_,b) -> map snd b)

--- id -> item-sec e sec
toFormat3 :: [(Topics,ID,Content)] -> [(Topics,[Content])]
toFormat3 = map (\(a,_,c) -> (a,[c]))

-- id -> par e desc
toFormat4 :: [(Topics,ID,Content)] -> [(Topics,Content)] 
toFormat4 = map (\(a,_,c) -> (a,c))

-- id -> item e text
toFormat5 :: [(Topics,ID,Content)] -> [Content]
toFormat5 = map (\(_,_,c) -> c)

-------

sectionTemplate :: String -> [(Topics,[Content])] -> String
sectionTemplate sec = intercalate "\n\n" . map showBlocks 
  
  where
    
    showBlocks (ts,conts) =
      "{ " ++ sec ++ " " ++ sayList' ts ++ " }\n\n"
      ++ intercalate "\n\n" conts
                  
itemSecTemplate :: String -> [(Topics,[Content])] -> String
itemSecTemplate sec = intercalate "\n\n" . map showBlocks 
  
  where
    
    showBlocks (ts,conts) =
      "{ " ++ sec ++ " " ++ sayList' ts ++ " }\n\n"
      ++ showCont conts
                  
    showCont = putWrap . intercalate "\n\n| "
    
    putWrap s = "{ item \n\n" ++ s ++ " }"

section :: [(Topics,[Content])] -> String
section = sectionTemplate "section"

section1 :: [(Topics,[Content])] -> String
section1 = sectionTemplate "section1"

subsection :: [(Topics,[Content])] -> String
subsection = sectionTemplate "subsection"

subsection1 :: [(Topics,[Content])] -> String
subsection1 = sectionTemplate "subsection1"

itemsSec :: [(Topics,[Content])] -> String
itemsSec = itemSecTemplate "section"

itemsSec1 :: [(Topics,[Content])] -> String
itemsSec1 = itemSecTemplate "section1"

itemsSub :: [(Topics,[Content])] -> String
itemsSub = itemSecTemplate "subsection"

itemsSub1 :: [(Topics,[Content])] -> String
itemsSub1 = itemSecTemplate "subsection1"

item :: [Content] -> String
item = putWrap . intercalate "\n\n| "
  where putWrap s = "{ item \n\n" ++ s ++ " }"

text :: [Content] -> String
text = intercalate "\n\n"

desc :: [(Topics,Content)] -> String
desc = putWrap . intercalate "\n\n| ". map makeItem
  where makeItem (ts,cs) = sayList' ts ++ " ; " ++ cs
        putWrap s = "{ desc \n\n" ++ s ++ " }"

par :: [(Topics,Content)] -> String
par = intercalate "\n\n" 
      . map (\(ts,cs) -> 
              "{ par " ++ sayList' ts ++ " |\n " ++ cs ++ " }")