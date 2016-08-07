module Tree where

import Data.Char

import Data
import Helper

-- Map
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = go where
  go (Leaf    x) = Leaf (f x)
  go (Branch xs) = Branch (map go xs)

-- Filter
filterTree :: Eq a => (a -> Bool) -> Tree a -> Tree a
filterTree pred = noRedundancy' . go where
  go b@(Branch xs) = let list = concat (map go xs)
                     in if list == [] 
                        then []
                        else [Branch list]
  go l@(Leaf    x) = if pred x then [] else [l]

filterTree' :: Eq a => (a -> Bool) -> [Tree a] -> [Tree a]
filterTree' pred = fromBranch . noRedundancy''' . go . Branch where
  go b@(Branch xs) = let list = concat (map go xs)
                     in if list == [] 
                        then []
                        else [Branch list]
  go l@(Leaf    x) = if pred x then [] else [l]

-- FoldGen : uma função para leafs outra para branches
foldTreeGen :: ([b] -> b) -> (a -> b) -> Tree a -> b
foldTreeGen fBranch fLeaf = go where
  go (Leaf    x) = fLeaf x
  go (Branch xs) = fBranch (map go xs)

foldTreeWithLevel :: (Int -> [b] -> b) -> (Int -> a -> b) -> Tree a -> b
foldTreeWithLevel fBranch fLeaf = go 0 where
  go i (Leaf    x) = fLeaf i x
  go i (Branch xs) = fBranch i (map (go(i+1)) xs)

-- Fold sem init
foldTree :: (a -> a -> a) -> Tree a -> a
foldTree f = go where
  go (Leaf    x) = x
  go (Branch xs) = foldl1 f (map go xs)

-- Fold com init
foldTree' :: (a -> a -> a) -> a -> Tree a -> a
foldTree' f init = go where
  go (Leaf    x) = x
  go (Branch xs) = foldl f init (map go xs)

-- Map com função que pega, além do valor, 
-- também a profundidade da arvore no momento
mapTreeWithLevel :: (Int -> a -> b) -> Tree a -> Tree b
mapTreeWithLevel f = go 0 where
  go i (Leaf    x) = Leaf $ f i x
  go i (Branch xs) = Branch $ map (go (i+1)) xs

mapTreeWithLevelIfAreJustLeaves :: (Int -> Bool -> a -> b) -> Tree a -> Tree b
mapTreeWithLevelIfAreJustLeaves f = go 0 True where
  go i can (Leaf    x) = Leaf $ f i can x
  go i can (Branch xs) = Branch $ map (go (i+1) areAllLeaves) xs
    where areAllLeaves = and $ map isLeaf xs
          isLeaf (Leaf   _) = True
          isLeaf (Branch _) = False

mapInLevel :: Int -> (a -> a) -> Tree a -> Tree a
mapInLevel l f = mapTreeWithLevel (\i x -> if i == l
                                           then f x
                                           else x)

mapInLevelM :: Monad m => Int -> (a -> m a) -> Tree a -> Tree (m a)
mapInLevelM l f = mapTreeWithLevel (\i x -> if i == l
                                           then f x
                                           else return x)

mapAboveLevelIfAreJustLeavesM :: Monad m 
                                 => Int 
                                 -> (a -> Bool -> m a) 
                                 -> Tree a 
                                 -> Tree (m a)
mapAboveLevelIfAreJustLeavesM l f = mapTreeWithLevelIfAreJustLeaves 
                                    (\i can x -> if i > l
                                                 then f x (not can)
                                                 else return x)

-- Join leaf neighbors with ( a -> a -> a )
joinNeighbors :: (a -> a -> a) -> Tree a -> Tree a
joinNeighbors f = noRedundancy . joinNeighbors' f
  where joinNeighbors' f (Leaf      x) = Leaf x
        joinNeighbors' f (Branch   []) = Branch []
        joinNeighbors' f (Branch  [x]) = Branch [joinNeighbors' f x]
        joinNeighbors' f (Branch list) = Branch (go list)
          where go []  = []
                go [Leaf    x] = [Leaf x]
                go [Branch ls] = br (go ls)
                go (x1:x2:xs) = case (x1,x2) of
                  (Leaf     x,    Leaf  y) -> go $ (Leaf (f x y)) : xs
                  (Branch  ls,    Leaf  x) -> br (go ls) ++ go (x2:xs)
                  (Leaf     x,  Branch ls) -> Leaf x : (br (go ls) ++ go xs)
                  (Branch ls1, Branch ls2) -> br (go ls1) ++ br (go ls2) ++ go xs
                br = (:[]) . Branch

stripOutEmptyTexts :: Tree String -> Tree String
stripOutEmptyTexts = filterTree (and . map isSpace)

-- Coloca um indicador de profundidade em cada dado da arvore
putLevelInWholeTree :: Tree a -> Tree (a,Int)
putLevelInWholeTree = mapTreeWithLevel (\i x -> (x,i))

maxLevel :: Tree a -> Int
maxLevel = foldTree max . mapTreeWithLevel (\i _ -> i)
