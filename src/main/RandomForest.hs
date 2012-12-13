{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RandomForest where

import DecisionTreeUtils
import DataSet
import Data.List
import System.Random

type RandomForest = [DecisionTree]

-- classify :: RandomForest -> Observation -> Bool
instance Classifier RandomForest where
  classify rf obs = 
    let classifications = map (\tree -> (classify tree) obs) rf
    in if (length $ filter id classifications) > (length $ filter not classifications)
       then True else False

-- train a random forest of n trees that look at f randomly chosen features 
-- at each split when training the trees
trainRIForest :: DataSet -> Int -> Int -> IO RandomForest
trainRIForest training n f = 
  do seeds <- getSeeds n
     let gs = map mkStdGen seeds
     return $ map (\g -> learnRandomInputTree training g f) gs

getSeeds :: Int -> IO [Int]
getSeeds n = mapM (\x -> randomIO :: IO Int) [1..n]


-- learn a random input tree looking at n randomly selected features at each 
-- split
learnRandomInputTree :: DataSet -> StdGen -> Int -> DecisionTree
learnRandomInputTree training g n = snd $ learnRI g n training []


-- this is more complicated than the basic CART methodology because we need to 
-- keep a StdGen as well
learnRI :: StdGen -> Int -> DataSet -> [Int] -> (StdGen, DecisionTree)
-- this empty training set case comes up when the training set isn't splittable
learnRI g _ [] _ = let (randomBool, g') = random g
                   in if randomBool then (g', TrueLeaf) else (g', FalseLeaf)
learnRI g n training usedIndices = 
  let ys = getClassifications training
      allSameClass = (all id ys) || (all not ys)
  in if allSameClass -- don't need to split anymore
     then if (head ys) then (g, TrueLeaf) else (g, FalseLeaf)
     else let allIndicesUsed = (length usedIndices) == (length (features (head training)))
          in if allIndicesUsed -- count the number of classifications b/c we can't split anymore
             then if ((length $ filter id ys) > (length $ filter not ys)) 
                  then (g, TrueLeaf) else (g, FalseLeaf)
             --here is where we actually split
             else let (g', (idx, cutoff, gain)) = randomSplit g training usedIndices n
                      leftTrain = filter (\obs -> (features obs) !! idx < cutoff) training
                      rightTrain = filter (\obs -> (features obs) !! idx >= cutoff) training
                      (h', leftTree) = learnRI g' n leftTrain (usedIndices ++ [idx])
                      (k', rightTree) = learnRI h' n rightTrain (usedIndices ++ [idx])
                  in (k', DecisionTree idx cutoff leftTree rightTree)


-- This function picks the best split from n randomly chosen attributes
-- and returns the column index, numerical cutoff and gain.
-- Note that this assumes that all columns are continuous variables.
randomSplit :: StdGen -> DataSet -> [Int] -> Int -> (StdGen, (Int, Double, Double))
randomSplit g training usedIndices n = 
  let numColumns = length $ features (head training)
      unusedIndices = filter (flip notElem usedIndices) [0..(numColumns - 1)]
      (g', randomIndices) = getRandomIndices g n unusedIndices
  in (g', getBestSplit training $ filter (flip notElem randomIndices) [0..(numColumns -1)])


-- this part has a bunch of book-keeping with the StdGen's because we need to make
-- sure that we don't ever re-use one, which would be really bad
getRandomIndices :: StdGen -> Int -> [Int] -> (StdGen, [Int])
getRandomIndices g n xs = let pairs = pickRandomElements g n xs
                          in (fst $ last pairs, map snd pairs)

pickRandomElements :: StdGen -> Int -> [Int] -> [(StdGen, Int)]
pickRandomElements g 0 _ = []
pickRandomElements g n xs = 
  if n >= length xs then zip (repeat g) xs 
  else let (i, g') = randomR (0, length xs - 1) g
       in (g',(xs !! i)) : pickRandomElements g' (n-1) ((take i xs) ++ (drop (i+1) xs))
