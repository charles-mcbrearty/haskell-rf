module CART where
-- this module includes the functionality necessary to use the
-- CART algorithm to train a decision tree

import Data.List
import DataSet
import DecisionTreeUtils

-- this is the exposed function that works on continuous features
learnDecisionTree :: DataSet -> DecisionTree
learnDecisionTree training = learn training []

-- need to keep track of the used attributes internally
learn :: DataSet -> [Int] -> DecisionTree
learn [] _ = TrueLeaf
-- the empty training set case can come up with an inconsistent training set.
learn training usedIndices = 
  let ys = getClassifications training
      allSameClass = (all id ys) || (all not ys)
  in if allSameClass -- don't need to split anymore
     then if (head ys) then TrueLeaf else FalseLeaf
     else let allIndicesUsed = (length usedIndices) == (length (features (head training)))
          in if allIndicesUsed -- count the number of classifications b/c we can't split anymore
             then if ((length $ filter id ys) > (length $ filter not ys)) 
                  then TrueLeaf else FalseLeaf
             --here is where we actually split
             else let (idx, cutoff, gain) = getBestSplit training usedIndices
                      leftTrain = filter (\obs -> (features obs) !! idx < cutoff) training
                      rightTrain = filter (\obs -> (features obs) !! idx >= cutoff) training
                      leftTree = learn leftTrain (usedIndices ++ [idx])
                      rightTree = learn rightTrain (usedIndices ++ [idx])
                  in DecisionTree idx cutoff leftTree rightTree


