module DecisionTreeUtils where

-- this is a set of common functionality need by 
-- both CART and the random forest algorithm

import DataSet
import Data.List

-- decision trees are going to be binary classifiers with continuous 
-- features to start.
data DecisionTree = TrueLeaf
                  | FalseLeaf
                  | DecisionTree Int Double DecisionTree DecisionTree
     deriving (Show)

-- classify :: DecisionTree -> Observation -> Bool
instance Classifier DecisionTree where
  classify TrueLeaf _                              = True
  classify FalseLeaf _                             = False
  classify (DecisionTree idx cutoff left right) o  = if ((features o) !! idx < cutoff) then classify left o 
                                                                                       else classify right o


-- this looks at each column in a training set and calculates the best column to 
-- split on and returns the column index, numerical cutoff and gain.
-- Note that this assumes that all columns are continuous variables.
getBestSplit :: DataSet -> [Int] -> (Int, Double, Double)
getBestSplit training usedIndices = (columnIdx, cutoff, gain)
  where numColumns = length $ features (head training)
        ys = getClassifications training
        allowedIndices = filter (\i -> notElem i usedIndices) [0..(numColumns - 1)]
        columnSplits = map (\i -> (i, calculateSplit (zip (getColumn i training) ys))) allowedIndices
        (columnIdx, (cutoff, gain)) = maximumBy (\(i,(c,g)) (i',(c',g')) -> compare g g') columnSplits


-- this takes a list of feature-classification pairs (i.e., we already chose a column 
-- of the training set to look at) and calculates an optimal split for this feature
-- and returns it as a (cutoff, gain) pair 
calculateSplit :: [(Double, Bool)] -> (Double, Double)
calculateSplit obs = (cutoff, gain) 
  where sortedObs = sortBy (\(x,y) (x', y') -> compare x x') obs
        ys = map snd sortedObs -- sort classifications by feature value
        splitIndices = getSplitIndices ys
        entropies = map (splitEntropy ys) splitIndices -- new entropies after splitting
        (e, idx) = minimumBy (\(e,i) (e', i') -> compare e e') $ zip entropies splitIndices
        gain = (entropy ys) - e
        cutoff = (fst (sortedObs !! idx) + fst (sortedObs !! (idx - 1))) / 2.0


-- this takes a sorted list of classifications and calculates the indices
-- that it should be split at to calculate the optimal split.  
getSplitIndices :: [Bool] -> [Int]
getSplitIndices ys = 
  let zipped = zip ys (tail ys) -- (y, next y) pairs
      idxSplitPairs = zip [1..] $ map (\(y,y') -> y /= y') zipped -- (idx, t/f if split pairs)
  in map fst $ filter snd idxSplitPairs -- filter & pull out the idx's


-- calculate the weighted average of entropies after splitting at an idx
splitEntropy :: [Bool] -> Int -> Double
splitEntropy ys idx = 
  let (p, q) = splitAt idx ys
  in ((fromIntegral $ length p) * (entropy p) + (fromIntegral $ length q) * (entropy q)) / (fromIntegral $ length ys)

-- this calculates the entropy of a list of bools
entropy :: [Bool] -> Double
entropy bools = if (numTrue == 0 || numFalse == 0) then 0.0 -- have to check explicitly or we get a NaN
                else -1 * (p1 * (log p1) + p2 * (log p2))
  where (numTrue, numFalse) = foldr (\bool (x, y) -> if bool then (x + 1, y) else (x, y +1)) (0.0,0.0) bools
        (p1, p2) = (numTrue / (numTrue + numFalse), numFalse / (numTrue + numFalse)) :: (Double, Double)
