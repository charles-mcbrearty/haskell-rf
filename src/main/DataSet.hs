module DataSet where

import Data.List
import System.Random

type FeatureType = Double
-- this is the data type for the feature variables.  We start with only
-- continuous variables

-- this represents an (x,y) pair in the data set
data Observation = Observation {
  features :: [FeatureType],
  value :: Bool
} deriving (Show)

type DataSet = [Observation] 

-- this pulls all the y's out of a training set
getClassifications :: DataSet -> [Bool]
getClassifications = map value

-- this pulls the x_i's out of the training set for a column in question
getColumn :: Int -> DataSet -> [Double]
getColumn idx = map (\o -> (features o) !! idx)

-- sorts the training set by a particular column
sortByIndex :: Int -> DataSet -> DataSet
sortByIndex idx os = sortBy (\p q -> compare ((features p) !! idx) ((features q) !! idx)) os

-- randomly split a dataset into test / training sets based on a percentage
partition :: Double -> DataSet -> IO (DataSet, DataSet)
partition fraction os = do
  g <- newStdGen
  let randomFloats = take (length os) (randomRs (0.0, 1.0) g :: [Double])
  let randomBools = map (\d -> d < fraction) randomFloats
  let zipped = zip randomBools os
  let train = map snd (filter (not . fst) zipped)
  let test = map snd (filter fst zipped)
  return (train, test)


-- this is a decision tree / random forest / whatever
class Classifier a where 
  classify :: a -> Observation -> Bool

errorRate :: (Classifier a) => a -> [Observation] -> Double
errorRate classifier obss =
  let ys = getClassifications obss
      predictions = map (classify classifier) obss
      numErrors = length $ filter (\(b1, b2) -> b1 /= b2) (zip predictions ys)
  in (fromIntegral numErrors) / (fromIntegral $ length ys)




