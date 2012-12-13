module WDBCFileParser where

import System.IO
import Data.List.Split
import DataSet
import CART
import DecisionTreeUtils

--main = do 
--  inh <- openFile "wdbc.data" ReadMode
--  inputStr <- hGetContents inh
--  let inputLines = lines inputStr
--  putStr $ unlines (map (show . parseWdbcLine) inputLines)
--  hClose inh


calculateErrors :: DataSet -> Int -> (Double, Double)
calculateErrors dataSet trainSize = 
  let (training, test) = splitAt trainSize dataSet
      tree = learnDecisionTree training
  in (errorRate tree training, errorRate tree test)

-- input file line is id,M/F,pred1,pred2,...
parseWdbcLine :: String -> Observation
parseWdbcLine line = Observation predictors classification
  where parts = splitOn "," line
        mb = head $ tail parts -- this is M/B for malignant or benign
        classification = if mb == "M" then True else False
        predictors = map (\s -> read s :: Double) (tail $ tail parts)
