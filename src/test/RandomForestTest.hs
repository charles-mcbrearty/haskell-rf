module RandomForestTest where

import System.IO
import RandomForest
import WDBCFileParser
import DataSet

main = do 
  inh <- openFile "wdbc.data" ReadMode
  inputStr <- hGetContents inh
  let observations = map parseWdbcLine (lines inputStr)
  hClose inh 
