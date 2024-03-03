module Main where
import CSVParser (parseCSV, validateCSV)
import Data.Time (Day)
import Utils (parseDate)
import Filters (parseFilterArgs, all')
import System.IO (readFile')
import Transaction (parseTransactions)

main = do
  input <- getLine
  contents <- readFile' input
  let csv = case parseCSV contents >>= parseTransactions of Left _ -> []; Right c -> c
  args <- getLine
  either print (\filters -> print $ filter (all' filters) csv) $ parseFilterArgs args