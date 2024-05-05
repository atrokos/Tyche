module Arguments where
import System.IO (appendFile)
import Transaction
import Utils (joinString)
import Filters
import Data.List
import CSVParser
import qualified Data.Set as Set

header :: String
header = "date,from,to,title,amount,ISO\n"

printArgsHelp :: IO ()
printArgsHelp =
    putStrLn $
    "Here are all known commands:\n" ++
    "add [date] [name] [from] [to] [amount] [currency]  Adds a new transaction to the file defined in config.ini.\n" ++
    "remove [filters]                                   Remove all transactions satisfying the filters from the file defined in config.ini.\n" ++
    "filter [filters]                                   Shows all transactions satisfying the given filters.\n" ++
    "stats  [filters]                                   Shows general statistics regarding the transactions that satisfy the given filters."
    
printFiltersHelp :: IO ()
printFiltersHelp =
    putStrLn $
    "Filters follow this syntax:\n" ++
    "--[property] {>, >=, <, <=, ==} [value]\n\n" ++
    "Example: \"--amount > 50\" will filter all transactions that have their amount greater than 50.\n\n" ++
    "The \"==\" sign is implicit; you can leave it out:\n" ++
    "Example: \"--title Gas money\" will filter transactions with that title."

addCommand :: [String] -> String -> IO ()
addCommand transaction filename =
    case parseTransaction transaction of
        Left msg    -> print msg
        Right pTran -> do
            putStrLn $ "Added this transaction to " ++ filename
            print pTran
            appendFile filename ("\n" ++ dumpTransaction pTran)

getFilteredTransactions :: [String] -> String -> Either String [Transaction]
getFilteredTransactions filterArgs csvContents = do
    filters <- parseFilterArgs (joinString " " filterArgs)
    transactions <- parseCSV csvContents >>= validateCSV >>= parseTransactions
    return $ filterTransactions transactions filters

getDiffTransactions :: [String] -> String -> Either String [Transaction]
getDiffTransactions filterArgs csvContents = do
    transactions <- parseCSV csvContents >>= validateCSV >>= parseTransactions
    let allTrns = Set.fromList transactions
    toBeRemoved <- getFilteredTransactions filterArgs csvContents
    let tbrSet = Set.fromList toBeRemoved
    return $ Set.toList $ Set.difference allTrns tbrSet

-- TODO: If filters are empty, ask if the user wants to delete all
removeCommand :: [String] -> String -> IO ()
removeCommand filterArgs filename = do
    csvContents <- readFile filename
    case getDiffTransactions filterArgs csvContents of
        Left msg   -> putStrLn msg
        Right trns -> writeFile filename $ header ++ (joinString "\n" $ dumpTransaction <$> trns)

showStats :: [Transaction] -> String -> String
showStats transactions filename =
    "Showing statistics for "++ filename ++ ",\n" ++
    "from " ++ (show $ _fromDate stat) ++ " to " ++ (show $ _toDate stat) ++ "\n\n" ++
    "Total incomes:  " ++ (show $ _incomes stat) ++ "\n" ++
    "Total expenses: " ++ (show $ _expenses stat) ++ "\n" ++
    "=============================================\n" ++
    "Total difference: " ++ (show $ _diff stat)
    where
        stat = createStats transactions

handleStats :: [String] -> String -> String -> Either String String
handleStats filterArgs csvContents filename = do
    filtered <- getFilteredTransactions filterArgs csvContents
    return $ showStats filtered filename

statsCommand :: [String] -> String -> IO ()
statsCommand filterArgs filename = do
    csvContents <- readFile filename
    either putStrLn putStrLn $ handleStats filterArgs csvContents filename

filterCommand :: [String] -> String -> IO ()
filterCommand filterArgs filename = do
    csvContents <- readFile filename
    case getFilteredTransactions filterArgs csvContents of
        Left msg   -> putStrLn msg
        Right trns -> do
            putStrLn $ "Showing all relevant transactions from " ++ filename
            putStrLn $ joinString "\n\n" $ show <$> trns