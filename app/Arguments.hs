module Arguments where
import System.IO (appendFile)
import Transaction
    ( createStats,
      dumpTransaction,
      parseTransaction,
      parseTransactions,
      Statistics(_diff, _fromDate, _toDate, _incomes, _expenses),
      Transaction )
import Utils (joinString)
import Filters ( filterTransactions, parseFilterArgs )
import Data.List ()
import CSVParser ( parseCSV, validateCSV, defaultHeader )
import qualified Data.Set as Set
import CSVParser (defaultHeader)

-- |Prints all commands and their help string to STDOUT.
printArgsHelp :: IO ()
printArgsHelp =
    putStrLn $
    "Here are all known commands:\n" ++
    "add [date] [name] [from] [to] [amount] [currency]  Adds a new transaction to the file defined in config.ini.\n" ++
    "remove [filters]                                   Remove all transactions satisfying the filters from the file defined in config.ini.\n" ++
    "filter [filters]                                   Shows all transactions satisfying the given filters.\n" ++
    "stats  [filters]                                   Shows general statistics regarding the transactions that satisfy the given filters."
    
-- |Prints help about `Filter`s to STDOUT.
printFiltersHelp :: IO ()
printFiltersHelp =
    putStrLn $
    "Filters follow this syntax:\n" ++
    "--[property] {>, >=, <, <=, ==} [value]\n\n" ++
    "Example: \"--amount > 50\" will filter all transactions that have their amount greater than 50.\n\n" ++
    "The \"==\" sign is implicit; you can leave it out:\n" ++
    "Example: \"--title Gas money\" will filter transactions with that title."

-- |Main command that parses the given list of `String`s, converts them to a `Transaction` and
-- appends it to the configured CSV file.
addCommand :: [String] -> String -> IO ()
addCommand transaction filename =
    case parseTransaction transaction of
        Left msg    -> print msg
        Right pTran -> do
            putStrLn $ "Added this transaction to " ++ filename
            print pTran
            appendFile filename ("\n" ++ dumpTransaction pTran)

-- |Parses the `Filter` arguments and returns all `Transaction`s satisfying all the `Filter`s.
getFilteredTransactions :: [String] -> String -> Either String [Transaction]
getFilteredTransactions filterArgs csvContents = do
    filters <- parseFilterArgs (joinString " " filterArgs)
    if length filters == 0 then
        Left "No filters given. If you want to remove all transactions, the command is `remove --ALL TRNS`."
    else
        parseCSV csvContents >>= validateCSV >>= parseTransactions >>= \transactions -> return $ filterTransactions transactions filters

-- |Removes all `Transaction`s satisftying all given filters.
removeTransactions :: [String] -> String -> Either String [Transaction]
removeTransactions filterArgs csvContents = do
    transactions <- parseCSV csvContents >>= validateCSV >>= parseTransactions
    let allTrns = Set.fromList transactions
    toBeRemoved <- getFilteredTransactions filterArgs csvContents
    let tbrSet = Set.fromList toBeRemoved
    return $ Set.toList $ Set.difference allTrns tbrSet

-- |Main command for removing `Transaction`s.
removeCommand :: [String] -> String -> IO ()
removeCommand filterArgs filename = do
    csvContents <- readFile filename
    case removeTransactions filterArgs csvContents of
        Left msg   -> putStrLn msg
        Right trns -> writeFile filename $ (joinString "\n" $ defaultHeader : (dumpTransaction <$> trns))

-- |Shows statistics from the given list of `Transaction`s.
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

-- |Filters `Transaction`s before showing them.
handleStats :: [String] -> String -> String -> Either String String
handleStats filterArgs csvContents filename = do
    filtered <- getFilteredTransactions filterArgs csvContents
    return $ showStats filtered filename

-- |Main command for running `Transaction` statistics.
statsCommand :: [String] -> String -> IO ()
statsCommand filterArgs filename = do
    csvContents <- readFile filename
    either putStrLn putStrLn $ handleStats filterArgs csvContents filename

-- |Main command for filtering `Transaction`s.
filterCommand :: [String] -> String -> IO ()
filterCommand filterArgs filename = do
    csvContents <- readFile filename
    case getFilteredTransactions filterArgs csvContents of
        Left msg   -> putStrLn msg
        Right trns ->
            if length trns == 0 then
                putStrLn "No transactions satisfying the given filters were found."
            else 
                listTransactions trns filename

-- |Lists all found `Transaction`s (if any).
listTransactions :: [Transaction] -> String -> IO ()
listTransactions trns filename = do
    putStrLn $ "Showing " ++ (show $ length trns) ++ " relevant transaction(s) from " ++ filename
    putStrLn $ joinString "\n\n" $ show <$> trns
