module Arguments where
import System.IO (appendFile)
import Transaction
import Utils (joinString)
import Filters
import Data.List
import CSVParser
import Data.Either

printArgs :: [(String, String)] -> IO ()
printArgs args = do
    let maxL = foldl (\ curr tuple -> max ((length . fst) tuple) curr ) 1 args
    let formattedArgs = formatArgHelp args maxL
    either putStrLn (mapM_ putStrLn) formattedArgs

formatArgHelp :: [(String, String)] -> Int -> Either String [String]
formatArgHelp argHelp numOfSpaces =
    let
        spaces :: String -> Either String String
        spaces a = if numOfSpaces < 0 then Left "Number of spaces cannot be negative!" else Right $ replicate (numOfSpaces - length a + 2) ' '
        format :: (String, String) -> Either String String
        format (comm, help) = do
            offset <- spaces comm
            return $ comm ++ offset ++ help
    in traverse format argHelp

fullArgHelp :: [(String, String)]
fullArgHelp = [
    ("add [-n [date] <name> <from> <to> <amount> <currency>]", "Adds a new transaction in interactive mode. -n for non-interactive mode."),
    ("removeAll [-n [date] <name>]", "Remove all transactions satisfying the name/date.")
    ]

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
    
removeCommand :: [String] -> String -> IO ()
removeCommand filterArgs filename = do
    print filterArgs
    csvContents <- readFile filename
    case getFilteredTransactions filterArgs csvContents of
        Left msg   -> putStrLn msg
        Right trns -> writeFile filename $ joinString "\n" $ dumpTransaction <$> trns

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