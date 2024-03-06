module Main where
import CSVParser (parseCSV, validateCSV)
import Data.Time (Day)
import Utils (parseDate)
import Filters (parseFilterArgs, all')
import System.IO (readFile')
import ConfigParser ( parseConfigFile )
import qualified Data.Map as M
import Transaction (parseTransactions, Transaction)
import Data.Maybe ( fromMaybe )
import System.Environment
import Arguments

{-
WORKFLOW
0) Load configuration, find path
1) Get arguments;
    depending on argument, do:
      add    ->  addTransaction (IO)
      remove ->  Load transactions (IO) -> removeAllTransactions (IO)
      stats  ->  Load transactions (IO) -> putStrLn $ showStats transactions (IO)
      filter ->  Load transactions (IO) -> Parse filters (Either) -> filterTransactions (Either) -> showTransactions (IO)
-}


init :: String -> Either String String
init contents = do
  config <- parseConfigFile contents
  case M.lookup "file" config of
    Nothing     -> Left "Filepath not found in config!"
    (Just path) -> Right path
  
commands :: M.Map String ([String] -> String -> IO ())
commands = M.fromList [
          ("add", addCommand),
          ("remove", removeCommand),
          ("stats", statsCommand),
          ("filter", filterCommand)]

handleArgs :: String -> [String] -> IO ()
handleArgs filename (command:args) = do
  case M.lookup command commands of
    Just func -> func args filename
    Nothing   -> putStrLn $ "Uknown command: " ++ command

main = do
  contents <- readFile' "config.ini"
  args <- getArgs
  case Main.init contents of
    Left m -> putStrLn m
    Right filename -> handleArgs filename args

-- main = getArgs >>= print