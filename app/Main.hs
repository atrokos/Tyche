module Main where
import CSVParser (parseCSV, validateCSV)
import Data.Time (Day)
import Utils (parseDate, joinString)
import Filters (parseFilterArgs, all')
import System.IO (readFile')
import ConfigParser ( parseConfigFile )
import qualified Data.Map as M
import Transaction (parseTransactions, Transaction)
import Data.Maybe ( fromMaybe )
import System.Environment ( getArgs )
import Arguments
    ( addCommand,
      filterCommand,
      printArgsHelp,
      printFiltersHelp,
      removeCommand,
      statsCommand )
import Session (initAll, initConfig)

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

-- |Checks the config file for the CSV file path.
init :: String -> Either String String
init contents = do
  config <- parseConfigFile contents
  case M.lookup "file" config of
    Nothing     -> Left "Filepath not found in config!"
    (Just path) -> Right path
  
-- |Returns a Map of Command -> Command function
commands :: M.Map String ([String] -> String -> IO ())
commands = M.fromList [
          ("add", addCommand),
          ("remove", removeCommand),
          ("stats", statsCommand),
          ("filter", filterCommand),
          ("help", \_ _ -> printArgsHelp),
          ("filter?", \_ _ -> printFiltersHelp),
          ("init", \_ _ -> putStrLn "The init command only takes one argument (session name)."),
          ("switch", \_ _ -> putStrLn "The switch command only takes one argument (session name)."),
          ("session", \_ session -> putStrLn $ "The current session is " ++ session)]

-- |Executes the given command if it exists.
handleArgs :: String -> [String] -> IO ()
handleArgs filename (command:args) = do
  case M.lookup command commands of
    Just func -> func args filename
    Nothing   -> putStrLn ("Uknown command: " ++ command) >> printArgsHelp

-- |Handles configuration related operations, like creating or parsing it.
handleConfig :: [String] -> IO ()
handleConfig ["init", filename] = initAll filename
handleConfig ["switch", filename] = initConfig filename
handleConfig args = do
  contents <- readFile' "config.ini"
  case Main.init contents of
    Right filename -> handleArgs filename args
    Left err -> putStrLn err

main = getArgs >>= handleConfig
