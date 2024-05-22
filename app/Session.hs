module Session where
import CSVParser (defaultHeader)
import System.Directory (doesFileExist)

-- |Creates the configuration file and the CSV file with the default header (if it does not exist).
initAll :: String -> IO ()
initAll "" = putStrLn "Session name required."
initAll sessionName = initConfig sessionName

initConfig :: String -> IO ()
initConfig sessionName = do
  let csvname = sessionName ++ ".csv"
  writeFile "config.ini" $ "file = " ++ csvname
  exists <- doesFileExist csvname
  if not exists then do
    putStrLn $ "Created and switched to " ++ sessionName ++ "."
    initCSV csvname
  else putStrLn $ "Switched to " ++ sessionName ++ "."

initCSV :: String -> IO ()
initCSV filename = writeFile filename $
  defaultHeader
