module Session where
import CSVParser (defaultHeader)

-- |Creates the configuration file and the CSV file with the default header.
initAll :: String -> IO ()
initAll filename = initConfig filename >> initCSV filename

initConfig :: String -> IO ()
initConfig filename = writeFile "config.ini" $
  "file = " ++ filename ++ ".csv"

initCSV :: String -> IO ()
initCSV filename = writeFile (filename ++ ".csv") $
  defaultHeader ++ "\n"
