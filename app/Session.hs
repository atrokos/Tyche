module Session where


initAll :: String -> IO ()
initAll filename = initConfig filename >> initCSV filename

initConfig :: String -> IO ()
initConfig filename = writeFile "config.ini" $
  "file = " ++ filename ++ ".csv"

initCSV :: String -> IO ()
initCSV filename = writeFile (filename ++ ".csv") $
  "date,from,to,title,amount,ISO\n"
