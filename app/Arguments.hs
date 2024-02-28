module Arguments where

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
    ("add [-n [date] <name> <from> <to>]", "Adds a new transaction in interactive mode. -n for non-interactive mode."),
    ("removeAll [-n [date] <name>]", "Remove all transactions satisfying the name/date.")
    ]