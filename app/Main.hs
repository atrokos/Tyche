module Main where
import CSVParser (parseCSV, validateCSV)

main :: IO ()
main = do
    input <- getLine
    contents <- readFile input
    let csv = parseCSV contents >>= validateCSV
    case csv of 
        (Left message) -> putStrLn message
        (Right group) -> print group
    