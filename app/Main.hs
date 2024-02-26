module Main where
import Groups (tamto)

main :: IO ()
main = do
    input <- getLine
    let groups = tamto input
    case groups of 
        (Left message) -> putStrLn message
        (Right group) -> print group
    