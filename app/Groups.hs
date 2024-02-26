module Groups where
import Data.List.Split (splitOn)
import Errors
import Control.Monad (foldM)

data Group = Groups {name :: String, subgroup :: Group} | Group {name :: String}
    deriving Show

tamto :: String -> Either String Group
tamto = parseGroup . splitOn "::"

parseGroup :: [String] -> Either String Group
parseGroup [] = Left "Empty group given!"
parseGroup [group] = validateGroupName group >>= \name -> return $ Group name
parseGroup (group:subgroups) =
    do
        name <- validateGroupName group
        subgroup <- parseGroup subgroups
        return $ Groups name subgroup

disallowedNameChars :: [Char]
disallowedNameChars = [':', '$', '%', '@'] -- klidne zmenit

validateGroupName :: String -> Either String String
validateGroupName name = foldM checkPresence name disallowedNameChars

checkPresence :: String -> Char -> Either String String
checkPresence string char = if char `elem` string then Left $ char:" is not allowed as a group name!" else Right string
