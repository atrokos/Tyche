{-# LANGUAGE InstanceSigs #-}
module Groups where
import Data.List.Split (splitOn)
import Control.Monad (foldM)

data Group = Groups {name :: String, subgroup :: Group} | Group {name :: String}
    deriving (Eq, Ord)

instance Show Group where
    show :: Group -> String
    show (Group name) = name
    show (Groups name subgroup) = name ++ "::" ++ show subgroup

parseGroup :: String -> Either String Group
parseGroup = stringToGroup . splitOn "::"

stringToGroup :: [String] -> Either String Group
stringToGroup [] = Left "Empty group given!"
stringToGroup [group] = validateGroupName group >>= \name -> return $ Group name
stringToGroup (group:subgroups) =
    do
        name <- validateGroupName group
        subgroup <- stringToGroup subgroups
        return $ Groups name subgroup

disallowedNameChars :: [Char]
disallowedNameChars = [':', '$', '%', '@'] -- klidne zmenit

validateGroupName :: String -> Either String String
validateGroupName name = foldM checkPresence name disallowedNameChars

checkPresence :: String -> Char -> Either String String
checkPresence string char = if char `elem` string then Left $ char:" is not allowed as a group name!" else Right string

length :: Group -> Integer
length (Group _) = 1
length (Groups _ subgroup) = lengthH subgroup 1
    where
        lengthH (Group _) acc = acc + 1
        lengthH (Groups _ sub) acc = lengthH sub (acc + 1)
        
-- LGroup has the whole path equal to at least part of RGroup
-- incomes, incomes::mainAccount => True
-- incomes::mainAccount, incomes::otherAccount => False
containsG :: Group -> Group -> Bool
containsG (Group lname) (Group rname) = lname == rname
containsG (Group lname) (Groups rname _) = lname == rname
containsG (Groups lname lsubgroup) (Groups rname rsubgroup) = if lname == rname then containsG lsubgroup rsubgroup else False
containsG _ _ = False