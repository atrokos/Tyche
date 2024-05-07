{-# LANGUAGE InstanceSigs #-}
module Groups where
import Data.List.Split (splitOn)
import Control.Monad (foldM)

-- |Represent a user defined group.
data Group =
    Groups {
        name :: String,   -- ^Name of the current head group.
        subgroup :: Group -- ^Its subgroup.
        }
    | Group {
        name :: String    -- ^Name of the terminal group.
        }
    deriving (Eq, Ord)

instance Show Group where
    show :: Group -> String
    show (Group name) = name
    show (Groups name subgroup) = name ++ "::" ++ show subgroup

-- |Parses `Group` from the given `String`.
parseGroup :: String -> Either String Group
parseGroup = stringToGroup . splitOn "::"

-- |Parses `Group` from the given list of `Strings`.
-- `Group`s are concatenated from left to right.
stringToGroup :: [String] -> Either String Group
stringToGroup [] = Left "Empty group given!"
stringToGroup [group] = validateGroupName group >>= \name -> return $ Group name
stringToGroup (group:subgroups) =
    do
        name <- validateGroupName group
        subgroup <- stringToGroup subgroups
        return $ Groups name subgroup

-- |List of disallowed `Group` name characters.
disallowedNameChars :: [Char]
disallowedNameChars = [':', '$', '%', '@']

-- |Validates that the given `String` does not contain illegal characters.
validateGroupName :: String -> Either String String
validateGroupName name = foldM checkPresence name disallowedNameChars
    where checkPresence string char = if char `elem` string then
                                        Left $ char:" is not allowed as a group name!"
                                      else
                                        Right string
        
-- |LGroup has the whole path equal to at least part of RGroup
-- incomes, incomes::mainAccount => True
-- incomes::mainAccount, incomes::otherAccount => False
containsG :: Group -> Group -> Bool
containsG (Group lname) (Group rname) = lname == rname
containsG (Group lname) (Groups rname _) = lname == rname
containsG (Groups lname lsubgroup) (Groups rname rsubgroup) = if lname == rname then containsG lsubgroup rsubgroup else False
containsG _ _ = False