module ConfigParser where

import qualified Data.Map as M
import Data.List.Split.Internals ( splitOn )
import Data.Char (isSpace)

data Configuration = Config (M.Map String String)

-- |Parses a single line into a tuple (Key, Value)
parseConfigLine :: String -> Either String (String, String)
parseConfigLine str = case filter (not . isSpace) <$> splitOn "=" str of
    [key, value] -> Right (key, value)
    _            -> Left $ "Unknown line " ++ str

-- |Parses the given .ini file contents into a Map: Key -> Value
parseConfigFile :: String -> Either String (M.Map String String)
parseConfigFile contents = traverse parseConfigLine (lines contents) >>= \list -> return $ M.fromList list