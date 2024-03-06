module ConfigParser where

import qualified Data.Map as M
import Data.List.Split.Internals ( splitOn )
import Data.Char (isSpace)

data Configuration = Config (M.Map String String)

parseConfigLine :: String -> Either String (String, String)
parseConfigLine str = case filter (not . isSpace) <$> splitOn "=" str of
    [key, value] -> Right (key, value)
    _            -> Left $ "Unknown line " ++ str

parseConfigFile :: String -> Either String (M.Map String String)
parseConfigFile contents = traverse parseConfigLine (lines contents) >>= \list -> return $ M.fromList list