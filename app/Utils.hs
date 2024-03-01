module Utils where
import Data.Foldable (Foldable(foldl'))
import Text.Read (readEither)
import Data.Time (Day)
import Data.Time.Format ( defaultTimeLocale, parseTimeM )

joinString :: String -> [String] -> String
joinString _ [] = ""
joinString _ [string] = string
joinString sep strings = foldl' (\ acc str -> acc ++ str ++ sep) "" (init strings) ++ last strings

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just right) = Right right

stringToFloat :: String -> Either String Float
stringToFloat str = readEither str

-- Parses a date in the "YYYY-(M)M-(D)D" format 
parseDate :: String -> Either String Day
parseDate s = maybeToEither ("The date is incorrect: " ++ s) $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" s