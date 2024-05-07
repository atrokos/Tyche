module Utils where
import Data.Foldable (Foldable(foldl'))
import Text.Read (readEither)
import Data.Time (Day, getCurrentTime, utctDay)
import Data.Time.Format ( defaultTimeLocale, parseTimeM )
import System.IO (appendFile)

-- |Joins `String`s together using the given separator.
joinString :: String -> [String] -> String
joinString _ [] = ""
joinString _ [string] = string
joinString sep strings = foldl' (\ acc str -> acc ++ str ++ sep) "" (init strings) ++ last strings

{-|
  Converts `Maybe` b to `Either` a b like so:
  
  `Just` b -> `Right` b
  `Nothing` -> `Left` a
-}
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just right) = Right right

-- |Parses the given `String` to a `Float`.
stringToFloat :: String -> Either String Float
stringToFloat str = readEither str

-- |Parses `Day` from a `String` in the "YYYY-(M)M-(D)D" format .
parseDate :: String -> Either String Day
parseDate s = maybeToEither ("The date is incorrect: " ++ s) $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" s
