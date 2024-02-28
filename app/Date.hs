module Date where
import Text.Read (readMaybe)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
data Date = Date {year :: Integer, month :: Integer, day :: Integer}

instance Show Date where
    show (Date y m d) = (show y) ++"-"++ (show m) ++ "-" ++ (show d)


stringToInt :: String -> Either String Integer
stringToInt str = case readMaybe str of
                    (Just num) -> Right num
                    Nothing    -> Left $ "Could not parse to Integer " <> str

{- Accepts date in the YYYY-MM-DD format -}
parseDate :: String -> Either String Date
parseDate date = traverse stringToInt (wordsBy (== '-') date) >>= listToDate

listToDate :: [Integer] -> Either String Date
listToDate list = case list of
                    [y, m, d] -> Right $ Date y m d
                    _         -> Left $ "Given date has either too many or too few numbers!"