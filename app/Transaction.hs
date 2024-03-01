module Transaction where
import Groups (Group, parseGroup)
import Data.Time.Calendar ( Day )
import CSVParser (CSVRow, ParsedCSV)
import Utils ( joinString, maybeToEither, stringToFloat, parseDate )

data Transaction = Transaction {_date::Day, _from::Group, _to::Group, _title :: String, _money::Money} | Empty
  deriving Show
data Money = Money {_amount::Float, _currency::String}
  deriving Show

parseTransaction :: CSVRow -> Either String Transaction
parseTransaction [date, from, to, title, amount, curr] =
  do
    pDate   <- parseDate date
    pFrom   <- parseGroup from
    pTo     <- parseGroup to
    pAmount <- stringToFloat amount
    return $ Transaction pDate pFrom pTo title (Money pAmount curr)

parseTransaction row = Left $ "This line is in incorrect format:\n" ++ (joinString "," row)

parseTransactions :: [CSVRow] -> Either String [Transaction]
parseTransactions csv = traverse parseTransaction csv

dumpTransaction :: Transaction -> String
dumpTransaction t = joinString "," [date, from, to, amount, curr]
  where
    date   = show $ _date t
    from   = show $ _from t
    to     = show $ _to t
    amount = show $ (_amount . _money) t
    curr   = show $ (_currency . _money) t

writeTransactions :: String -> [Transaction] -> IO ()
writeTransactions filename transactions = writeFile filename csv
  where csv = foldl1 (\l r -> l ++ "\n" ++ r) $ dumpTransaction <$> transactions

-- The most basic implementation of Lenses - I only need a getter and a setter for the nested Money,
-- rest can be used with the implemented record functions
data Lens' a b = Lens' {get :: (a -> b), set :: (a -> b -> a)}

amount :: Lens' Transaction Float
amount = Lens' (\t -> (_amount . _money) t) (\t newVal -> t {_money = Money { _amount = newVal, _currency = (_currency . _money) t}})

currency :: Lens' Transaction String
currency = Lens' (\t -> (_currency . _money) t) (\t newVal -> t {_money = Money { _amount =  (_amount . _money) t, _currency = newVal}})
