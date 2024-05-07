module Transaction where
import Groups (Group, parseGroup)
import Data.Time.Calendar ( Day )
import CSVParser (CSVRow, ParsedCSV)
import Utils ( joinString, maybeToEither, stringToFloat, parseDate )
import Data.List


-- |Represents a single monetary transaction.
data Transaction =
  Transaction {
    _date::Day,       -- ^The day the Transaction was performed.
    _from::Group,     -- ^Group from which the money was taken. 
    _to::Group,       -- ^Group to which the money was put.
    _title :: String, -- ^Title of the Transaction.
    _money::Money     -- ^Monetary info of the Transaction.
    } 
    | Empty
  deriving (Eq, Ord)

-- |Groups currency and its value together.
data Money =
  Money {
    _amount::Float,   -- ^Monetary amount.
    _currency::String -- ^ISO code of the currency.
    }
  deriving (Eq, Ord)

instance Show Money where
  show (Money amount curr) = show amount ++ " " ++ curr

instance Show Transaction where
  show t = show (_date t) ++ "  " ++ (_title t) ++ "\n" ++
            "  " ++ (show $ _from t) ++ " -> " ++ (show $ _to t) ++
            "\n  " ++ (show $ _money t)

-- |Parses a CSVRow to a `Transaction`.
-- Returns: Right when parsing succeeds, Left when an error occurs.
parseTransaction :: CSVRow -> Either String Transaction
parseTransaction [date, title, from, to, amount, curr] =
  do
    pDate   <- parseDate date
    pFrom   <- parseGroup from
    pTo     <- parseGroup to
    pAmount <- stringToFloat amount
    return $ Transaction pDate pFrom pTo title (Money pAmount curr)
parseTransaction row = Left $ "This line is in incorrect format:\n" ++ (joinString "," row)

-- |Parses CSV to a list of `Transaction`s.
parseTransactions :: [CSVRow] -> Either String [Transaction]
parseTransactions csv = traverse parseTransaction csv

-- |Converts a `Transaction` to its CSV equivalent.
dumpTransaction :: Transaction -> String
dumpTransaction t = joinString "," [date, (_title t), from, to, amount, curr]
  where
    date   = show $ _date t
    from   = show $ _from t
    to     = show $ _to t
    amount = show $ (_amount . _money) t
    curr   = (_currency . _money) t

{-|
  Writes the list of `Transaction`s to the given file.
  
  Params:
  - `String` : path to the file
  - `[Transaction]` : list of `Transaction`s
-}
writeTransactions :: String -> [Transaction] -> IO ()
writeTransactions filename transactions = writeFile filename csv
  where csv = foldl1 (\l r -> l ++ "\n" ++ r) $ dumpTransaction <$> transactions

-- |The most basic implementation of Lenses - we only need a getter and a setter for the nested Money.
-- The rest can be used with the implemented record functions.
data Lens' a b = Lens' {get :: (a -> b), set :: (a -> b -> a)}

-- |Gets or sets the value for the `Transactions`' money amount.
amount :: Lens' Transaction Float
amount = Lens' (\t -> (_amount . _money) t) (\t newVal -> t {_money = Money { _amount = newVal, _currency = (_currency . _money) t}})

-- |Gets or sets the ISO code for the `Transactions`' currency.
currency :: Lens' Transaction String
currency = Lens' (\t -> (_currency . _money) t) (\t newVal -> t {_money = Money { _amount =  (_amount . _money) t, _currency = newVal}})

{-|
  Represents basic statistics of `Transaction`s.
-}
data Statistics = Statistics {
  _incomes :: Float,
  _expenses :: Float,
  _diff :: Float,
  _fromDate :: Day,
  _toDate :: Day
} | EmptyStat

{-|
  Updates the statistics according to the given `Transaction`.
-}
updateStatistics :: Statistics -> Transaction -> Statistics
updateStatistics EmptyStat t =
  let
    incomes   = max (get amount t) 0
    expenses  = min (get amount t) 0
    diff      = incomes - expenses
    fromDate  = _date t
    toDate    = _date t
  in Statistics incomes expenses diff fromDate toDate

updateStatistics stat t =
  let
    incomes   = (_incomes stat) + max (get amount t) 0
    expenses  = (_expenses stat) + min (get amount t) 0
    diff      = incomes - expenses
    fromDate  = min (_fromDate stat) (_date t)
    toDate    = max (_toDate stat) (_date t)
  in Statistics incomes expenses diff fromDate toDate

-- |Creates a statistic from the given list of `Transaction`s.
createStats :: [Transaction] -> Statistics
createStats transactions = foldl' updateStatistics EmptyStat transactions
