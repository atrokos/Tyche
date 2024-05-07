{-# LANGUAGE InstanceSigs #-}
module Filters where
import Data.Time (Day)
import Transaction
    ( amount, Lens'(get), Transaction(_title, _date, _from, _to) )
import Data.Functor.Contravariant (Contravariant (contramap))
import Groups ( containsG, parseGroup )
import Utils ( joinString, stringToFloat, parseDate )
import Data.List.Split (wordsBy)
import qualified Data.Char as Char
import Data.List.Split.Internals ( splitOn )

newtype Filter a = Filter (a -> Bool)

instance Contravariant Filter where
  contramap :: (a' -> a) -> Filter a -> Filter a'
  contramap g (Filter f) = Filter (f . g)

-- |Checks whether the given value satisfies all `Filter`s.
all' :: [Filter a] -> a -> Bool
all' [] _ = True
all' ((Filter f):fs) x = if f x then all' fs x else False

-- |Creates a comparison function according to the given `String`. 
createCompare :: Ord a => String -> a -> Either String (Filter a)
createCompare "==" val = Right $ Filter (== val)
createCompare ">" val = Right $ Filter (> val)
createCompare "<" val = Right $ Filter (< val)
createCompare ">=" val = Right $ Filter (>= val)
createCompare "<=" val = Right $ Filter (<= val)
createCompare c _ = Left $ "Unknown comparator: " ++ c

-- |Creates a `Filter` from the given `String`.
createFilter :: [String] -> Either String (Filter Transaction)
createFilter [typ, val] = parseFilter typ "==" val
createFilter [typ, comp, val] = parseFilter typ comp val
createFilter arg = Left $ "Unknown argument: " ++ joinString " " arg

-- |Parses the given `String`s parts to the actual `Filter`.
parseFilter :: String -> String -> String -> Either String (Filter Transaction)
parseFilter typ comp val = case typ of
  "amount" -> do
                parsedValue <- stringToFloat val
                filter      <- createCompare comp parsedValue
                return $ contramap (get amount) filter
  "date"   -> do
                parsedValue <- parseDate val
                filter      <- createCompare comp parsedValue
                return $ contramap _date filter
  "title"  -> createCompare comp val >>= \filter -> return $ contramap _title filter
  "from"   -> do
                parsedGroup <- parseGroup val
                return $ contramap _from (Filter (parsedGroup `containsG`))
  "to"     -> do
                parsedGroup <- parseGroup val
                return $ contramap _to (Filter (parsedGroup `containsG`))
  "ALL"    -> return $ contramap _title (Filter (/= "\0")) -- A small trick; I assume that no user can input the NULL character, so all transactions cannot equal this.
  wrong    -> Left $ "Unknown filter property: " ++ wrong

-- |Creates `Filter`s from the given list of lists of `String`s.
parseFilters :: [[String]] -> Either String [Filter Transaction]
parseFilters list = traverse createFilter list

-- |Returns a list of `Transaction`s that satisfy all given `Filter`s.
filterTransactions :: [Transaction] -> [Filter Transaction] -> [Transaction]
filterTransactions trns filters = filter (all' filters) trns

parseFilterArgs :: String -> Either String [Filter Transaction]
parseFilterArgs string = traverse splitWords (splitOn "--" string) >>= \(_:rest) -> parseFilters rest

splitWords :: String -> Either String [String]
splitWords "" = Right []
splitWords str = do
    let spanned = span (/= '"') str
    if (not . null . snd) spanned then
        stripQuotations (snd spanned) >>= \noQuote -> return $ (words . fst) spanned ++ [noQuote]
    else
        return $ (words . fst) spanned

-- |A `String` variant of last that is total.
safeLast :: String -> Char
safeLast "" = '\0'
safeLast rest = last rest

-- |Removes quoation marks from both ends of the `String`.
stripQuotations :: String -> Either String String
stripQuotations ('"':rest) = if safeLast rest == '"' then Right $ init rest else Left "Non-terminated string!"
