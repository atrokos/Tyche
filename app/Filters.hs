{-# LANGUAGE InstanceSigs #-}
module Filters where
import Data.Time (Day)
import Transaction
import Data.Functor.Contravariant (Contravariant (contramap))
import Groups
import Utils
import Data.List.Split (wordsBy)
import qualified Data.Char as Char
import Data.List.Split.Internals ( splitOn )

newtype Filter a = Filter (a -> Bool)

instance Contravariant Filter where
  contramap :: (a' -> a) -> Filter a -> Filter a'
  contramap g (Filter f) = Filter (f . g)

all' :: [Filter a] -> a -> Bool
all' [] _ = True
all' ((Filter f):fs) x = if f x then all' fs x else False

createCompare :: Ord a => String -> a -> Either String (Filter a)
createCompare "==" val = Right $ Filter (== val)
createCompare ">" val = Right $ Filter (> val)
createCompare "<" val = Right $ Filter (< val)
createCompare ">=" val = Right $ Filter (>= val)
createCompare "<=" val = Right $ Filter (<= val)
createCompare c _ = Left $ "Unknown comparator: " ++ c

createFilter :: [String] -> Either String (Filter Transaction)
createFilter [typ, val] = parseFilter typ "==" val
createFilter [typ, comp, val] = parseFilter typ comp val
createFilter arg = Left $ "Unknown argument: " ++ show arg

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
  "from"   -> return $ contramap _from (Filter (`contains` val))
  "to"     -> return $ contramap _to (Filter (`contains` val))
  wrong    -> Left $ "Unknown filter property: " ++ wrong

parseFilters :: [[String]] -> Either String [Filter Transaction]
parseFilters list = traverse createFilter list

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

safeLast :: String -> Char
safeLast "" = '\0'
safeLast rest = last rest

stripQuotations :: String -> Either String String
stripQuotations ('"':rest) = if safeLast rest == '"' then Right $ init rest else Left "Non-terminated string!"
