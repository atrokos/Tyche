{-# LANGUAGE InstanceSigs #-}
module Filters where
import Data.Time (Day)
import Transaction
import Data.Functor.Contravariant (Contravariant (contramap))
import Groups
import Utils
import Data.List.Split (wordsBy)
import qualified Data.Char as Char

type FilterArgs = (String, String, String)
newtype Filter a = Filter (a -> Bool)

instance Contravariant Filter where
  contramap :: (a' -> a) -> Filter a -> Filter a'
  contramap g (Filter f) = Filter (f . g)

all' :: [Filter a] -> a -> Bool
all' [] _ = True
all' ((Filter f):fs) x = if f x then all' fs x else False

createCompare :: Ord a => String -> a -> Either String (Filter a)
createCompare ">" val = Right $ Filter (> val)
createCompare "<" val = Right $ Filter (< val)
createCompare ">=" val = Right $ Filter (>= val)
createCompare "<=" val = Right $ Filter (<= val)
createCompare "==" val = Right $ Filter (== val)
createCompare c _ = Left $ "Unknown comparator: " ++ c

createFilter :: FilterArgs -> Either String (Filter Transaction)
createFilter (typ, comp, val) = case typ of
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
  _ -> Left "Unsupported filter field"

parseFilters :: [FilterArgs] -> Either String [Filter Transaction]
parseFilters list = traverse createFilter list

filterTransactions :: [Transaction] -> [Filter Transaction] -> [Transaction]
filterTransactions trns filters = filter (all' filters) trns

parseFilterArgs :: String -> [FilterArgs]
parseFilterArgs string = wordsBy (Char.isSpace) string 