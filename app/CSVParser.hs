module CSVParser where
import Utils ( joinString ) 

type CSVCell = String
type CSVRow = [CSVCell]
type ParsedCSV = Either String [CSVRow]

data Consumed = Consumed {result :: CSVCell, rest :: String}
    deriving Show

consumeString :: String -> String -> Either String Consumed
consumeString [] list = Left "A string was not ended properly!"
consumeString (curr:xs) acc
    | curr == '"' = if isEscaped xs curr then consumeString (drop 1 xs) (acc ++ "\"") else Right $ Consumed acc xs
    | otherwise   = consumeString xs (acc ++ [curr])

isEscaped :: String -> Char -> Bool  -- Checks whether the next character (if any) is the one supplemented
isEscaped [] _ = False
isEscaped (x:rest) char = char == x

consumeCell :: String -> Consumed
consumeCell string = uncurry Consumed (span (/= ',') string)

parseLine :: String -> Either String CSVRow
parseLine [] = Right []
parseLine (',':'"':xs) = do
    currParsed <- consumeString xs []
    nextParsed <- parseLine (rest currParsed)
    return $ [result currParsed] ++ nextParsed

parseLine (',':xs) = parseLine (rest consumedWOB) >>= (\consumed -> return $ [result consumedWOB] ++ consumed)
  where consumedWOB = consumeCell xs

parseLine xs = parseLine (rest consumedWB) >>= (\consumed -> return $ [result consumedWB] ++ consumed)
  where consumedWB = consumeCell xs

parseCSV :: String -> ParsedCSV
parseCSV input = traverse parseLine (lines input)

validateCSV :: [CSVRow] -> ParsedCSV -- Checks whether all rows have the same amount of columns
validateCSV [] = Right []
validateCSV [row] = Right [row]
validateCSV all@(header:rows) = traverse (\r -> if l == length r then Right r else Left $ "Error: This row does not have the same amount of columns as the header:\n" <> (joinString "," r)) all
    where l = length $ header
