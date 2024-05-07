module CSVParser where
import Utils ( joinString ) 
import Data.Char (isSpace)
import Data.List.Split (splitOn)

type CSVCell = String
type CSVRow = [CSVCell]
type ParsedCSV = Either String [CSVRow]

-- |Returns the default CSV header.
defaultHeader :: String
defaultHeader = "date,title,from,to,amount,ISO"

-- |Represents the state of CSV parsing.
data Consumed =
  Consumed {
    result :: CSVCell, -- ^The most currently parsed `CSVCell`.
    rest :: String     -- ^Rest of the CSV row to the right of `result` as a `String`.
    }
    deriving Show

consumeString :: String -> String -> Either String Consumed
consumeString "" list = Left "A string was not ended properly!"
consumeString (curr:xs) acc
    | curr == '"' = if isEscaped xs curr then consumeString (drop 1 xs) (acc ++ "\"") else Right $ Consumed acc xs
    | otherwise   = consumeString xs (acc ++ [curr])

-- |Checks whether the next character (if any) is the one supplemented.
isEscaped :: String -> Char -> Bool 
isEscaped "" _ = False
isEscaped (x:rest) char = char == x

consumeCell :: String -> Consumed
consumeCell string = uncurry Consumed (span (/= ',') string)

parseLine :: String -> Either String CSVRow
parseLine "" = Right []
parseLine (',':'"':xs) = do
    currParsed <- consumeString xs []
    nextParsed <- parseLine (rest currParsed)
    return $ [result currParsed] ++ nextParsed
parseLine (',':xs) = parseLine (rest consumedWOB) >>= (\consumed -> return $ [result consumedWOB] ++ consumed)
  where consumedWOB = consumeCell xs
parseLine xs = parseLine (rest consumedWB) >>= (\consumed -> return $ [result consumedWB] ++ consumed)
  where consumedWB = consumeCell xs

parseCSV :: String -> ParsedCSV
parseCSV "" = Left "An empty CSV was found, terminating."
parseCSV input = traverse parseLine (nonEmpty . lines $ input) >>= \csv -> return $ csv
  where nonEmpty = filter (any (not . isSpace))

-- |Checks that the given CSV has the default header and that all rows have the same amount of columns.
-- Returns the same CSV, without its header when successful.
validateCSV :: [CSVRow] -> ParsedCSV
validateCSV [] = Left "The CSV file is missing a header!"
validateCSV (header:rows) = do
  let headerString = joinString "," header
  let expectedRows = length $ splitOn "," defaultHeader
  if defaultHeader /= headerString then
    Left "The CSV file has an incorrect header or it is missing!"
  else
    validateRows expectedRows rows

validateRows :: Int -> [CSVRow] -> ParsedCSV
validateRows rowLength = traverse checkLength
  where checkLength row = if length row /= rowLength then
                            Left $ "Error: This row does not have the same amount of columns as the header:\n" ++ joinString "," row
                          else
                            Right row