

data Consumed = Consumed {result :: String, rest :: String}
    deriving Show

consumeWhile :: String -> (Char -> Bool) -> Consumed
consumeWhile [] _ = Consumed [] []
consumeWhile list pred = consumeWhileH (reverse list) pred []
    where
        consumeWhileH :: String -> (Char -> Bool) -> String -> Consumed
        consumeWhileH [] _ acc = Consumed acc []
        consumeWhileH (x:rest) pred acc = if pred x then consumeWhileH rest pred (x:acc) else Consumed acc rest

consumeString :: String -> String -> Either String Consumed
consumeString [] list = Left "A string was not ended properly!"
consumeString (curr:xs) acc
    | curr == '"' = if isEscaped xs curr then consumeString (drop 1 xs) (acc ++ "\"") else Right $ Consumed acc xs
    | otherwise   = consumeString xs (acc ++ [curr])

isEscaped :: String -> Char -> Bool
isEscaped [] _ = False
isEscaped (x:rest) char = char == x