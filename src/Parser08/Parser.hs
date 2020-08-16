module Parser08.Parser where

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: String -> Either String Token
lookAhead [] = Right TokEnd
lookAhead (c:cs)    | c == '(' = Right TokLParen
                    | c == ')' = Right TokRParen
                    | otherwise = Left $ "Bad input: " ++ (c:cs)

accept :: [Char] -> Either String [Char]
accept [] = Left "Nothing to accept"
accept (c:cs) = Right cs

