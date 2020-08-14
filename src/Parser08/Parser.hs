module Parser08.Parser where

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: String -> Token
lookAhead [] = TokEnd
lookAhead (c:cs)    | c == '(' = TokLParen
                    | c == ')' = TokRParen
                    | otherwise = error $ "Bad input: " ++ (c:cs)

accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (c:cs) = cs

