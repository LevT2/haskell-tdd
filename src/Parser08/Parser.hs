module Parser08.Parser where

data Tree = Node Tree Tree | Leaf
    deriving (Show, Eq)

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

  
data TScanError = NothingToAccept 
                | BadInput String 
  deriving (Show, Eq)

data TParseError = UnconsumedString String
                  | BadExpression String
                  | ScanError TScanError
  deriving (Show, Eq)
  

--data ScanError = BadInput {input :: String, expected :: [String]}
--  deriving (Show, Eq)

lookAhead :: String -> Either TScanError Token
lookAhead [] = Right TokEnd
lookAhead (c:cs)    | c == '(' = Right TokLParen
                    | c == ')' = Right TokRParen
                    | otherwise = Left $ BadInput (c:cs)

accept :: [Char] -> Either TScanError [Char]
accept [] = Left $ NothingToAccept
accept (c:cs) = Right cs


--root, expr, par :: [Char] -> (Tree, [Char])

root = \s -> (Leaf, "()")
--
--expr toks =
--    let (p, toks')   = par toks
--        (p', toks'') = par toks'
--    in (Node p p', toks'')
--
---- kind of unwrap helper
--accept' toks = case accept toks of
--  Right value -> value
--  Left error -> error
--
--par toks =
--    case lookAhead toks of
--      TokLParen ->
--        case lookAhead (accept toks) of
--          TokRParen -> (Leaf, accept (accept toks))
--          _ -> let (e, toks') = expr (accept toks)
--               in  if lookAhead toks' == TokRParen
--                   then (e, accept toks')
--                   else error $ "Missing closing paren in: " ++ show toks'
--      _ -> error $ "Bad expression: " ++ show toks

parse :: String -> Either TParseError Tree
parse str = let (tree, str') = root str
            in
                if null str'
                then Right tree
                else Left $ UnconsumedString str'