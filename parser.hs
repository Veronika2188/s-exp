-- Define the data type for S-expressions (Symbolic Expressions)
data SExp = Atom String | SList [SExp]

-- Define how to show S-expressions as strings
instance Show SExp where
    show (Atom v) = v
    show (SList l) = "(" <> concatMap show l <> ")"

-- Tokenize the input string into a list of tokens
tokenize :: String -> [String]
tokenize input = 
  -- Filter out empty strings and split the input by spaces, adding spaces around parentheses
  filter (not . null) $ words $ concatMap (\c -> if c `elem` "()" then [' ', c, ' '] else [c]) input

-- Parse a list of tokens into an S-expression
parseTokens :: [String] -> SExp
parseTokens tokens = case parseTokens' tokens of
  -- If parsing is successful and there are no remaining tokens, return the S-expression
  (sexp, []) -> sexp
  -- Otherwise, throw an error
  _ -> error "Invalid S-expression"

-- Helper function for parsing tokens, returns an S-expression and the remaining tokens
parseTokens' :: [String] -> (SExp, [String])
parseTokens' [] = error "Unexpected end of input"
parseTokens' (token:tokens) =
  case token of
    -- If the token is an opening parenthesis, parse a list
    "(" -> do 
          let (children, rest) = parseList tokens 
          (SList children, rest)
    -- If the token is a closing parenthesis, throw an error
    ")" -> error "Unmatched )"
    -- Otherwise, the token is an atom
    _   -> (Atom token, tokens)

-- Parse a list of tokens into a list of S-expressions
parseList :: [String] -> ([SExp], [String])
parseList tokens = case tokens of
  -- If the list is empty, throw an error
  [] -> error "Unmatched ("
  -- If the next token is a closing parenthesis, return an empty list and the remaining tokens
  ")":rest -> ([], rest)
  -- Otherwise, parse an S-expression and recursively parse the rest of the list
  _ -> do 
        let (sexp, rest) = parseTokens' tokens
        case parseList rest of
            (sexps, finalRest) -> (sexp:sexps, finalRest)

-- Parse an input string into an S-expression
parse :: String -> SExp
parse input = parseTokens (tokenize input)

-- Main function to test the parser
main :: IO ()
main = print $ parse "(+ 1 (* 2 3))"
