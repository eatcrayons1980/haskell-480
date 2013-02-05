import System.Environment
import Data.Char (isAlpha, toLower, isDigit, isSpace)

data Token
    = VarId String
    | IntTok Int
    | FloatTok Float
    | StringTok String
    | BoolTok Bool

-- Symbols

    | LeftParen
    | RightParen

-- Operators

    | Minus
    | Plus
    | Mod
    | Carrot
    | Mult
    | Div
    | Equal
    | Less

-- Keywords

    | KW_And
    | KW_Or
    | KW_Not
    | KW_Iff

    | KW_Assign
    | KW_Cos
    | KW_Exp
    | KW_If
    | KW_Let
    | KW_Logn
    | KW_PrintLn
    | KW_Sin
    | KW_Tan
    | KW_While

    | KW_Int
    | KW_Float
    | KW_Bool
    | KW_String        

    | EOF
    | Error String
    deriving (Eq)

operators :: [(Char,Token)]
operators = [
    ( '-', Minus ),
    ( '+', Plus ),
    ( '%', Mod ),
    ( '^', Carrot ),
    ( '*', Mult ),
    ( '/', Div ),
    ( '=', Equal ),
    ( '<', Less ),
    ( '(', LeftParen),
    ( ')', RightParen)
    ]

keywords :: [(String,Token)]
keywords = [
    ( "and", KW_And ),
    ( "or", KW_Or ),
    ( "not", KW_Not ),
    ( "iff", KW_Iff ),
    ( "assign", KW_Assign ),
    ( "cos", KW_Cos ),
    ( "exp", KW_Exp ),
    ( "if", KW_If ),
    ( "let", KW_Let ),
    ( "log", KW_Logn ),
    ( "println", KW_PrintLn ),
    ( "sin", KW_Sin ),
    ( "tan", KW_Tan ),
    ( "while", KW_While ),
    ( "int", KW_Int ),
    ( "float", KW_Float ),
    ( "string", KW_String ),
    ( "bool", KW_Bool ),
    ( "true", BoolTok True),
    ( "false", BoolTok False)
    ]

-- |'lexer' is the main tokenizing
-- function. It takes a string and
-- transforms it into a list of
-- tokens.
lexer :: String->[Token]
lexer xs = let s = removeWhiteSpace xs
    in lexer' s

-- |Called by 'lexer' as a sister
-- function.
lexer' [] = [EOF]
lexer' xs = let (t, rest) = lexToken xs
    in (t:lexer rest)

-- |'removeWhiteSpace' takes a string
-- and strips off preceding whitespace.
removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace l@(x:xs) = case isSpace x of
    True -> removeWhiteSpace xs
    False -> l

-- |'lexToken' determines the first
-- token on the string. The prefix
-- matching the token is removed.
-- The token and the remainder of the
-- string is returned.
lexToken :: String->(Token, String)
lexToken [] = (EOF, [])
lexToken l@(x:xs)
    | (x == '"') = let (string, rest) = readString l
        in (StringTok string, rest)
    | isDigit x = readNum l
    | isOperator x = ((map snd (filter ((==x).fst) operators)) !! 0, xs)
    | isKeywords l = let (string, rest) = readVarChars l
        in ((map snd (filter ((==string).fst) keywords)) !! 0, rest)
    | isVarChar x = let (string, rest) = readVarChars l
        in (VarId string, rest)
    | otherwise = (Error "Unknown character", xs)

-- |'readString' takes in as input
-- a string with a double quoted
-- string prefix. Strips the quotes
-- and returns the quoted string
-- and the remainder of the string.
readString :: String->(String, String)
readString [] = ([], [])
readString l@(x:xs) = let (quote, rest) = span isNotQuote xs
    in (quote, tail rest)

-- |'readNum' takes a string and
-- returns an IntTok or FloatTok
-- token and the rest of the string.
readNum :: String->(Token, String)
readNum [] = (EOF, [])
readNum xs = let (i, rest0) = span isDigit xs
    in case rest0 of
        ('.':ps) -> let (f, rest1) = span isDigit ps
            in case rest1 of
                ('e':es) -> let (e, rest2) = readE es
                    in (FloatTok (read (concat [i, ".", f, e])::Float), rest2)
                (_) -> (FloatTok (read (concat [i, ".", f])::Float), rest1)
        ('e':es) -> let (e, rest1) = readE es
            in (FloatTok (read (concat [i, e])::Float), rest1)
        (_:es) -> (IntTok (read i::Int), rest0)

-- |'readE' reads the exponent
-- value off the beginning of
-- a string.
readE :: String->(String, String)
readE l = case l of
    ('+':xs) -> let (exp, rest) = span isDigit xs 
        in ((concat ["e+", exp]), rest)
    ('-':xs) -> let (exp, rest) = span isDigit xs
        in ((concat ["e-", exp]), rest)
    (z:_) -> if (isDigit z)
        then let (exp, rest) = span isDigit l
            in ((concat ["e+", exp]), rest)
        else ("e+0", l)

-- |'readVarChar' splits a string
-- at the end of a variable name.
readVarChars :: String->(String, String)
readVarChars [] = ([], [])
readVarChars xs = span isVarChar xs

-- |'isVarChar' returns true if
-- the input character is a letter
-- number or underscore.
isVarChar :: Char -> Bool
isVarChar x = (isAlpha x || isDigit x || x == '_')

-- |'isNotQuote' returns true if
-- the input character is anything
-- other than a double quote.
isNotQuote :: Char -> Bool
isNotQuote c = (c /= '"')

-- |'isOperator' returns true if
-- the input character is an element
-- of the 'operators' list.
isOperator :: Char -> Bool
isOperator x = x `elem` (map fst operators)

-- |'isKeywords' returns true if
-- the input string matches an
-- element of the 'keywords' list.
isKeywords :: String -> Bool
isKeywords x = (fst (readVarChars x) `elem` (map fst keywords))

{- Main
 - Takes a filename as an argument
 - for scanning and returns a list
 - of tokens. -}
main = do
    (fileName1:_) <- getArgs
    printWords fileName1

printWords :: FilePath -> IO ()
printWords source = do
    contents <- readFile source
    mapM_ putStrLn (map show (lexer contents))

{- Show definitions for Tokens -}
instance Show Token where
    show (VarId x)     = "Variable:   "++show x
    show (IntTok x)    = "Integer:    "++show x
    show (FloatTok x)  = "Float:      "++show x
    show (StringTok x) = "String:     "++show x
    show (BoolTok x)   = "Bool:       "++show x
    show (LeftParen)   = "LeftParen:  ("
    show (RightParen)  = "RightParen: )"
    show (Minus)       = "Minus:      -"
    show (Plus)        = "Plus:       +"
    show (Mod)         = "Modulus:    %"
    show (Carrot)      = "Exponent:   ^"
    show (Mult)        = "Multiply:   *"
    show (Div)         = "Divide:     /"
    show (Equal)       = "Equal:      ="
    show (Less)        = "Less Than:  <"
    show (KW_And)      = "Keyword:    and"
    show (KW_Or)       = "Keyword:    or"
    show (KW_Not)      = "Keyword:    not"
    show (KW_Iff)      = "Keyword:    iff"
    show (KW_Assign)   = "Keyword:    assign"
    show (KW_Cos)      = "Keyword:    cos"
    show (KW_Exp)      = "Keyword:    e"
    show (KW_If)       = "Keyword:    if"
    show (KW_Let)      = "Keyword:    let"
    show (KW_Logn)     = "Keyword:    logn"
    show (KW_PrintLn)  = "Keyword:    println"
    show (KW_Sin)      = "Keyword:    sin"
    show (KW_Tan)      = "Keyword:    tan"
    show (KW_While)    = "Keyword:    while"
    show (KW_Int)      = "Keyword:    int"
    show (KW_Float)    = "Keyword:    float"
    show (KW_Bool)     = "Keyword:    bool"
    show (KW_String)   = "Keyword:    string"
    show (EOF)         = "EOF"
    show (Error x)     = "Error:      "++show x
