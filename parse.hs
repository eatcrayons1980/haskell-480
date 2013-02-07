{----------------------------------------------------
    CS480 - IBTL Compiler
    Authors: Kevin Tang, Kyle Mannari, Paul Freeman
-----------------------------------------------------}

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

{----------------------------------------------------
    lexer:
    Is the main tokenizing function. It takes a
    string and transforms it into a list of
    tokens.
-----------------------------------------------------}
lexer :: String->[Token]
lexer xs = let s = removeWhiteSpace xs
    in lexer' s


{----------------------------------------------------
    lexer':
    Called by 'lexer' as a sister function.
-----------------------------------------------------}
lexer' [] = [EOF]
lexer' xs = let (t, rest) = lexToken xs
    in (t:lexer rest)


{----------------------------------------------------
    removeWhiteSpace:
    takes a string and strips off preceding
    whitespace.
-----------------------------------------------------}
removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace l@(x:xs) = case isSpace x of
    True -> removeWhiteSpace xs
    False -> l


{----------------------------------------------------
    lexToken:   
    Determines the first token on the string. The
    prefix matching the token is removed. The token
    and the remainder of the string is returned.
-----------------------------------------------------}
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


{----------------------------------------------------
    readString:
    Takes in as input a string with a double quoted
    string prefix. Strips the quotes and returns the
    quoted string and the remainder of the string.
-----------------------------------------------------}
readString :: String->(String, String)
readString [] = ([], [])
readString l@(x:xs) = let (quote, rest) = span isNotQuote xs
    in (quote, tail rest)


{----------------------------------------------------
    readNum:
    Takes a string and returns an IntTok or FloatTok
    token and the rest of the string.
-----------------------------------------------------}
readNum :: String->(Token, String)
readNum [] = (EOF, [])
readNum xs = let (i, rest0) = span isDigit xs
    in case rest0 of
        ('.':ps) -> let (f, rest1) = span isDigit ps
            in case rest1 of
                ('e':es) -> let (e, rest2) = readE rest1
                    in (FloatTok (read (i++"."++f++e)::Float), rest2)
                (_) -> (FloatTok (read (i++"."++f)::Float), rest1)
        ('e':[]) -> let (e, rest1) = readE rest0
            in (FloatTok (read (concat [i, e])::Float), rest1)
        ('e':es) -> if isAlpha (head es)
            then (IntTok (read i::Int), rest0)
            else let (e, rest1) = readE rest0
                in (FloatTok (read (concat [i, e])::Float), rest1)
        (_) -> (IntTok (read i::Int), rest0)


{----------------------------------------------------
    readE:
    Reads the exponent value off the beginning of a
    string. String should begin with 'e'.
-----------------------------------------------------}
readE :: String->(String, String)
readE [] = ([], [])
readE (x:[]) = ("e+0",[])
readE (x:y:[])
    | isDigit y = ("e+"++[y],[])
    | isSpace y = ("e+0",[])
    | otherwise = ([],[x,y])
readE (x:y:z:[])
    | isDigit y && isDigit z = ("e+"++[y,z],[])
    | y `elem` "+-" && isDigit z = ([x,y,z],[])
    | isSpace y = ("e+0",[z])
    | otherwise = ([],[x,y,z])
readE (x:y:z:xs)
    | isDigit y = let (exp, rest) = span isDigit (y:z:xs)
        in (("e+"++exp),rest)
    | y `elem` "+-" && isDigit z = let (exp, rest) = span isDigit (z:xs)
        in (([x,y]++exp),rest)
    | isSpace y = ("e+0",(z:xs))
    | otherwise = ("e+0",(y:z:xs))


{----------------------------------------------------
    readVarChar:
    Splits a string at the end of a variable name.
-----------------------------------------------------}
readVarChars :: String->(String, String)
readVarChars [] = ([], [])
readVarChars xs = span isVarChar xs


{----------------------------------------------------
    isVarChar:
    Returns true if the input character is a letter
    number or underscore.
-----------------------------------------------------}
isVarChar :: Char -> Bool
isVarChar x = (isAlpha x || x == '_')


{----------------------------------------------------
    isNotQuote:
    returns true if the input character is anything
    other than a double quote.
-----------------------------------------------------}
isNotQuote :: Char -> Bool
isNotQuote c = (c /= '"')


{----------------------------------------------------
    isOperator:
    Returns true if the input character is an element
    of the 'operators' list.
-----------------------------------------------------}
isOperator :: Char -> Bool
isOperator x = x `elem` (map fst operators)


{----------------------------------------------------
    isKeywords:
    Returns true if the input string matches an
    element of the 'keywords' list.
-----------------------------------------------------}
isKeywords :: String -> Bool
isKeywords x = (fst (readVarChars x) `elem` (map fst keywords))


{----------------------------------------------------
    Main:
    Takes a filename as an argument for scanning and
    returns a list of tokens.
-----------------------------------------------------}
main = do
    (fileName1:_) <- getArgs
    printWords fileName1


{----------------------------------------------------
    printWords:
    Display driver for our token list.
-----------------------------------------------------}
printWords :: FilePath -> IO ()
printWords source = do
    contents <- readFile source
    mapM_ putStrLn (map show (lexer contents))


{----------------------------------------------------
    Show definitions for Tokens
-----------------------------------------------------}
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
