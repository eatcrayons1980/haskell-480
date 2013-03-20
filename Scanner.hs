{----------------------------------------------------
    CS480 - IBTL Compiler
    Authors: Kevin Tang, Kyle Mannari, Paul Freeman
-----------------------------------------------------}
module Scanner where

--import System.Environment
import Data.Char (isAlpha, isDigit, isSpace)

data Token
    = VarId String
    | IntTok String
    | FloatTok String
    | StringTok String
    | BoolTok String

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
    | Greater

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
    | KW_Print
    | KW_Sin
    | KW_Tan
    | KW_While

    | KW_Int
    | KW_Float
    | KW_Bool
    | KW_String        

    | EOF
    | Epsilon
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
    ( '>', Greater ),
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
    ( "print", KW_Print ),
    ( "sin", KW_Sin ),
    ( "tan", KW_Tan ),
    ( "whle", KW_While ),
    ( "int", KW_Int ),
    ( "float", KW_Float ),
    ( "string", KW_String ),
    ( "bool", KW_Bool ),
    ( "true", BoolTok "true"),
    ( "false", BoolTok "false")
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
lexer' ::  String -> [Token]
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
removeWhiteSpace l@(x:xs) = if isSpace x then removeWhiteSpace xs else l

{----------------------------------------------------
    lexToken:   
    Determines the first token on the string. The
    prefix matching the token is removed. The token
    and the remainder of the string is returned.
-----------------------------------------------------}
lexToken :: String->(Token, String)
lexToken [] = (EOF, [])
lexToken l@(x:xs)
    | isQuote = let (s, r) = span (/='"') xs
                    in (StringTok s, tail r)
    | isOperator = (head (map snd (filter ((== x) . fst) operators)), xs)
    | isKeyword = (head (map snd (filter ((== first) . fst) keywords)), rest)
    | isDigit x = readNum l
    | isVariable = (VarId first, rest)
    | otherwise = (Error "Unknown character", xs)
    where
        isQuote = x=='"'
        isOperator = x `elem` map fst operators
        isVariable = isAlpha x || x=='_'
        isKeyword = first `elem` map fst keywords
        (first, rest) = span (\v->isAlpha v || isDigit v || v=='_') l

-- We can merge all of the "is" prefixed variables in where into the guards
-- but I left it for readability.

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
                ('e':_) -> let (e, rest2) = readE rest1
                    in (FloatTok (i++"."++f++e), rest2)
                (_) -> (FloatTok (i++"."++f++"e"), rest1)
        ('e':[]) -> let (e, rest1) = readE rest0
            in (FloatTok (i++e), rest1)
        ('e':es) -> if isAlpha (head es)
            then (IntTok i, rest0)
            else let (e, rest1) = readE rest0
                in (FloatTok (i++e), rest1)
        (_) -> (IntTok i, rest0)


{----------------------------------------------------
    readE:
    Reads the exponent value off the beginning of a
    string. String should begin with 'e'.
-----------------------------------------------------}
readE :: String->(String, String)
readE [] = ([], [])
readE (_:[]) = ("e+0",[])
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
    | isDigit y = let (f, rest) = span isDigit (y:z:xs)
        in ("e+"++f,rest)
    | y `elem` "+-" && isDigit z = let (f, rest) = span isDigit (z:xs)
        in ([x,y]++f,rest)
    | isSpace y = ("e+0", z:xs)
    | otherwise = ("e+0", y:z:xs)

{----------------------------------------------------
    Main:
    Takes a filename as an argument for scanning and
    returns a list of tokens.
main = do
    (fileName1:_) <- getArgs
    printWords fileName1
-----------------------------------------------------}


{----------------------------------------------------
    printWords:
    Display driver for our token list.

printWords :: FilePath -> IO ()
printWords source = do
    contents <- readFile source
    mapM_ putStrLn (map show $ lexer contents)
-----------------------------------------------------}

{----------------------------------------------------
    Show definitions for Tokens
-----------------------------------------------------}
instance Show Token where
    show (VarId x)     = x
    show (IntTok x)    = x
    show (FloatTok x)  = x
    show (StringTok x) = x
    show (BoolTok x)   = x
    show (LeftParen)   = "("
    show (RightParen)  = ")"
    show (Minus)       = "-"
    show (Plus)        = "+"
    show (Mod)         = "mod"
    show (Carrot)      = "**"
    show (Mult)        = "*"
    show (Div)         = "/"
    show (Equal)       = "="
    show (Less)        = "<"
    show (Greater)     = ">"
    show (KW_And)      = "and"
    show (KW_Or)       = "or"
    show (KW_Not)      = "invert"
    show (KW_Iff)      = "xor invert"
    show (KW_Assign)   = "assign"
    show (KW_Cos)      = "cos"
    show (KW_Exp)      = "exp"
    show (KW_If)       = "if"
    show (KW_Let)      = "let"
    show (KW_Logn)     = "logn"
    show (KW_Print)    = "print"
    show (KW_Sin)      = "sin"
    show (KW_Tan)      = "tan"
    show (KW_While)    = "while"
    show (KW_Int)      = "int"
    show (KW_Float)    = "float"
    show (KW_Bool)     = "bool"
    show (KW_String)   = "string"
    show (EOF)         = "EOF"
    show (Epsilon)     = ""
    show (Error x)     = x
