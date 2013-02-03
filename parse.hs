import System.Environment
import Data.Char (isAlphaNum, toLower, isDigit, isSpace)

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
    ( "e", KW_Exp ),
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

isNotQuote :: Char -> Bool
isNotQuote c = (c /= '"')

isDigOrDec :: Char -> Bool
isDigOrDec c = (c == '.') || isDigit c

lexer :: String->[Token]
lexer xs = let s = removeWhiteSpace xs
    in lexer' s

lexer' [] = [EOF]
lexer' xs = let (t, rest) = lexToken xs
    in (t:lexer rest)

lexToken :: String->(Token, String)
lexToken [] = (EOF, [])
lexToken l@(x:xs)
    | (x == '"') = let (string, rest@(r:rs)) = readString xs
        in (StringTok string, rs)
    | isDigit x = let (num, rest) = readNum l
        in case isFloat num of
            True -> (FloatTok (read num::Float), rest)
            False -> (IntTok (read num::Int), rest)
    | isOperator x = ((map snd (filter ((==x).fst) operators)) !! 0, xs)
    | isKeywords l = let (string, rest) = readWord l
        in ((map snd (filter ((==string).fst) keywords)) !! 0, rest)
    | isIdentChar x = let (string, rest) = readIdentifier l
        in (VarId string, rest)
    | otherwise = (Error "Unknown character", xs)

removeWhiteSpace [] = []
removeWhiteSpace l@(x:xs) = case isSpace x of
    True -> removeWhiteSpace xs
    False -> l

readString :: String->(String, String)
readString [] = ([], [])
readString xs = span isNotQuote xs

readNum :: String->(String, String)
readNum [] = ([], [])
readNum xs = span isDigOrDec xs

readIdentifier :: String->(String, String)
readIdentifier [] = ([], [])
readIdentifier xs = span isIdentChar xs 

isIdentChar :: Char -> Bool
isIdentChar x = (isAlphaNum x || x == '_') 

isFloat :: String->Bool
isFloat [] = False 
isFloat l@(x:xs) = case (x == '.') of
    True -> True
    False -> isFloat xs

readWord :: String->(String, String)
readWord [] = ([], [])
readWord xs = span isIdentChar xs

isOperator :: Char -> Bool
isOperator x = x `elem` (map fst operators)

isKeywords :: String -> Bool
isKeywords x = (fst (readWord x) `elem` (map fst keywords))

main = do
    (fileName1:_) <- getArgs
    printWords fileName1

printWords :: FilePath -> IO ()
printWords source = do
    contents <- readFile source
    mapM_ putStrLn (map show (lexer contents))

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