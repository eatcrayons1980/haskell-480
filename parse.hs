import System.Environment
import qualified Data.ByteString.Lazy as B

import Data.Char (isAlpha, isAlphaNum, isLower, isUpper, toLower,
                  isDigit, isHexDigit, isOctDigit, isSpace,
                  ord, chr, digitToInt)
import qualified Data.Char (isSymbol)
import Data.List (elemIndex)
import Data.Maybe(fromJust)

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

        | KW_int
        | KW_float
        | KW_bool
        | KW_string        

        | EOF
        | Error String
        deriving (Eq)

operators :: [(String,Token)]
operators = [
    ( "-", Minus ),
    ( "+", Plus ),
    ( "%", Mod ),
    ( "^", Carrot ),
    ( "*", Mult ),
    ( "/", Div ),
    ( "=", Equal ),
    ( "<", Less )
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
    ( "int", KW_int ),
    ( "float", KW_float ),
    ( "string", KW_string ),
    ( "bool", KW_bool ),
    ( "true", BoolTok True),
    ( "false", BoolTok False)
    ]

isSymbol :: Char -> Bool
isSymbol c = c `elem` "+-*/%^=<()"

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
    | isSymbol x = do
        case x of
            ('+') -> (Plus, xs)
            ('-') -> (Minus, xs)
            ('*') -> (Mult, xs)
            ('/') -> (Div, xs)
            ('%') -> (Mod, xs)
            ('^') -> (Carrot, xs)
            ('=') -> (Equal, xs)
            ('<') -> (Less, xs)
            ('(') -> (LeftParen, xs)
            (')') -> (RightParen, xs)
    | isKeywords l = let (string, rest) = readWord l
        in ((map snd (filter ((==string).fst) keywords)) !! 0, rest)
    | isAlpha x = let (string, rest) = readIdentifier l
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
readWord xs = span isAlpha xs

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
    show (VarId x)                = "Variable: \t"++show x
    show (IntTok x)               = "Integer: \t"++show x
    show (FloatTok x)             = "Float: \t\t"++show x
    show (StringTok x)            = "String: \t"++show x
    show (BoolTok x)              = "Bool: \t\t"++show x
    show (LeftParen)              = "Separator: \t("
    show (RightParen)             = "Separator: \t)"
    show (Minus)                  = "Arith Op: \t-"
    show (Plus)                   = "Arith Op: \t+"
    show (Mod)                    = "Arith Op: \t%"
    show (Carrot)                 = "Arith Op: \t^"
    show (Mult)                   = "Arith Op: \t*"
    show (Div)                    = "Arith Op: \t/"
    show (Equal)                  = "Logic Op: \t="
    show (Less)                   = "Logic Op: \t<"
    show (KW_And)                 = "Logic Op: \t&&"
    show (KW_Or)                  = "Logic Op: \t||"
    show (KW_Not)                 = "Logic Op: \t!"
    show (KW_Iff)                 = "Logic Op: \tIff"
    show (KW_Assign)              = "Keyword: \tassign"
    show (KW_Cos)                 = "Keyword: \tcos"
    show (KW_Exp)                 = "Keyword: \te"
    show (KW_If)                  = "Keyword: \tif"
    show (KW_Let)                 = "Keyword: \tlet"
    show (KW_Logn)                = "Keyword: \tlogn"
    show (KW_PrintLn)             = "Keyword: \tprintln"
    show (KW_Sin)                 = "Keyword: \tsin"
    show (KW_Tan)                 = "Keyword: \ttan"
    show (KW_While)               = "Keyword: \twhile"
    show (KW_int)                 = "Keyword: \tint"
    show (KW_float)               = "Keyword: \tfloat"
    show (KW_bool)                = "Keyword: \tbool"
    show (KW_string)              = "Keyword: \tstring"
    show (EOF)                    = "EOF"
    show (Error x)                = "Error: \t\t"++show x
        