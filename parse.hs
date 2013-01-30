import System.Environment
import qualified Data.ByteString.Lazy as B

import Data.Char (isAlpha, isLower, isUpper, toLower,
                  isDigit, isHexDigit, isOctDigit, isSpace,
                  ord, chr, digitToInt)
import qualified Data.Char (isSymbol)

data Token
        = VarId String
        | IntTok Int
        | FloatTok Rational
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

        | EOF
        | Error
        deriving (Eq,Show)

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
    ( "exp", KW_Exp ),
    ( "if", KW_If ),
    ( "let", KW_Let ),
    ( "logn", KW_Logn ),
    ( "println", KW_PrintLn ),
    ( "sin", KW_Sin ),
    ( "tan", KW_Tan ),
    ( "while", KW_While )
    ]

isSymbol :: Char -> Bool
isSymbol c = c `elem` "+-*/%^=<"

lexer :: String->[Token]
lexer xs = let s = removeWhiteSpace xs
    in lexer' s

lexer' [] = [EOF]
lexer' xs = let (t, rest) = lexToken xs
    in (t:lexer rest)

lexToken :: String->(Token, String)
lexToken [] = (EOF, [])
lexToken l@(x:xs)
    | isDigit x = let (num, rest) = readNum l
        in case rest of
            ('.':d:_) | isDigit d -> do
                (IntTok num, rest)
            _ -> (IntTok num, rest)
    | isAlpha x = let (string, rest) = readIdentifier l
        in (VarId string, rest)
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
            _ -> (Error, xs)
    | otherwise = (Error, xs)

removeWhiteSpace [] = []
removeWhiteSpace l@(x:xs) = case isSpace x of
    True -> removeWhiteSpace xs
    False -> l

readNum :: String->(Int, String)
readNum [] = (0, [])
readNum xs = let (num, rest) = span isDigit xs
    in (read num::Int, rest)

readIdentifier :: String->(String, String)
readIdentifier [] = ([], [])
readIdentifier xs = span isAlpha xs 

main = do
    (fileName1:_) <- getArgs
    printWords fileName1

printWords :: FilePath -> IO ()
printWords source = do
    contents <- readFile source
    putStrLn (show (lexer contents))
