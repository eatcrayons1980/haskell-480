{----------------------------------------------------
    CS480 - IBTL Compiler
    Authors: Paul Freeman
-----------------------------------------------------}
module Scanner where

--import System.Environment
import Data.Char (isAlpha, isDigit, isSpace)

data Token
    = T_LeftBracket
    | T_RightBracket
    | T_Assignment
    | T_AssignmentError
    | T_Add
    | T_Minus
    | T_Multiply
    | T_Divide
    | T_Mod
    | T_Exponentiate
    | T_Equal
    | T_Greater
    | T_GreaterEq
    | T_Less
    | T_LessEq
    | T_NotEq
    | T_NotEqError
    | T_Or
    | T_And
    | T_Not
    | T_Sin
    | T_Cos
    | T_Tan
    | T_StringVal String
    | T_StringValError String
    | T_True
    | T_False
    | T_NameVal String
    | T_IntVal String
    | T_FloatVal String
    | T_FloatValFrac String
    | T_FloatValError String
    | T_Decimal
    | T_Stdout
    | T_If
    | T_While
    | T_Let
    | T_Bool
    | T_Int
    | T_Float
    | T_String
    | T_EOF
    | T_Error String
    | T_Empty
    deriving (Eq)

{----------------------------------------------------
    lexer:
    Is the main tokenizing function. It takes a
    string and transforms it into a list of
    tokens.
-----------------------------------------------------}
lexer :: String -> [Token]
lexer xs = lexer' T_Empty xs

{----------------------------------------------------
    lexer':
    Called by 'lexer' as a sister function.
-----------------------------------------------------}
lexer' :: Token -> String -> [Token]
-- T_Empty
lexer' T_Empty []       = []
lexer' T_Empty ('[':xs) = T_LeftBracket  : lexer' T_Empty xs
lexer' T_Empty (']':xs) = T_RightBracket : lexer' T_Empty xs
lexer' T_Empty (':':xs) =                  lexer' T_AssignmentError xs
lexer' T_Empty ('+':xs) = T_Add          : lexer' T_Empty xs
lexer' T_Empty ('-':xs) = T_Minus        : lexer' T_Empty xs
lexer' T_Empty ('*':xs) = T_Multiply     : lexer' T_Empty xs
lexer' T_Empty ('/':xs) = T_Divide       : lexer' T_Empty xs
lexer' T_Empty ('%':xs) = T_Mod          : lexer' T_Empty xs
lexer' T_Empty ('^':xs) = T_Exponentiate : lexer' T_Empty xs
lexer' T_Empty ('=':xs) = T_Equal        : lexer' T_Empty xs
lexer' T_Empty ('>':xs) =                  lexer' T_Greater xs
lexer' T_Empty ('<':xs) =                  lexer' T_Less xs
lexer' T_Empty ('!':xs) =                  lexer' T_NotEqError xs
lexer' T_Empty ('"':xs) =                  lexer' (T_StringValError []) xs
lexer' T_Empty ('_':xs) =                  lexer' (T_NameVal "_") xs
lexer' T_Empty ('.':xs) =                  lexer' T_Decimal xs
lexer' T_Empty (x:xs)
    | isSpace x = lexer' T_Empty xs
    | isAlpha x = lexer' (T_NameVal [x]) xs
    | isDigit x = lexer' (T_IntVal [x]) xs
    | otherwise = (T_Error [x]) : lexer' T_Empty xs
-- T_AssignmentError
lexer' T_AssignmentError ('=':xs) = T_Assignment : lexer' T_Empty xs
lexer' T_AssignmentError xs       = T_Error ":"  : lexer' T_Empty xs
-- T_Greater
lexer' T_Greater ('=':xs) = T_GreaterEq : lexer' T_Empty xs
lexer' T_Greater xs       = T_Greater   : lexer' T_Empty xs
-- T_Less
lexer' T_Less ('=':xs) = T_LessEq : lexer' T_Empty xs
lexer' T_Less xs       = T_Less   : lexer' T_Empty xs
-- T_NotEqError
lexer' T_NotEqError ('=':xs) = T_NotEq     : lexer' T_Empty xs
lexer' T_NotEqError xs       = T_Error "!" : lexer' T_Empty xs
-- T_StringValError
lexer' (T_StringValError s) ('"':xs) = T_StringVal s : lexer' T_Empty xs
lexer' (T_StringValError s) (x:xs)   = lexer' (T_StringValError (s++[x])) xs
-- T_NameVal
lexer' (T_NameVal s) ('_':xs) = lexer' (T_NameVal (s++"_")) xs
lexer' (T_NameVal s) l@(x:xs)
    | isAlpha x     =                 lexer' (T_NameVal (s++[x])) xs
    | isDigit x     =                 lexer' (T_NameVal (s++[x])) xs
    | s == "or"     = T_Or          : lexer' T_Empty l
    | s == "and"    = T_And         : lexer' T_Empty l
    | s == "not"    = T_Not         : lexer' T_Empty l
    | s == "sin"    = T_Sin         : lexer' T_Empty l
    | s == "cos"    = T_Cos         : lexer' T_Empty l
    | s == "tan"    = T_Tan         : lexer' T_Empty l
    | s == "true"   = T_True        : lexer' T_Empty l
    | s == "false"  = T_False       : lexer' T_Empty l
    | s == "stdout" = T_Stdout      : lexer' T_Empty l
    | s == "if"     = T_If          : lexer' T_Empty l
    | s == "while"  = T_While       : lexer' T_Empty l
    | s == "let"    = T_Let         : lexer' T_Empty l
    | s == "bool"   = T_Bool        : lexer' T_Empty l
    | s == "int"    = T_Int         : lexer' T_Empty l
    | s == "float"  = T_Float       : lexer' T_Empty l
    | s == "string" = T_String      : lexer' T_Empty l
    | otherwise     = (T_NameVal s) : lexer' T_Empty l
-- T_Decimal
lexer' T_Decimal l@(x:xs)
    | isDigit x = lexer' (T_FloatValFrac ('.':[x])) xs
    | otherwise = T_Decimal : lexer' T_Empty l
-- T_IntVal
lexer' (T_IntVal s) ('.':xs) = lexer' (T_FloatValFrac (s++".")) xs
lexer' (T_IntVal s) ('e':xs) = lexer' (T_FloatValError (s++"e")) xs
lexer' (T_IntVal s) l@(x:xs)
    | isDigit x = lexer' (T_IntVal (s++[x])) xs
    | otherwise = (T_IntVal s) : lexer' T_Empty l
-- T_FloatValFrac
lexer' (T_FloatValFrac s) []       = [T_FloatVal s]
lexer' (T_FloatValFrac s) ('e':xs) = lexer' (T_FloatValError (s++"e")) xs
lexer' (T_FloatValFrac s) l@(x:xs)
    | isDigit x = lexer' (T_FloatValFrac (s++[x])) xs
    | otherwise = (T_FloatVal s) : lexer' T_Empty l
-- T_FloatValError
lexer' (T_FloatValError s) ('+':xs) = lexer' (T_FloatValError (s++"+")) xs
lexer' (T_FloatValError s) ('-':xs) = lexer' (T_FloatValError (s++"-")) xs
lexer' (T_FloatValError s) l@(x:xs)
    | isDigit x = lexer' (T_FloatVal (s++[x])) xs
    | otherwise = (T_FloatValError s) : lexer' T_Empty l
-- T_FloatVal
lexer' (T_FloatVal s) l@(x:xs)
    | isDigit x = lexer' (T_FloatVal (s++[x])) xs
    | otherwise = (T_FloatVal s) : lexer' T_Empty l
-- Catch
lexer' t [] = [t]

{----------------------------------------------------
    Show definitions for Tokens
-----------------------------------------------------}
instance Show Token where
    show T_LeftBracket  = "["
    show T_RightBracket = "]"
    show T_Assignment   = ":="
    show T_AssignmentError = "Error :"
    show T_Add = "+"
    show T_Minus = "-"
    show T_Multiply = "*"
    show T_Divide = "/"
    show T_Mod = "%"
    show T_Exponentiate = "^"
    show T_Equal = "="
    show T_Greater = "<"
    show T_GreaterEq = ">="
    show T_Less = "<"
    show T_LessEq = "<="
    show T_NotEq = "!="
    show T_NotEqError = "Error !"
    show T_Or = "or"
    show T_And = "and"
    show T_Not = "not"
    show T_Sin = "sin"
    show T_Cos = "cos"
    show T_Tan = "tan"
    show (T_StringVal s) = "String "++show s
    show (T_StringValError s) = "String Error "++show s
    show T_True = "true"
    show T_False = "false"
    show (T_NameVal s) = "Name "++show s
    show (T_IntVal s) = "Int "++show s
    show (T_FloatVal s) = "Float "++show s
    show (T_FloatValFrac s) = "Float Error "++show s
    show (T_FloatValError s) = "Float Error "++show s
    show T_Decimal = "Decimal Error ."
    show T_Stdout = "stdout"
    show T_If = "if"
    show T_While = "while"
    show T_Let = "let"
    show T_Bool = "bool"
    show T_Int = "int"
    show T_Float = "float"
    show T_String = "string"
    show T_EOF = "<EOF>"
    show (T_Error s) = "Error "++show s
    show T_Empty = ""
