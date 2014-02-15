module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String

type OurParser a b = GenParser Token a b

{- Grammar -}
-- T -> [S]
t = do
    x<-parseLeftBracket <?> "["
    y<-s
    z<-parseRightBracket <?> "]"
    return (x++y++z)
-- S -> []S' | [S]S' | exprS'
s = try (do
    x<-parseLeftBracket <?> "["
    y<-parseRightBracket <?> "]"
    z<-s'
    return (x++y++z))
    <|>
    try (do
    w<-parseLeftBracket <?> "["
    x<-s
    y<-parseRightBracket <?> "]"
    z<-s'
    return (w++x++y++z))
    <|>
    do
    x<-expr
    y<-s'
    return (x++y)
-- S' -> SS' | Empty
s' = try (do
    x<-s
    y<-s'
    return (x++y))
    <|>
    return "" -- epsilon
-- expr -> oper | stmts
expr = try (do
    x<-oper
    return x)
    <|>
    do
    x<-stmts
    return x
-- oper -> [:= name oper] | [binops oper oper] | [unops oper] | constants | name
oper = try (do
    v<-parseLeftBracket <?> "["
    w<-parseAssignment <?> ":="
    x<-name
    y<-oper
    z<-parseRightBracket <?> "]"
    return (v++w++x++y++z))
    <|>
    try (do
    v<-parseLeftBracket <?> "["
    w<-binops
    x<-oper
    y<-oper
    z<-parseRightBracket <?> "]"
    return (v++w++x++y++z))
    <|>
    try (do
    w<-parseLeftBracket <?> "["
    x<-unops
    y<-oper
    z<-parseRightBracket <?> "]"
    return (w++x++y++z))
    <|>
    try (do
    x<-constants
    return x)
    <|>
    do
    x<-name
    return x
-- binops -> + | - | * | / | % | ^ | = | > | >= | < | <= | != | or | and
binops = do
    x<-parseBinOp <?> "binary operator"
    return x
-- unops -> - | not | sin | cos | tan
unops = do
    x<-parseUnOp <?> "bnary operator"
    return x
-- constants -> strings | ints | floats
constants = try (do
    x<-strings
    return x)
    <|>
    try (do
    x<-ints
    return x)
    <|>
    do
    x<-floats
    return x
-- strings -> regex for str literal in C | true | false
strings = try (do
    x<-parseStringValue <?> "string value"
    return x)
    <|>
    do
    x<-parseBoolValue <?> "true/false"
    return x
-- name -> regex for ids in C
name = do
    x<-parseNameValue <?> "name value"
    return x
-- ints -> regex for pos/neg ints in C
ints = do
    x<-parseIntValue <?> "int value"
    return x
-- floats -> regex for pos/neg floats in C
floats = do
    x<-parseFloatValue <?> "float value"
    return x
-- stmts -> ifstmts | whilestmts | letstmts | printstmts
stmts = try (do
    x<-ifstmts
    return x)
    <|>
    try (do
    x<-whilestmts
    return x)
    <|>
    try (do
    x<-letstmts
    return x)
    <|>
    do
    x<-printstmts
    return x
-- printstmts -> [stdout oper]
printstmts = do
    w<-parseLeftBracket <?> "["
    x<-parseStdout <?> "stdout"
    y<-oper
    z<-parseRightBracket <?> "}"
    return (w++x++y++z)
-- ifstmts -> [if expr expr expr] | [if expr expr]
ifstmts = try (do
    u<-parseLeftBracket <?> "["
    v<-parseIf <?> "if"
    w<-expr
    x<-expr
    y<-expr
    z<-parseRightBracket <?> "]"
    return (u++v++w++x++y++z))
    <|>
    do
    v<-parseLeftBracket <?> "["
    w<-parseIf <?> "if"
    x<-expr
    y<-expr
    z<-parseRightBracket <?> "]"
    return (v++w++x++y++z)
-- whilestmts -> [while expr exprlist]
whilestmts = do
    v<-parseLeftBracket <?> "["
    w<-parseWhile <?> "while"
    x<-expr
    y<-exprlist
    z<-parseRightBracket <?> "]"
    return (v++w++x++y++z)
-- exprlist -> expr | expr exprlist
exprlist = try (do
    x<-expr
    return x)
    <|>
    do
    x<-expr
    y<-exprlist
    return (x++y)
-- letstmts = [let [varlist]]
letstmts = do
    u<-parseLeftBracket <?> "["
    v<-parseLet <?> "let"
    w<-parseLeftBracket <?> "["
    x<-varlist
    y<-parseRightBracket <?> "]"
    z<-parseRightBracket <?> "]"
    return (u++v++w++x++y++z)
-- varlist -> [name type] | [name type] varlist
varlist = try (do
    w<-parseLeftBracket <?> "["
    x<-name
    y<-types
    z<-parseRightBracket <?> "]"
    return (w++x++y++z))
    <|>
    do
    v<-parseLeftBracket <?> "["
    w<-name
    x<-types
    y<-parseRightBracket <?> "]"
    z<-varlist
    return (v++w++x++y++z)
-- types -> bool | int | float | string
types = try (do
    x<-parseBool <?> "bool"
    return x)
    <|>
    try (do
    x<-parseInt <?> "int"
    return x)
    <|>
    try (do
    x<-parseFloat <?> "float"
    return x)
    <|>
    do
    x<-parseString <?> "string"
    return x

{- Parsers -}
parseLeftBracket = do
    i <- getState
    mytoken (\t -> case t of T_LeftBracket -> Just(indent i++"[\n")
                             other         -> Nothing)
parseRightBracket = do
    i <- getState
    mytoken (\t -> case t of T_RightBracket -> Just(indent i++"]\n")
                             other          -> Nothing)
parseAssignment = do
    i <- getState
    mytoken (\t -> case t of T_Assignment   -> Just(indent i++":=\n")
                             other          -> Nothing)
parseBinOp = do
    i <- getState
    mytoken (\t -> case t of T_Add          -> Just(indent i++"+\n")
                             T_Minus        -> Just(indent i++"-\n")
                             T_Multiply     -> Just(indent i++"*\n")
                             T_Divide       -> Just(indent i++"/\n")
                             T_Mod          -> Just(indent i++"%\n")
                             T_Exponentiate -> Just(indent i++"^\n")
                             T_Equal        -> Just(indent i++"=\n")
                             T_Greater      -> Just(indent i++">\n")
                             T_GreaterEq    -> Just(indent i++">=\n")
                             T_Less         -> Just(indent i++"<\n")
                             T_LessEq       -> Just(indent i++"<=\n")
                             T_NotEq        -> Just(indent i++"!=\n")
                             T_Or           -> Just(indent i++"or\n")
                             T_And          -> Just(indent i++"and\n")
                             other          -> Nothing)
parseUnOp = do
    i <- getState
    mytoken (\t -> case t of T_Minus        -> Just(indent i++"-\n")
                             T_Not          -> Just(indent i++"not\n")
                             T_Sin          -> Just(indent i++"sin\n")
                             T_Cos          -> Just(indent i++"cos\n")
                             T_Tan          -> Just(indent i++"tan\n")
                             other          -> Nothing)
parseStringValue = do
    i <- getState
    mytoken (\t -> case t of (T_StringVal s)-> Just(indent i++s++"\n")
                             other          -> Nothing)
parseBoolValue = do
    i <- getState
    mytoken (\t -> case t of T_True         -> Just(indent i++"true\n")
                             T_False        -> Just(indent i++"false\n")
                             other          -> Nothing)
parseNameValue = do
    i <- getState
    mytoken (\t -> case t of (T_NameVal s)  -> Just(indent i++s++"\n")
                             other          -> Nothing)
parseIntValue = do
    i <- getState
    mytoken (\t -> case t of (T_IntVal s)   -> Just(indent i++s++"\n")
                             other          -> Nothing)
parseFloatValue = do
    i <- getState
    mytoken (\t -> case t of (T_FloatVal s) -> Just(indent i++s++"\n")
                             other          -> Nothing)
parseStdout = do
    i <- getState
    mytoken (\t -> case t of T_Stdout       -> Just(indent i++"stdout\n")
                             other          -> Nothing)
parseIf = do
    i <- getState
    mytoken (\t -> case t of T_If           -> Just(indent i++"if\n")
                             other          -> Nothing)
parseWhile = do
    i <- getState
    mytoken (\t -> case t of T_While        -> Just(indent i++"while\n")
                             other          -> Nothing)
parseLet = do
    i <- getState
    mytoken (\t -> case t of T_Let          -> Just(indent i++"let\n")
                             other          -> Nothing)
parseBool = do
    i <- getState
    mytoken (\t -> case t of T_Bool         -> Just(indent i++"bool\n")
                             other          -> Nothing)
parseInt = do
    i <- getState
    mytoken (\t -> case t of T_Int          -> Just(indent i++"int\n")
                             other          -> Nothing)
parseFloat = do
    i <- getState
    mytoken (\t -> case t of T_Float        -> Just(indent i++"float\n")
                             other          -> Nothing)
parseString = do
    i <- getState
    mytoken (\t -> case t of T_String       -> Just(indent i++"string\n")
                             other          -> Nothing)
{- Helpers -}
mytoken test = tokenPrim show update_pos test

update_pos pos _ _ = newPos "" 0 0

indent n = take n (repeat ' ')

{- main -}
main = do
    files <- getArgs
    flip mapM_ files $ \file -> do 
      contents <- readFile file
      case (runParser t 0 file $ lexer contents) of Left err -> print err
                                                    Right xs -> putStr xs
