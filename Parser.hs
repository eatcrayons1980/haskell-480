module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Data.List
import Data.Ord

type OurParser a b = GenParser Token a b
data Tree = EmptyTree | Node (String, [Tree]) deriving Show

{- Grammar -}
-- T -> [S]
t = do
    parseLeftBracket <?> "["
    x<-s
    parseRightBracket <?> "]"
    return $ Node ("TOP", x)
-- S -> []S' | [S]S' | exprS'
s = try (do
    parseLeftBracket <?> "["
    parseRightBracket <?> "]"
    x<-s'
    return x)
    <|>
    try (do
    parseLeftBracket <?> "["
    x<-s
    parseRightBracket <?> "]"
    y<-s'
    return $ x++y)
    <|>
    do
    x<-expr
    y<-s'
    return $ x:y
-- S' -> SS' | Empty
s' = try (do
    x<-s
    y<-s'
    return $ x++y)
    <|>
    return []
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
    parseLeftBracket <?> "["
    parseAssignment <?> ":="
    x<-name
    y<-oper
    parseRightBracket <?> "]"
    return $ Node (":=", [Node (x,[]),y]))
    <|>
    try (do
    parseLeftBracket <?> "["
    w<-binops
    x<-oper
    y<-oper
    parseRightBracket <?> "]"
    return $ Node (w, [x,y]))
    <|>
    try (do
    parseLeftBracket <?> "["
    x<-unops
    y<-oper
    parseRightBracket <?> "]"
    return $ Node (x, [y]))
    <|>
    try (do
    x<-constants
    return $ Node (x,[]))
    <|>
    do
    x<-name
    return $ Node (x,[])
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
    parseLeftBracket <?> "["
    parseStdout <?> "stdout"
    x<-oper
    parseRightBracket <?> "}"
    return $ Node ("print", [x])
-- ifstmts -> [if expr expr expr] | [if expr expr]
ifstmts = try (do
    parseLeftBracket <?> "["
    parseIf <?> "if"
    w<-expr
    x<-expr
    y<-expr
    parseRightBracket <?> "]"
    return $ Node ("if", [w,x,y]))
    <|>
    do
    parseLeftBracket <?> "["
    parseIf <?> "if"
    x<-expr
    y<-expr
    parseRightBracket <?> "]"
    return $ Node ("if", [x,y])
-- whilestmts -> [while expr exprlist]
whilestmts = do
    parseLeftBracket <?> "["
    parseWhile <?> "while"
    x<-expr
    y<-exprlist
    parseRightBracket <?> "]"
    return $ Node ("while", x:y)
-- exprlist -> expr | expr exprlist
exprlist = try (do
    x<-expr
    return [x])
    <|>
    do
    x<-expr
    y<-exprlist
    return $ x:y
-- letstmts = [let [varlist]]
letstmts = do
    parseLeftBracket <?> "["
    parseLet <?> "let"
    parseLeftBracket <?> "["
    x<-varlist
    parseRightBracket <?> "]"
    parseRightBracket <?> "]"
    return $ Node ("let", x)
-- varlist -> [name type] | [name type] varlist
varlist = try (do
    parseLeftBracket <?> "["
    x<-name
    y<-types
    parseRightBracket <?> "]"
    return $ [Node (y, [Node (x,[])])])
    <|>
    do
    parseLeftBracket <?> "["
    w<-name
    x<-types
    parseRightBracket <?> "]"
    y<-varlist
    return $ Node (x, [Node (w,[])]) : y
-- types -> bool | int | float | string
types = do
    x<-parseType <?> "type"
    return x

{- Parsers -}
parseLeftBracket = do
    i <- getState
    mytoken (\t -> case t of T_LeftBracket -> Just "["
                             other         -> Nothing)
parseRightBracket = do
    i <- getState
    mytoken (\t -> case t of T_RightBracket -> Just "]"
                             other          -> Nothing)
parseAssignment = do
    i <- getState
    mytoken (\t -> case t of T_Assignment   -> Just ":="
                             other          -> Nothing)
parseBinOp = do
    i <- getState
    mytoken (\t -> case t of T_Add          -> Just "+"
                             T_Minus        -> Just "-"
                             T_Multiply     -> Just "*"
                             T_Divide       -> Just "/"
                             T_Mod          -> Just "%"
                             T_Exponentiate -> Just "^"
                             T_Equal        -> Just "="
                             T_Greater      -> Just ">"
                             T_GreaterEq    -> Just ">="
                             T_Less         -> Just "<"
                             T_LessEq       -> Just "<="
                             T_NotEq        -> Just "!="
                             T_Or           -> Just "or"
                             T_And          -> Just "and"
                             other          -> Nothing)
parseUnOp = do
    i <- getState
    mytoken (\t -> case t of T_Minus        -> Just "-"
                             T_Not          -> Just "not"
                             T_Sin          -> Just "sin"
                             T_Cos          -> Just "cos"
                             T_Tan          -> Just "tan"
                             other          -> Nothing)
parseStringValue = do
    i <- getState
    mytoken (\t -> case t of (T_StringVal s)-> Just s
                             other          -> Nothing)
parseBoolValue = do
    i <- getState
    mytoken (\t -> case t of T_True         -> Just "true"
                             T_False        -> Just "false"
                             other          -> Nothing)
parseNameValue = do
    i <- getState
    mytoken (\t -> case t of (T_NameVal s)  -> Just s
                             other          -> Nothing)
parseIntValue = do
    i <- getState
    mytoken (\t -> case t of (T_IntVal s)   -> Just s
                             other          -> Nothing)
parseFloatValue = do
    i <- getState
    mytoken (\t -> case t of (T_FloatVal s) -> Just s
                             other          -> Nothing)
parseStdout = do
    i <- getState
    mytoken (\t -> case t of T_Stdout       -> Just "stdout"
                             other          -> Nothing)
parseIf = do
    i <- getState
    mytoken (\t -> case t of T_If           -> Just "if"
                             other          -> Nothing)
parseWhile = do
    i <- getState
    mytoken (\t -> case t of T_While        -> Just "while"
                             other          -> Nothing)
parseLet = do
    i <- getState
    mytoken (\t -> case t of T_Let          -> Just "let"
                             other          -> Nothing)
parseType = do
    i <- getState
    mytoken (\t -> case t of T_Bool         -> Just "bool"
                             T_Int          -> Just "int"
                             T_Float        -> Just "float"
                             T_String       -> Just "string"
                             other          -> Nothing)

{- Helpers -}
mytoken test = tokenPrim show update_pos test

update_pos pos _ _ = newPos "" 0 0

spaces' = repeat ' '
moreLimbs = not . (all (==' '))

indent n = take n spaces'

printTree EmptyTree      = repeat ""
printTree (Node (s, [])) = takeWhile moreLimbs $ (" "++s++" ") : repeat (take (length s + 2) spaces')
printTree (Node (s, t))  = takeWhile moreLimbs $ first : map padding rest
    where
        rest      = foldl1 (mymerge) (map printTree t)
        padA      = max 2 $ (length $ head rest) - (length s)
        padB      = max 0 $ (length s + 2) - (length $ head rest)
        first     = take (padA `div` 2) spaces' ++ s ++ take (padA - (padA `div` 2)) spaces'
        padding x = take (padB `div` 2) spaces' ++ x ++ take (padB - (padB `div` 2)) spaces'

mymerge :: [String] -> [String] -> [String]
mymerge []       []        = []
mymerge (a:as)   []        = [a]
mymerge []       (x:xs)    = [x]
mymerge (a:[])   (x:[])    = (a++x) : mymerge [take (length a) spaces'] [take (length x) spaces']
mymerge (a:[])   (x:y:ys)  = (a++x) : mymerge [take (length a) spaces'] (y:ys)
mymerge (a:b:bs) (x:[])    = (a++x) : mymerge (b:bs) [take (length x) spaces']
mymerge (a:b:bs) (x:y:ys)  = (a++x) : mymerge (b:bs) (y:ys)

{- main -}
main = do
    files <- getArgs
    flip mapM_ (concatMap lines files) $ \file -> do 
      contents <- readFile file
      case (runParser t 0 file $ lexer contents) of Left err -> print err
                                                    Right xs -> mapM_ putStrLn $ printTree xs
