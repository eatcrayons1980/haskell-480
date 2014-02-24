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
    x<-parseLeftBracket <?> "["
    y<-s
    z<-parseRightBracket <?> "]"
    return $ Node ("t", [x,y,z])
-- S -> []S' | [S]S' | exprS'
s = try (do
    x<-parseLeftBracket <?> "["
    y<-parseRightBracket <?> "]"
    z<-s'
    return $ Node ("s", [x,y,z]))
    <|>
    try (do
    w<-parseLeftBracket <?> "["
    x<-s
    y<-parseRightBracket <?> "]"
    z<-s'
    return $ Node ("s", [w,x,y,z]))
    <|>
    do
    x<-expr
    y<-s'
    return $ Node ("s", [x,y])
-- S' -> SS' | Empty
s' = try (do
    x<-s
    y<-s'
    return $ Node ("s'", [x,y]))
    <|>
    return EmptyTree
-- expr -> oper | stmts
expr = try (do
    x<-oper
    return $ Node ("expr", [x]))
    <|>
    do
    x<-stmts
    return $ Node ("expr", [x])
-- oper -> [:= name oper] | [binops oper oper] | [unops oper] | constants | name
oper = try (do
    v<-parseLeftBracket <?> "["
    w<-parseAssignment <?> ":="
    x<-name
    y<-oper
    z<-parseRightBracket <?> "]"
    return $ Node ("oper", [v,w,x,y,z]))
    <|>
    try (do
    v<-parseLeftBracket <?> "["
    w<-binops
    x<-oper
    y<-oper
    z<-parseRightBracket <?> "]"
    return $ Node ("oper", [v,w,x,y,z]))
    <|>
    try (do
    w<-parseLeftBracket <?> "["
    x<-unops
    y<-oper
    z<-parseRightBracket <?> "]"
    return $ Node ("oper", [w,x,y,z]))
    <|>
    try (do
    x<-constants
    return $ Node ("oper", [x]))
    <|>
    do
    x<-name
    return $ Node ("oper", [x])
-- binops -> + | - | * | / | % | ^ | = | > | >= | < | <= | != | or | and
binops = do
    x<-parseBinOp <?> "binary operator"
    return $ Node ("binop", [x])
-- unops -> - | not | sin | cos | tan
unops = do
    x<-parseUnOp <?> "bnary operator"
    return $ Node ("unop", [x])
-- constants -> strings | ints | floats
constants = try (do
    x<-strings
    return $ Node ("constant", [x]))
    <|>
    try (do
    x<-ints
    return $ Node ("constant", [x]))
    <|>
    do
    x<-floats
    return $ Node ("constant", [x])
-- strings -> regex for str literal in C | true | false
strings = try (do
    x<-parseStringValue <?> "string value"
    return $ Node ("string", [x]))
    <|>
    do
    x<-parseBoolValue <?> "true/false"
    return $ Node ("string", [x])
-- name -> regex for ids in C
name = do
    x<-parseNameValue <?> "name value"
    return $ Node ("name", [x])
-- ints -> regex for pos/neg ints in C
ints = do
    x<-parseIntValue <?> "int value"
    return $ Node ("int", [x])
-- floats -> regex for pos/neg floats in C
floats = do
    x<-parseFloatValue <?> "float value"
    return $ Node ("float", [x])
-- stmts -> ifstmts | whilestmts | letstmts | printstmts
stmts = try (do
    x<-ifstmts
    return $ Node ("stmt", [x]))
    <|>
    try (do
    x<-whilestmts
    return $ Node ("stmt", [x]))
    <|>
    try (do
    x<-letstmts
    return $ Node ("stmt", [x]))
    <|>
    do
    x<-printstmts
    return $ Node ("stmt", [x])
-- printstmts -> [stdout oper]
printstmts = do
    w<-parseLeftBracket <?> "["
    x<-parseStdout <?> "stdout"
    y<-oper
    z<-parseRightBracket <?> "}"
    return $ Node ("printstmt", [w,x,y,z])
-- ifstmts -> [if expr expr expr] | [if expr expr]
ifstmts = try (do
    u<-parseLeftBracket <?> "["
    v<-parseIf <?> "if"
    w<-expr
    x<-expr
    y<-expr
    z<-parseRightBracket <?> "]"
    return $ Node ("ifstmt", [u,v,w,x,y,z]))
    <|>
    do
    v<-parseLeftBracket <?> "["
    w<-parseIf <?> "if"
    x<-expr
    y<-expr
    z<-parseRightBracket <?> "]"
    return $ Node ("ifstmt", [v,w,x,y,z])
-- whilestmts -> [while expr exprlist]
whilestmts = do
    v<-parseLeftBracket <?> "["
    w<-parseWhile <?> "while"
    x<-expr
    y<-exprlist
    z<-parseRightBracket <?> "]"
    return $ Node ("whilestmt", [v,w,x,y,z])
-- exprlist -> expr | expr exprlist
exprlist = try (do
    x<-expr
    return $ Node ("exprlist", [x]))
    <|>
    do
    x<-expr
    y<-exprlist
    return $ Node ("exprlist", [x,y])
-- letstmts = [let [varlist]]
letstmts = do
    u<-parseLeftBracket <?> "["
    v<-parseLet <?> "let"
    w<-parseLeftBracket <?> "["
    x<-varlist
    y<-parseRightBracket <?> "]"
    z<-parseRightBracket <?> "]"
    return $ Node ("letstmt", [u,v,w,x,y,z])
-- varlist -> [name type] | [name type] varlist
varlist = try (do
    w<-parseLeftBracket <?> "["
    x<-name
    y<-types
    z<-parseRightBracket <?> "]"
    return $ Node ("varlist", [w,x,y,z]))
    <|>
    do
    v<-parseLeftBracket <?> "["
    w<-name
    x<-types
    y<-parseRightBracket <?> "]"
    z<-varlist
    return $ Node ("varlist", [v,w,x,y,z])
-- types -> bool | int | float | string
types = try (do
    x<-parseBool <?> "bool"
    return $ Node ("bool", [x]))
    <|>
    try (do
    x<-parseInt <?> "int"
    return $ Node ("int", [x]))
    <|>
    try (do
    x<-parseFloat <?> "float"
    return $ Node ("float", [x]))
    <|>
    do
    x<-parseString <?> "string"
    return $ Node ("string", [x])

{- Parsers -}
parseLeftBracket = do
    i <- getState
    mytoken (\t -> case t of T_LeftBracket -> Just $ Node ("[", [])
                             other         -> Nothing)
parseRightBracket = do
    i <- getState
    mytoken (\t -> case t of T_RightBracket -> Just $ Node ("]", [])
                             other          -> Nothing)
parseAssignment = do
    i <- getState
    mytoken (\t -> case t of T_Assignment   -> Just $ Node (":=", [])
                             other          -> Nothing)
parseBinOp = do
    i <- getState
    mytoken (\t -> case t of T_Add          -> Just $ Node ("+", [])
                             T_Minus        -> Just $ Node ("-", [])
                             T_Multiply     -> Just $ Node ("*", [])
                             T_Divide       -> Just $ Node ("/", [])
                             T_Mod          -> Just $ Node ("%", [])
                             T_Exponentiate -> Just $ Node ("^", [])
                             T_Equal        -> Just $ Node ("=", [])
                             T_Greater      -> Just $ Node (">", [])
                             T_GreaterEq    -> Just $ Node (">=", [])
                             T_Less         -> Just $ Node ("<", [])
                             T_LessEq       -> Just $ Node ("<=", [])
                             T_NotEq        -> Just $ Node ("!=", [])
                             T_Or           -> Just $ Node ("or", [])
                             T_And          -> Just $ Node ("and", [])
                             other          -> Nothing)
parseUnOp = do
    i <- getState
    mytoken (\t -> case t of T_Minus        -> Just $ Node ("-", [])
                             T_Not          -> Just $ Node ("not", [])
                             T_Sin          -> Just $ Node ("sin", [])
                             T_Cos          -> Just $ Node ("cos", [])
                             T_Tan          -> Just $ Node ("tan", [])
                             other          -> Nothing)
parseStringValue = do
    i <- getState
    mytoken (\t -> case t of (T_StringVal s)-> Just $ Node (s, [])
                             other          -> Nothing)
parseBoolValue = do
    i <- getState
    mytoken (\t -> case t of T_True         -> Just $ Node ("true", [])
                             T_False        -> Just $ Node ("false", [])
                             other          -> Nothing)
parseNameValue = do
    i <- getState
    mytoken (\t -> case t of (T_NameVal s)  -> Just $ Node (s, [])
                             other          -> Nothing)
parseIntValue = do
    i <- getState
    mytoken (\t -> case t of (T_IntVal s)   -> Just $ Node (s, [])
                             other          -> Nothing)
parseFloatValue = do
    i <- getState
    mytoken (\t -> case t of (T_FloatVal s) -> Just $ Node (s, [])
                             other          -> Nothing)
parseStdout = do
    i <- getState
    mytoken (\t -> case t of T_Stdout       -> Just $ Node ("stdout", [])
                             other          -> Nothing)
parseIf = do
    i <- getState
    mytoken (\t -> case t of T_If           -> Just $ Node ("if", [])
                             other          -> Nothing)
parseWhile = do
    i <- getState
    mytoken (\t -> case t of T_While        -> Just $ Node ("while", [])
                             other          -> Nothing)
parseLet = do
    i <- getState
    mytoken (\t -> case t of T_Let          -> Just $ Node ("let", [])
                             other          -> Nothing)
parseBool = do
    i <- getState
    mytoken (\t -> case t of T_Bool         -> Just $ Node ("bool", [])
                             other          -> Nothing)
parseInt = do
    i <- getState
    mytoken (\t -> case t of T_Int          -> Just $ Node ("int", [])
                             other          -> Nothing)
parseFloat = do
    i <- getState
    mytoken (\t -> case t of T_Float        -> Just $ Node ("float", [])
                             other          -> Nothing)
parseString = do
    i <- getState
    mytoken (\t -> case t of T_String       -> Just $ Node ("string", [])
                             other          -> Nothing)
{- Helpers -}
mytoken test = tokenPrim show update_pos test

update_pos pos _ _ = newPos "" 0 0

spaces' = repeat ' '
moreLimbs = not . (all (==' '))

indent n = take n spaces'

printTree EmptyTree      = repeat ""
printTree (Node (s, [])) = (" "++s++" ") : repeat (take (length s + 2) spaces')
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
