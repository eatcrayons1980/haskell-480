module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Data.Tree
import Data.List

help = "Usage:\n         Parse [option] [files]\n\n"++
            "-h, --help  -> This usage document.\n"++
            "-s          -> Display scanner output only.\n"++
            "Default     -> Full run on files."

{- Grammar -}
-- F -> TF | <EOF>
f = do{ t_node <- t;
        f_node <- f;
        return $ t_node++f_node }
    <|>
    do{ parseEOF <?> "end of file" }

-- T -> (S)
t = do{ parseLeftParen <?> "(";
        s_node <- s;
        parseRightParen <?> ")";
        return $ [type_op s_node] }

-- S -> (A | atomB
s = do{ parseLeftParen <?> "(";
        a_node <- a;
        return a_node }
    <|>
    do{ atom <- parseAtom <?> "atom";
        b_node <- b;
        return (atom++b_node) }

-- A -> )B | S)B
a = do{ parseRightParen <?> ")";
        b_node <- b;
        return b_node }
    <|>
    do{ s_node <- s;
        parseRightParen <?> ")";
        b_node <- b;
        return $ [type_op s_node]++b_node }

-- B -> S | Empty
b = do{ s_node <- s;
        return s_node }
    <|> return []

{- Parsers -}
parseLeftParen = do
    mytoken (\t -> case t of LeftParen  -> Just([])
                             other      -> Nothing)
parseRightParen = do
    mytoken (\t -> case t of RightParen -> Just([])
                             other      -> Nothing)
parseAtom = do
    mytoken (\t -> case t of EOF        -> Nothing
                             LeftParen  -> Nothing
                             RightParen -> Nothing
                             other      -> Just([t]))
parseEOF = do
    mytoken (\t -> case t of EOF        -> Just([])
                             other      -> Nothing)
{- Helpers -}
mytoken test = tokenPrim show update_pos test

update_pos pos _ _ = newPos "" 0 0

type_op :: [Token] -> Token
type_op ( op:[]) = op
-- Integer Operators
type_op ( (Plus)      : (IntTok x) :[]) = IntTok $ x++" +"
type_op ( (Minus)     : (IntTok x) :[]) = IntTok $ x++" -"
type_op ( (Mult)      : (IntTok x) :[]) = IntTok $ x++" *"
type_op ( (Div)       : (IntTok x) :[]) = IntTok $ x++" /"
type_op ( (Equal)     : (IntTok x) :[]) = IntTok $ x++" ="
-- Float Operators
type_op ( (Plus)      : (FloatTok x) :[]) = FloatTok $ x++" f+"
type_op ( (Minus)     : (FloatTok x) :[]) = FloatTok $ x++" f-"
type_op ( (Mult)      : (FloatTok x) :[]) = FloatTok $ x++" f*"
type_op ( (Div)       : (FloatTok x) :[]) = FloatTok $ x++" f/"
type_op ( (Equal)     : (FloatTok x) :[]) = FloatTok $ x++" ="
type_op ( (KW_Assign) : (FloatTok x) :[]) = FloatTok $ x++" assign"
type_op ( (KW_While)  : (FloatTok x) :[]) = FloatTok $ x++" while"
-- Other Operators
type_op ( op:rest:[]) = FloatTok $ show rest++" "++show op
type_op ( op:xs)      = type_op (op:(type_op' xs):[])

-- Token Combiner
type_op' :: [Token] -> Token
type_op' (a:[]) = a
type_op' (a:b:[]) = case a of
        (IntTok x)   -> case b of (IntTok y)   -> IntTok   $ x++" "++y
                                  (FloatTok y) -> FloatTok $ x++" s>f "++y
                                  (VarId y)    -> FloatTok $ x++" "++y
                                  other        -> FloatTok $ x++" "++show b
        (FloatTok x) -> case b of (IntTok y)   -> FloatTok $ x++" "++y++" s>f"
                                  (FloatTok y) -> FloatTok $ x++" "++y
                                  (VarId y)    -> FloatTok $ x++" "++y
                                  other        -> FloatTok $ x++" "++show b
        (VarId x)    -> case b of (IntTok y)   -> FloatTok $ x++" "++y
                                  (FloatTok y) -> FloatTok $ x++" "++y
                                  (VarId y)    -> FloatTok $ x++" "++y
                                  other        -> FloatTok $ x++" "++show b
        other -> Epsilon
type_op' (a:b:c:ds) = type_op' $ (a:[type_op' (b:c:ds)])

{- main -}
main = do
    args <- getArgs
    case args of
        [] -> putStrLn help
        "-h":_ -> putStrLn help
        "--help":_ -> putStrLn help
        "-s":files -> flip mapM_ files $ \file -> do
                putStrLn file
                contents <- readFile file
                mapM_ putStrLn (map show $ lexer contents)
        _ ->    flip mapM_ args $ \file -> do
                    contents <- readFile file
                    case (runParser f "$" file $ lexer contents) of
                        Left err -> print err
                        Right xs -> mapM_ putStrLn $ map show xs
