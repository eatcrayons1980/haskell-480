module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Data.Tree

help = "Usage:\n         Parse [option] [files]\n\n"++
            "-h, --help  -> This usage document.\n"++
            "-s          -> Display scanner output only.\n"++
            "Default     -> Full run on files."

{- Grammar -}
-- F -> TF | <EOF>
f = do{ f_inh <- t;
        f_node <- f;
        return (f_inh {subForest = subForest(f_inh)++[f_node]}) }
    <|>
    do{ parseEOF <?> "end of file" }

-- T -> (S)
t = do{ parseLeftParen <?> "(";
        t_node <- s;
        parseRightParen <?> ")";
        return t_node }

-- S -> (A | atomB
s = do{ parseLeftParen <?> "(";
        s_node <- a;
        return s_node }
    <|>
    do{ b_inh <- parseAtom <?> "atom";
        b_node <- b;
        return (b_inh {subForest = subForest(b_inh)++[b_node]}) }

-- A -> )B | S)B
a = do{ parseRightParen <?> ")";
        x<-b;
        return x }
    <|>
    do{ b_inh <- s;
        parseRightParen <?> ")";
        b_node <- b;
        return (b_inh {subForest = subForest(b_inh)++[b_node]}) }

-- B -> S | Empty
b = do{ x<-s; return x }
    <|> return (Node "e" [])

{- Parsers -}
parseLeftParen = do
    i <- getState
    mytoken (\t -> case t of LeftParen  -> Just(Node "(" [] )
                             other      -> Nothing)
parseRightParen = do
    i <- getState
    mytoken (\t -> case t of RightParen -> Just(Node ")" [] )
                             other      -> Nothing)
parseAtom = do
    i <- getState
    mytoken (\t -> case t of EOF        -> Nothing
                             LeftParen  -> Nothing
                             RightParen -> Nothing
                             other      -> Just(Node (show t) []))
parseEOF = do
    mytoken (\t -> case t of EOF        -> Just(Node "<EOF>" [])
                             other      -> Nothing)
{- Helpers -}
mytoken test = tokenPrim show update_pos test

update_pos pos _ _ = newPos "" 0 0

indent n = take (4*n) (repeat ' ')

{- main -}
main = do
    args <- getArgs
    case args of
        [] -> putStrLn help
        "-h":_ -> putStrLn help
        "--help":_ -> putStrLn help
        "-s":files -> flip mapM_ files $ \file -> do
                putStrLn ("\n\n"++file)
                contents <- readFile file
                mapM_ putStrLn (map show $ lexer contents)
        _ ->    flip mapM_ args $ \file -> do
                    putStrLn ("\n\n"++file)
                    contents <- readFile file
                    case (runParser f "$" file $ lexer contents) of
                        Left err -> print err
                        Right xs -> putStr (drawTree xs)
