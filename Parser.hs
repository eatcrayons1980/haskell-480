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

type OurParser a b = GenParser Token a b

{- Grammar -}
-- F -> TF | <EOF>
f = do{ x<-t;
        y<-f;
        return (Node "$" [x,y]) }
    <|> do{ x<-parseEOF <?> "end of file"; return x }
-- T -> (S)
t = do{ x<-parseLeftParen <?> "("; updateState(1+); y<-s; updateState(subtract 1);
        z<-parseRightParen <?> ")"; return y }
-- S -> (A | atomB
s = do{ x<-parseLeftParen <?> "("; updateState(1+); y<-a; return y }
    <|> do{ x<-parseAtom <?> "atom";
            y<-b;
            return $ x {subForest = subForest(x)++[y]} }
-- A -> )B | S)B
a = do{ updateState(subtract 1); x<-parseRightParen <?> ")"; y<-b; return y }
    <|> do{ x<-s; updateState(subtract 1); y<-parseRightParen <?> ")"; z<-b; return (Node "A" [x,z])  }
-- B -> S | Empty
b = do{x<-s; return x }
    <|> return (Node "e" [])   -- epsilon

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
                    case (runParser f 0 file $ lexer contents) of
                        Left err -> print err
                        Right xs -> putStr (drawTree xs)
