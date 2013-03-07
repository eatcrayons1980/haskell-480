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
f = do{ t_node <- t;
        f_node <- f;
        return $ concat [t_node,f_node] }
    <|>
    do{ parseEOF <?> "end of file" }

-- T -> (S)
t = do{ parseLeftParen <?> "(";
        s_node <- s;
        parseRightParen <?> ")";
        let x:xs = s_node
        in return $ concat [xs,[x]] }

-- S -> (A | atomB
s = do{ parseLeftParen <?> "(";
        a_node <- a;
        return a_node }
    <|>
    do{ atom <- parseAtom <?> "atom";
        b_node <- b;
        return $ concat [atom,b_node] }

-- A -> )B | S)B
a = do{ parseRightParen <?> ")";
        b_node <- b;
        return b_node }
    <|>
    do{ s_node <- s;
        parseRightParen <?> ")";
        b_node <- b;
        let x:xs = init s_node
            b_node' = [last s_node]
        in return $ concat [concat [xs++b_node'++[x]], b_node] }

-- B -> S | Empty
b = do{ s_node <- s;
        return s_node }
    <|> return []

{- Parsers -}
parseLeftParen = do
    i <- getState
    mytoken (\t -> case t of LeftParen  -> Just([])
                             other      -> Nothing)
parseRightParen = do
    i <- getState
    mytoken (\t -> case t of RightParen -> Just([])
                             other      -> Nothing)
parseAtom = do
    i <- getState
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

indent n = take (4*n) (repeat ' ')





toGforth :: Int->[Token]->String
toGforth _ [] = ""
toGforth f ((IntTok x):xs) 
                    | f == 1 = show x++" s>f "++toGforth f xs
                    | otherwise = show x++" "++toGforth f xs
toGforth f (FloatTok x:xs) = floatTokHasE(show x)++" "++toGforth f xs 
toGforth f (x:xs) 
           | (x == Mult || x == Div || x == Minus || x == Plus) && f == 1 = "f"++show(x)++" "++toGforth f xs
           | otherwise = show x++" "++toGforth f xs


floatTokHasE :: [Char]->String
floatTokHasE [] = "e"
floatTokHasE (x:xs) 
             | x == 'e' = (x:xs)
             | otherwise = x:floatTokHasE(xs)

{- return 0 doesn't contain float, 1 contains-}
hasFloatTok :: [Token]->Int
hasFloatTok [] = 0
hasFloatTok (FloatTok x:xs) = 1
hasFloatTok (x:xs) = hasFloatTok xs






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
                    putStrLn file
                    contents <- readFile file
                    case (runParser f "$" file $ lexer contents) of
                        Left err -> print err
                        Right xs -> putStr (toGforth (hasFloatTok xs) xs) 
