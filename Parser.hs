module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Pos
--import Control.Monad.IO.Class

type OurParser a b = GenParser Token a b
--type OurParser a = ParsecT [Token] () IO a

mytoken :: (Token -> Maybe a) -> OurParser n a
mytoken test = tokenPrim show update_pos test

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = noPos
update_pos pos _ [] = noPos

noPos :: SourcePos
noPos = newPos "" 0 0

parseLeftParen :: OurParser Int String
parseLeftParen 
  = do{ i <- getState
      ; mytoken (\tok -> case tok of 
                       LeftParen  -> Just ((indent i) ++ "(\n")
                       other      -> Nothing) }

parseRightParen :: OurParser Int String
parseRightParen 
  = do{ i <- getState
      ; mytoken (\tok -> case tok of 
                       RightParen -> Just ((indent i) ++ ")\n")
                       other      -> Nothing) }

parseEOF :: OurParser Int String
parseEOF 
  = mytoken (\tok -> case tok of 
                       EOF        -> Just "<EOF>\n"
                       other      -> Nothing)

parseAtom :: OurParser Int String
parseAtom 
  = do{ i <- getState
      ; mytoken (\tok -> case tok of 
                       EOF        -> Nothing
                       LeftParen  -> Nothing
                       RightParen -> Nothing
                       VarId x    -> Just ((indent i) ++ (show x) ++ "\n")
                       IntTok x   -> Just ((indent i) ++ (show x) ++ "\n")
                       FloatTok x -> Just ((indent i) ++ (show x) ++ "\n")
                       StringTok x-> Just ((indent i) ++ (show x) ++ "\n")
                       BoolTok x  -> Just ((indent i) ++ (show x) ++ "\n")
                       Scanner.Error x -> Just ((indent i) ++ (show x) ++ "\n")
                       other      -> Just ((indent i) ++ (show tok) ++ "\n")) }

{- OUR GRAMMAR 

    F->TF|<EOF>
    T->(S)
    S->(A|atomB
    A->)B|S)B
    B->S|Empty  
-}
f :: OurParser Int String
f = do{ x <- t
      ; y <- f
      ; return (x ++ y) }
    <|>
    do{ x <- parseEOF <?> "end of file"
      ; return x }

t :: OurParser Int String
t = do{ x <- parseLeftParen <?> "("
      ; updateState (4+)
      ; y <- s
      ; updateState (subtract 4)
      ; z <- parseRightParen <?> ")"
      ; return (x ++ y ++ z) }

s :: OurParser Int String
s = do{ x <- parseLeftParen <?> "("
      ; updateState (4+)
      ; y <- a
      ; return (x ++ y) }
    <|>
    do{ x <- parseAtom <?> "atom"
      ; y <- b
      ; return (x ++ y) }

a :: OurParser Int String
a = do{ updateState (subtract 4)
      ; x <- parseRightParen <?> ")"
      ; y <- b
      ; return (x ++ y) }
    <|>
    do{ x <- s
      ; updateState (subtract 4)
      ; y <- parseRightParen <?> ")"
      ; z <- b
      ; return (x ++ y ++ z) }

b :: OurParser Int String
b = do{ x <- s
      ; return x }
    <|> return ""

indent :: Int -> String
indent n = take n (repeat ' ')

main = do
    (fileName1:_) <- getArgs
    contents <- readFile fileName1
    case (runParser f 0 "" $ lexer contents) of
        Left err  -> print err
        Right xs  -> putStr xs
