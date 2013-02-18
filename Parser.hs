module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Control.Monad.IO.Class

--type OurParser a = GenParser Token () a
type OurParser a = ParsecT [Token] () IO a

mytoken :: (Token -> Maybe a) -> OurParser a
mytoken test = tokenPrim show update_pos test

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = noPos
update_pos pos _ [] = noPos

noPos :: SourcePos
noPos = newPos "" 0 0

parseLeftParen :: OurParser Token
parseLeftParen 
  = mytoken (\tok -> case tok of 
                       LeftParen  -> Just LeftParen
                       other      -> Nothing)

parseRightParen :: OurParser Token
parseRightParen 
  = mytoken (\tok -> case tok of 
                       RightParen -> Just RightParen
                       other      -> Nothing)

parseEOF :: OurParser Token
parseEOF 
  = mytoken (\tok -> case tok of 
                       EOF        -> Just EOF
                       other      -> Nothing)

parseAtom :: OurParser Token
parseAtom 
  = mytoken (\tok -> case tok of 
                       EOF        -> Nothing
                       LeftParen  -> Nothing
                       RightParen -> Nothing
                       other      -> Just tok)

{- OUR GRAMMAR 

    F->TF|<EOF>
    T->(S)
    S->(A|atomB
    A->)B|S)B
    B->S|Empty  
-}
f :: OurParser Token
f = do{ t ; f }
    <|> do{ x <- parseEOF <?> "end of file" ; liftIO $ putStrLn "Done" ; return x }

t :: OurParser Token
t = do{ parseLeftParen <?> "("; liftIO (putStrLn "(") ; s ; x <- parseRightParen <?> ")" ; liftIO (putStrLn ")") ; return x }

s :: OurParser Token
s = do{ parseLeftParen <?> "("; liftIO (putStrLn "(") ; a }
    <|> do{ x <- parseAtom <?> "atom"; liftIO (putStrLn $ show x) ; b }

a :: OurParser Token
a = do{ parseRightParen <?> ")" ; liftIO (putStrLn ")") ; b }
    <|> do{ s ; parseRightParen <?> ")"; liftIO (putStrLn ")") ; b }

b :: OurParser Token
b = s <|> return Epsilon


main = do
    (fileName1:_) <- getArgs
    contents <- readFile fileName1
    runPT f () fileName1 $ lexer contents -- lexer returns [Tokens]