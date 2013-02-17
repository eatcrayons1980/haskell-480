module Parser where

import Scanner
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

type OurParser a = GenParser Token () a

mytoken :: (Token -> Maybe a) -> OurParser a
mytoken test
  = token showTok posFromTok testTok
  where
    showTok t           = show t
    posFromTok t        = noPos
    testTok t           = test t

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

{- OUR GRAMMAR -}

f :: OurParser Token
f = do{ parseEOF <?> "end of file" }
    <|> do{ t ; f }

t :: OurParser Token
t = do{ parseLeftParen <?> "("; s ; parseRightParen <?> ")"}

s :: OurParser Token
s = do{ parseLeftParen <?> "("; a }
    <|> do{ parseAtom <?> "atom"; b }

a :: OurParser Token
a = do{ parseRightParen <?> ")"; b }
    <|> do{ s ; parseRightParen <?> ")"; b }

b :: OurParser Token
b = s <|> return Epsilon
