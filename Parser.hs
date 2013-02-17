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

parens :: OurParser ()
parens = do{ parseLeftParen
           ; parens
           ; parseRightParen
           ; parens
           }
        <|> return ()
