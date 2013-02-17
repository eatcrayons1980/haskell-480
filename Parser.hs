import Scanner
import Text.ParserCombinators.Parsec

type ourParser a :: GenParser Token () a

mytoken :: Show t => t -> GenParser ((Int,Int),t) () t
mytoken x
  = token showTok posFromTok testTok
  where
    showTok t           = show t
    posFromTok t        = 0
    testTok t           = if (x == t) then Just t else Nothing

parseLeftParen :: ourParse String
parseLeftParen 
  = mytoken (\tok -> case tok of 
                       LeftParen  -> Just "("
                       other      -> Nothing)

parseRightParen :: ourParse String
parseRightParen 
  = mytoken (\tok -> case tok of 
                       RightParen -> Just ")"
                       other      -> Nothing)

parens  :: Parser ()
parens  = do{ parseLeftParen
            ; parens
            ; parseRightParen
            ; parens
            }
        <|> return ()

main = do putStrLn "Hello"
