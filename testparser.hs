import Text.Parsec
import Control.Monad.IO.Class
import System.Environment
import Scanner
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

type OurParser a = ParsecT [Token] () IO a
 
play :: [Token] -> IO (Either ParseError Token)
play s = runParserT f () "parameter" s

{-pmain :: ParsecT [Token] () IO Token
pmain = do
  x <- pnum `chainl1` pplus
  eof
  return x

pnum = do
  x <- read `fmap` many1 digit
  liftIO (putStrLn "bling!")
  return x

pplus = char '+' >> return (+)-}

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


f = do{ t ; f }
    <|> do{ parseEOF <?> "end of file" }-- >>= (\x -> printMsg (x "Hot dawg")) }

t = do{ parseLeftParen <?> "("; s ; parseRightParen <?> ")"}

s = do{ parseLeftParen <?> "("; a }
    <|> do{ parseAtom <?> "atom"; b }

a = do{ parseRightParen <?> ")"; b }
    <|> do{ s ; parseRightParen <?> ")"; b }

b = s <|> return Epsilon