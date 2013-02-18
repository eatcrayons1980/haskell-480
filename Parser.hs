module Main where
import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
type OurParser a b = GenParser Token a b
{- Helpers -}
mytoken    test    = tokenPrim show update_pos test
update_pos pos _ _ = newPos "" 0 0
indent     n       = take n (repeat ' ')
{- Parsers -}
parseLeftParen = do{ i <- getState;
           mytoken (\t -> case t of LeftParen->Just(indent i++"(\n"); other->Nothing) }
parseRightParen = do{ i <- getState;
           mytoken (\t -> case t of RightParen->Just(indent i++")\n"); other->Nothing) }
parseAtom = do{ i <- getState;
           mytoken (\t -> case t of EOF->Nothing; LeftParen->Nothing; RightParen->Nothing
                                    other->Just(indent i++show t++"\n")) }
parseEOF = mytoken (\t -> case t of EOF->Just"<EOF>\n"; other->Nothing)
{- Grammar -}
f = do{x<-t; y<-f; return (x++y)}                                               -- F->TF|<EOF>
    <|> do{x<-parseEOF <?> "end of file"; return x}
t = do{x<-parseLeftParen <?> "("; updateState(4+); y<-s; updateState(subtract 4)-- T->(S)
      ; z<-parseRightParen <?> ")"; return (x++y++z)}
s = do{x<-parseLeftParen <?> "("; updateState (4+); y<-a; return (x++y)}        -- S->(A|atomB
    <|> do{x<-parseAtom <?> "atom"; y<-b; return (x++y)}
a = do{updateState(subtract 4); x<-parseRightParen <?> ")"; y<-b; return (x++y)}-- A->)B|S)B
    <|> do{x<-s; updateState(subtract 4); y<-parseRightParen <?> ")"; z<-b; return (x++y++z)}
b = do{x<-s; return x}                                                          -- B->S|Empty
    <|> return ""
{- Main -}
main = do{ (fileName1:_) <- getArgs; contents <- readFile fileName1;
    case (runParser f 0 "" $ lexer contents) of Left err->print err; Right xs->putStr xs}
