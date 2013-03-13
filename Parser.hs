module Main where

import System.Environment
import Scanner
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String()
import Data.Tree()
import Data.List()

help :: String
help = "Usage:\n         Parse [option] [files]\n\n"++
            "-h, --help  -> This usage document.\n"++
            "-s          -> Display scanner output only.\n"++
            "Default     -> Full run on files."

{----------------------------------------------------
    Grammar
-----------------------------------------------------}
-- F -> TF | <EOF>
f = do{ t_node <- t;
        f_node <- f;
        case t_node of (FloatTok _)  -> return $ show t_node++" f.\n"++f_node
                       (StringTok _) -> return $ show t_node++" type\n"++f_node
                       _             -> return $ show t_node++" .\n"++f_node }
    <|>
    do{ parseEOF <?> "end of file";
        return "" }

-- T -> (S)
t = do{ parseLeftParen <?> "(";
        s_node <- s;
        parseRightParen <?> ")";
        return $ typeOp s_node }

-- S -> (A | atomB
s = do{ parseLeftParen <?> "(";
        a_node <- a;
        return a_node }
    <|>
    do{ atom <- parseAtom <?> "atom";
        b_node <- b;
        return (atom++b_node) }

-- A -> )B | S)B
a = do{ parseRightParen <?> ")";
        b_node <- b;
        return b_node }
    <|>
    do{ s_node <- s;
        parseRightParen <?> ")";
        b_node <- b;
        return $ typeOp s_node:b_node }

-- B -> S | Empty
b = do{ s_node <- s;
        return s_node }
    <|> return []

{- Parsers -}
parseLeftParen  = mytoken (\v -> case v of LeftParen  -> Just []
                                           _          -> Nothing)
parseRightParen = mytoken (\v -> case v of RightParen -> Just []
                                           _          -> Nothing)
parseAtom       = mytoken (\v -> case v of EOF        -> Nothing
                                           LeftParen  -> Nothing
                                           RightParen -> Nothing
                                           _          -> Just [v])
parseEOF        = mytoken (\v -> case v of EOF        -> Just []
                                           _          -> Nothing)

{----------------------------------------------------
    gforth translation rules
-----------------------------------------------------}
typeOp :: [Token] -> Token
-- Boolean Operators
typeOp (l@KW_And    :BoolTok  x:BoolTok  y:[]) = BoolTok $ x++" "    ++y++" "     ++show l
typeOp (l@KW_Or     :BoolTok  x:BoolTok  y:[]) = BoolTok $ x++" "    ++y++" "     ++show l
typeOp (  KW_Not    :BoolTok  x:[])            = BoolTok $ x++" invert"
typeOp (  KW_Iff    :BoolTok  x:BoolTok  y:[]) = BoolTok $ x++" "    ++y++" xor invert"
typeOp (l@Equal     :FloatTok x:FloatTok y:[]) = BoolTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Equal     :FloatTok x:IntTok   y:[]) = BoolTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Equal     :IntTok   x:FloatTok y:[]) = BoolTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Equal     :IntTok   x:IntTok   y:[]) = BoolTok $ x++" "    ++y++" "     ++show l
typeOp (l@Less      :FloatTok x:FloatTok y:[]) = BoolTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Less      :FloatTok x:IntTok   y:[]) = BoolTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Less      :IntTok   x:FloatTok y:[]) = BoolTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Less      :IntTok   x:IntTok   y:[]) = BoolTok $ x++" "    ++y++" "     ++show l
-- Integer Operators
typeOp (l@Plus      :IntTok x:IntTok y:[]) = IntTok $ x++" "++y++" "++show l
typeOp (l@Minus     :IntTok x:IntTok y:[]) = IntTok $ x++" "++y++" "++show l
typeOp (  Minus     :IntTok x:[])          = IntTok $ x++" negate"
typeOp (l@Mult      :IntTok x:IntTok y:[]) = IntTok $ x++" "++y++" "++show l
typeOp (l@Div       :IntTok x:IntTok y:[]) = IntTok $ x++" "++y++" "++show l
typeOp (l@Mod       :IntTok x:IntTok y:[]) = IntTok $ x++" "++y++" "++show l
-- Float Operators
typeOp (l@Plus      :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Plus      :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Plus      :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Minus     :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Minus     :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Minus     :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (  Minus     :FloatTok x:[])            = FloatTok $ x++" fnegate"
typeOp (l@Mult      :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Mult      :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Mult      :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Div       :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Div       :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Div       :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Carrot    :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Carrot    :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Carrot    :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@Carrot    :IntTok   x:IntTok   y:[]) = FloatTok $ x++" s>f "++y++" s>f f"++show l
typeOp (l@Mod       :FloatTok x:FloatTok y:[]) = FloatTok $ x++" "    ++y++" f"    ++show l
typeOp (l@Mod       :FloatTok x:IntTok   y:[]) = FloatTok $ x++" "    ++y++" s>f f"++show l
typeOp (l@Mod       :IntTok   x:FloatTok y:[]) = FloatTok $ x++" s>f "++y++" f"    ++show l
typeOp (l@KW_Exp    :FloatTok x:[])            = FloatTok $ x++" f"                ++show l
typeOp (l@KW_Exp    :IntTok   x:[])            = FloatTok $ x++" s>f f"            ++show l
typeOp (l@KW_Sin    :FloatTok x:[])            = FloatTok $ x++" f"                ++show l
typeOp (l@KW_Sin    :IntTok   x:[])            = FloatTok $ x++" s>f f"            ++show l
typeOp (l@KW_Cos    :FloatTok x:[])            = FloatTok $ x++" f"                ++show l
typeOp (l@KW_Cos    :IntTok   x:[])            = FloatTok $ x++" s>f f"            ++show l
typeOp (l@KW_Tan    :FloatTok x:[])            = FloatTok $ x++" f"                ++show l
typeOp (l@KW_Tan    :IntTok   x:[])            = FloatTok $ x++" s>f f"            ++show l
typeOp (l@KW_Assign :FloatTok x:[])            = FloatTok $ x++" "                 ++show l
typeOp (l@KW_While  :FloatTok x:[])            = FloatTok $ x++" "                 ++show l
-- String Operators
typeOp (Plus        :StringTok x:StringTok y:[]) = StringTok $ "s\" "++x++"\" s\" "++y++"\" append"
-- Other Operators
typeOp (x:[])     = x
typeOp []         = Scanner.Error "<Empty set>"
typeOp (_:_:[])   = Scanner.Error "<Invalid operator or argument>"
typeOp (_:_:_:[]) = Scanner.Error "<Invalid operator or argument>"

{- Helpers -}
mytoken = tokenPrim show updatePos

updatePos pos _ _ = newPos "" 0 0

{----------------------------------------------------
    Main
-----------------------------------------------------}
main = do
    args <- getArgs
    case args of
        [] -> putStrLn help
        "-h":_ -> putStrLn help
        "--help":_ -> putStrLn help
        "-s":files -> flip mapM_ files $ \file -> do
                putStrLn file
                contents <- readFile file
                mapM_ print (lexer contents)
        _ ->    flip mapM_ args $ \file -> do
                    contents <- readFile file
                    case runParser f "$" file $ lexer contents of
                        Left err -> print err
                        Right xs -> putStrLn xs
