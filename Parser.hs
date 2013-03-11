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
        case t_node of (FloatTok _)  -> return $ show t_node++" f. "++f_node
                       (StringTok _) -> return $ show t_node++" type "++f_node
                       _             -> return $ show t_node++" . "++f_node }
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
parseLeftParen = mytoken (\v -> case v of LeftParen  -> Just []
                                          _          -> Nothing)
parseRightParen = mytoken (\v -> case v of RightParen -> Just []
                                           _          -> Nothing)
parseAtom = mytoken (\v -> case v of EOF        -> Nothing
                                     LeftParen  -> Nothing
                                     RightParen -> Nothing
                                     _          -> Just [v])
parseEOF = mytoken (\v -> case v of EOF -> Just []
                                    _   -> Nothing)

{----------------------------------------------------
    gforth translation rules
-----------------------------------------------------}
typeOp :: [Token] -> Token
typeOp (StringTok x:[]) = StringTok $ "s\" "++x++"\""
typeOp (op:[]) = op
-- Boolean Operators
typeOp (l@KW_And    :BoolTok x:[]) = BoolTok $ x++" "++show l
typeOp (l@KW_Or     :BoolTok x:[]) = BoolTok $ x++" "++show l
typeOp (l@KW_Not    :BoolTok x:[]) = BoolTok $ x++" "++show l
typeOp (l@KW_Iff    :BoolTok x:[]) = BoolTok $ x++" "++show l
-- Integer Operators
typeOp (l@Plus      :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Minus     :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Mult      :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Div       :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Mod       :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Equal     :IntTok x:[]) = IntTok $ x++" "++show l
typeOp (l@Less      :IntTok x:[]) = IntTok $ x++" "++show l
-- Float Ops applied to Ints
typeOp (l@Carrot    :IntTok x:IntTok y:[]) = FloatTok $ x++" s>f "++y++" s>f f"++show l
typeOp (l@KW_Exp    :IntTok x:[]) = FloatTok $ x++" s>f f"++show l
typeOp (l@KW_Sin    :IntTok x:[]) = FloatTok $ x++" s>f f"++show l
typeOp (l@KW_Cos    :IntTok x:[]) = FloatTok $ x++" s>f f"++show l
typeOp (l@KW_Tan    :IntTok x:[]) = FloatTok $ x++" s>f f"++show l
-- Float Operators
typeOp (l@Plus      :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@Minus     :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@Mult      :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@Div       :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@Carrot    :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@Mod       :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@KW_Exp    :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@KW_Sin    :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@KW_Cos    :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@KW_Tan    :FloatTok x:[]) = FloatTok $ x++" f"++show l
typeOp (l@KW_Assign :FloatTok x:[]) = FloatTok $ x++" "++show l
typeOp (l@KW_While  :FloatTok x:[]) = FloatTok $ x++" "++show l
typeOp (l@Equal     :FloatTok x:[]) = BoolTok $ x++" f"++show l
typeOp (l@Less      :FloatTok x:[]) = BoolTok $ x++" f"++show l
-- String Operators
typeOp (Plus        :StringTok x:[]) = StringTok $ x++" append"
-- Other Operators
typeOp (_:_:[]) = Scanner.Error "<Invalid operator or argument>"
typeOp (op:xs)      = typeOp [op, typeOp' xs]

-- Token Combiner
typeOp' :: [Token] -> Token
typeOp' (v:[]) = v
typeOp' (v:w:[]) = case v of
        (IntTok x)    -> case w of (IntTok y)    -> IntTok    $ x++" "++y
                                   (FloatTok y)  -> FloatTok  $ x++" s>f "++y
                                   _             -> Scanner.Error "<Unknown conversion to Int>"
        (FloatTok x)  -> case w of (IntTok y)    -> FloatTok  $ x++" "++y++" s>f"
                                   (FloatTok y)  -> FloatTok  $ x++" "++y
                                   _             -> Scanner.Error "<Unknown conversion to Float>"
        (BoolTok x)   -> case w of (BoolTok y)   -> BoolTok   $ x++" "++y
        (StringTok x) -> case w of (StringTok y) -> StringTok $ "s\" "++x++"\" "++" s\" "++y++"\""
        _             -> Scanner.Error "<Unknown type conversion>"
typeOp' (v:w:c:ds) = typeOp' (v:[typeOp' (w:c:ds)])

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
