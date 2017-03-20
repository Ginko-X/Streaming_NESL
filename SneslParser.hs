{- Streaming NESL Parser -}

module SneslParser where 

import SneslSyntax
import Text.ParserCombinators.Parsec


parseString s = parse parseExp "" s                
                                         
parseFile p = do input <- readFile p 
                 return (parseString input)

st2 = " a_var"
st1 = "\'c\'"
st3 = "\"str seq\""
st4 = "1.2"
st5 = "x+1"
st6 = "{1,2}"
st7 = "let x = 2+3*4 in x"
st8 = "#{1,2}*4-5"

testParser = map parseString [st7,st8]


-- code from unesl parser

whitespace :: Parser ()
whitespace = skipMany (do space
                          return ()
                       <|>
                       do try (string "--")
                          manyTill anyChar (try newline)
                          return ())

symbol :: String ->  Parser ()
symbol s = do try (string s)
              whitespace

parseVar :: Parser Exp
parseVar = do whitespace
              v <- letter <|> char '_'
              vs <- many (letter <|> digit <|> char '_')
              whitespace
              return $ Var (v:vs)


parseValue :: Parser Exp 
parseValue = do s <- many1 digit                
                ((do char '.'
                     s2 <- many1 digit
                     whitespace
                     return $ Lit $ RVal (read (s ++ "." ++ s2)))
                 <|> 
                 (do whitespace
                     return $ Lit $ IVal (read s)))                
             <|>  
             do string "true" 
                return $ Lit $ BVal True
             <|> 
             do string "false"
                return $ Lit $ BVal False
             <|> -- char
             do char '\''
                c <- anyChar
                char '\''
                whitespace
                return $ Lit (CVal c)           
             <|>  -- string sequence
             do char '"'
                s <- manyTill anyChar (try (char '"'))
                whitespace
                return $ Seq (map (Lit . CVal) s)
             <|> -- tuple or parenthesis
             do symbol "("
                es <- parseExp `sepBy` (symbol ",")
                symbol ")"
                case es of
                  [e] -> return e
                  _ -> return $ Tup es
             <|>  -- comprehension 
             do symbol "{"
                --(do es <- parseExp `sepBy` (symbol ",")
                --    symbol "}"
                --    return $ Seq es)
                -- <|>   
                (do e <- parseExp
                    ((do symbol ":"
                         qs <- parseQual `sepBy1` (symbol ";" <|> symbol ",")
                         me <- option Nothing
                               (do symbol "|"
                                   e <- parseExp
                                   return $ Just e)
                         symbol "}"
                         case me of
                           Nothing -> return $ GComp e qs 
                           Just ef -> return $ Call "concat" [GComp (RComp e ef) qs])
                      <|>
                     (do symbol "|"
                         e2 <- parseExp
                         symbol "}"
                         return $ RComp e e2)))
--             return GComp e1 ["", Call "iota" [b2i e2] ] ))


b2i :: Bool -> Int 
b2i b = if b then 1 else 0


parseQual = do p <- parseVar
               (symbol "<-" <|> symbol "in")
               e <- parseExp
               return (p, e)


binop :: String -> Exp -> Exp -> Exp
binop s x y = Call s [x,y]


parseTerm :: Parser Exp
parseTerm = parsePrefix `chainl1` (do sp <- getPosition
                                      o <- mulop
                                      return $ \e1 e2 -> o e1 e2)
              where mulop = do {symbol "*"; return $ binop "_times"} <|>
                            do {symbol "/"; return $ binop "_div"}

parseSum :: Parser Exp
parseSum = parseTerm `chainl1` (do sp <- getPosition
                                   o <- addop
                                   return $ \e1 e2 -> o e1 e2)
              where addop = do {try (symbol "++"); return $ binop "_append"} <|>
                            do {symbol "+"; return $ binop "_plus"} <|>
                            do {symbol "-"; return $ binop "_minus"}

parsePrefix :: Parser Exp
parsePrefix =
  do sp <- getPosition
     symbol "#"
     e <- parsePrefix
     return (Call "_length" [e])
  <|>
  do sp <- getPosition     
     symbol "&"
     e <- parsePrefix
     return (Call "index" [e])
  <|>
  do sp <- getPosition
     symbol "-"
     e <- parsePrefix
     return (Call "_uminus" [e])
  <|>
  parseSub

parseSub :: Parser Exp
parseSub = do e <- parseValue <|> parseVar
              ss <- many sub
              return $ foldl (\e1 (e2,p) -> (Call "_sub" [e1,e2])) e ss
            where sub = do sp <- getPosition 
                           symbol "["
                           e1 <- parseExp
                           symbol "]"
                           return (e1, sp)


parseExp :: Parser Exp
parseExp = do symbol "let"
              whitespace
              binds <- (do p <- parseVar 
                           symbol "="
                           e1 <- parseExp
                           return (p,e1))     --`sepBy1` (symbol ";")                  
              symbol "in"
              e2 <- parseExp
              return $ Let (fst binds) (snd binds) e2
--               return $ foldr (\(p,e1) e2 -> Let p e1 e2) e2 binds
           <|>
           parseComp

                   

parseComp = do e <- parseSum
               option e (do sp <- getPosition
                            o <- compop
                            e' <- parseSum
                            return $ (o e e'))
             where compop = do {symbol "=="; return $ binop "_eq"} <|>
                            do {symbol "<="; return $ binop "_leq"} <|>
                            do {symbol ">="; return $ \x y -> binop "_leq" y x} <|>
                            do {symbol "<"; return $ \x y -> Call "not" [Call "_leq" [y,x]]} <|>
                            do {symbol ">"; return $ \x y -> Call "not" [Call "_leq" [x,y]]}






