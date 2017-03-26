{- A basic Streaming NESL Parser 
 
+ reused code from unesl interpreter
+ no type checking
+ doesn't support user-defined functions
+ doesn't support pattern maching
+ support only one variable binding in sequence comprehension
-}

module SneslParser where 

import SneslSyntax
import Text.ParserCombinators.Parsec


parseString s = parse parseExp "" s                

parseFile file = do input <- readFile file 
                    return (parseString input)                                     


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

parseVar :: Parser Id 
parseVar = do whitespace
              v <- letter <|> char '_'
              vs <- many (letter <|> digit <|> char '_')
              whitespace
              return (v:vs)


parseValue :: Parser Exp 
parseValue = do s <- many1 digit           
                whitespace
                return $ Lit $ IVal (read s)
             <|>  
             do string "True" 
                whitespace
                return $ Lit $ BVal True
             <|> 
             do string "False"
                whitespace
                return $ Lit $ BVal False           
             <|> 
             do e <- parseVar
                ((do 
                   symbol "("
                   ps <- parseExp `sepBy` (symbol ",")
                   symbol ")"
                   return $ Call e ps)
                  <|>     
                 (return $ Var e))
             <|> -- tuple or parenthesis
             do symbol "("
                e1 <- parseExp
                ((do symbol ","
                     e2 <- parseExp
                     symbol ")" 
                     return $ Tup e1 e2)
                  <|>
                 (return e1))
             <|>  -- comprehensions 
             do symbol "{"
                (do e <- parseExp
                    ((do symbol ":"  -- general ones
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
                     (do symbol "|" -- restricted ones
                         e2 <- parseExp
                         symbol "}"
                         return $ RComp e e2)))
                         --return $ GComp e [(Var "", Call "index" [b2i e2])] )))  -- desugared


--b2i :: Exp -> Exp 
--b2i (Lit (BVal b)) = Lit $ IVal (if b then 1 else 0)


parseQual = do p <- parseVar
               (symbol "<-" <|> symbol "in")
               e <- parseExp
               return (Var p, e)


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
parseSub = do e <- parseValue <|> (do e' <- parseVar; return $ Var e')
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
                           return (Var p,e1)) 
              symbol "in"
              e2 <- parseExp
              return $ Let (fst binds) (snd binds) e2
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






