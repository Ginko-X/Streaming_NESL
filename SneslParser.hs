{- A basic Streaming NESL Parser   -}

module SneslParser where 

import SneslSyntax
import Text.ParserCombinators.Parsec

parseString :: String -> Either String Exp
parseString s = 
  case parse (do e <- parseExp; eof; return e) "" s of 
    Right e -> Right e 
    Left _ -> Left "Parsing error"


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

parsePat :: Parser Pat
parsePat = do x <- parseVar
              return $ PVar x
           <|>
           do symbol "_"
              return PWild
           <|>
           do symbol "("
              p1 <- parsePat 
              symbol ","
              p2 <- parsePat
              symbol ")"
              return $ PTup p1 p2


parseValue :: Parser Exp 
parseValue = do whitespace
                s <- many1 digit           
                whitespace
                return $ Lit $ IVal (read s)
             <|>  
             do symbol "T" 
                whitespace
                return $ Lit $ BVal True
             <|> 
             do symbol "F"
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
                 (do symbol ")" 
                     return e1))
             <|>  -- comprehensions and sequence
             do symbol "{"
                ((do e <- parseExp
                     ((do symbol ","
                          es <- parseExp `sepBy1` (symbol ",")
                          symbol "}"  
                          return $ Seq (e:es))                     
                       <|>
                      (do symbol "}" 
                          return $ Seq [e])
                       <|>
                      (do symbol ":"  -- general ones
                          qs <- parseQual `sepBy1` (symbol ";" <|> symbol ",")                        
                          symbol "}"
                          return $ GComp e qs)                        
                       <|> 
                      (do symbol "|" -- restricted ones
                          e2 <- parseExp
                          symbol "}"
                          return $ RComp e e2)))
                 <|>
                 (do symbol "}" 
                     tp <- parseType 
                     return $ SeqNil tp))


parseType :: Parser Type 
parseType = do symbol "int" 
               return TInt 
            <|>
            do symbol "bool"
               return TBool
            <|>
            do symbol "{"
               t <- parseType
               symbol "}"
               return $ TSeq t
            <|>
            do symbol "("
               t1 <- parseType
               symbol ","
               t2 <- parseType
               symbol ")"
               return $ TTup t1 t2 



parseQual :: Parser (Pat, Exp)
parseQual = do p <- parsePat
               (symbol "<-" <|> symbol "in")
               e <- parseExp
               return (p, e)


binop :: String -> Exp -> Exp -> Exp
binop s x y = Call s [x,y]


parseTerm :: Parser Exp
parseTerm = parsePrefix `chainl1` (do o <- mulop
                                      return $ \e1 e2 -> o e1 e2)
              where mulop = do {symbol "*"; return $ binop "_times"} <|>
                            do {symbol "/"; return $ binop "_div"}

parseSum :: Parser Exp
parseSum = parseTerm `chainl1` (do o <- addop
                                   return $ \e1 e2 -> o e1 e2)
              where addop = do {try (symbol "++"); return $ binop "_append"} <|>
                            do {symbol "+"; return $ binop "_plus"} <|>
                            do {symbol "-"; return $ binop "_minus"}

parsePrefix :: Parser Exp
parsePrefix =
  do symbol "#"
     e <- parsePrefix
     return (Call "_length" [e])
  <|>
  do symbol "&"
     e <- parsePrefix
     return (Call "index" [e])
  <|>
  do symbol "-"
     e <- parsePrefix
     return (Call "_uminus" [e])
  <|>
  parseSub


parseSub :: Parser Exp
parseSub = do e <- parseValue <|> (do e' <- parseVar; return $ Var e')
              ss <- many sub
              return $ foldl (\e1 e2 -> (Call "_sub" [e1,e2])) e ss
            where sub = do symbol "["
                           e1 <- parseExp
                           symbol "]"
                           return e1


parseExp :: Parser Exp
parseExp = do whitespace
              symbol "let"
              whitespace
              binds <- (do p <- parsePat
                           symbol "="
                           e1 <- parseExp
                           return (p,e1))
                        `sepBy1` (symbol ";")
              symbol "in"
              e2 <- parseExp
              return $ foldr (\(p,e1) e2 -> Let p e1 e2) e2 binds
           <|>
           parseComp

                   

parseComp = do e <- parseSum
               option e (do o <- compop
                            e' <- parseSum
                            return $ (o e e'))
             where compop = do {symbol "=="; return $ binop "_eq"} <|>
                            do {symbol "<="; return $ binop "_leq"} <|>
                            do {symbol ">="; return $ \x y -> binop "_leq" y x} <|>
                            do {symbol "<"; return $ \x y -> Call "not" [Call "_leq" [y,x]]} <|>
                            do {symbol ">"; return $ \x y -> Call "not" [Call "_leq" [x,y]]}






