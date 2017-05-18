{- A minimum Streaming NESL interpreter -}

module SneslInterp where

import SneslSyntax
import SneslParser
import Data.Char (chr, ord)
import Data.List (transpose)


runSneslInterpEnv :: [Def] -> Either String (Env, Int,Int)
runSneslInterpEnv defs = 
    let id0 = fst $ unzip r0
    in case rSnesl (sneslInterp defs r0) of 
          Right (r, nw, ns) -> Right (takeWhile (\(x,_) -> not $ x `elem` id0) r, nw, ns)
          Left s -> Left ("SNESL interpretating error: " ++ s)


-- interpreter a list of Defs 
sneslInterp :: [Def] -> Env -> Snesl Env
sneslInterp [] r = return r
sneslInterp (d:defs) r = 
  do r' <- sneslDefInterp d r 
     sneslInterp defs r'


-- interpreter one Def independently
sneslDefInterp :: Def -> Env -> Snesl Env

--sneslDefInterp (EDef i e) r = 
--  case rSnesl (eval e r) of
--    Right (v, nw, ns) ->  return ((i,v):r)
--    Left s -> fail $ "Snesl runtime error: "++s

sneslDefInterp (FDef fname args _ e) r = 
  do let ids = fst $ unzip args 
         f vs = eval e (zip ids vs ++ r1)
         r1 = (fname, FVal f) : r
     return r1 



runSneslExp :: Exp -> Env -> Either String (Val,Int,Int)
runSneslExp e env = 
  case rSnesl (eval e (env++r0)) of
    Right (v, nw, ns) ->  Right (v,nw,ns) 
    Left s -> Left $ "Snesl runtime error: "++s



type Env = [(Id, Val)]

eval :: Exp -> Env -> Snesl Val

eval (Var s) r = 
  case lookup s r of
    Just a -> return a
    Nothing -> error ("bad variable: " ++ s)

eval (Lit l) r = 
  returnc (1,1) (AVal l)

eval (Tup e1 e2) r = 
  do v1 <- eval e1 r
     v2 <- eval e2 r 
     return $ TVal v1 v2

eval (SeqNil tp) r =
     returnc (1,1) $ SVal []

eval (Seq es) r = 
  do vs <- mapM (\e -> eval e r) es  
     returnc (wrapWork(length vs),1) $ SVal vs

eval (Let p e1 e2) r =
  do v1 <- eval e1 r
     eval e2 (bind p v1 ++ r)

eval (Call i es) r =
  do vs <- mapM (\e -> eval e r) es
     case lookup i r of
       Just (FVal f) -> f vs
       Nothing -> error ("bad function: " ++ i)

-- general comprehension
eval (GComp e0 ps) r =
  do vs <- mapM (\(_,e) -> eval e r) ps 
     let vs'' = [v | SVal v <- vs] 
         vs' = transpose vs''
         v0l = length $ head vs''  
     if all (\v -> length v == v0l) vs''
     then (do 
             let binds = zipWith (\pl vl -> 
                                 concat $ zipWith (\(p,_) v -> bind p v) pl vl)
                               (replicate (length vs') ps) vs' 
             vss <- par $ zipWith (\e b -> eval e (b++r)) 
                                  (replicate (length vs') e0) binds
             returnc (1,1) $ SVal vss)
     else fail "Length mismatch in comprehension"
     

--restricted comprehension
eval (RComp e0 e1) r = 
  do (AVal (BVal b)) <- eval e1 r 
     case b of 
        True -> (do v <- eval e0 r; returnc (1,1) (SVal [v]))
        _ -> returnc (1,1) $ SVal []



bind :: Pat -> Val -> Env
bind (PVar x) v = [(x,v)]
bind PWild v = []
bind (PTup p1 p2) (TVal v1 v2) = (bind p1 v1) ++ (bind p2 v2)


par :: [Snesl a] -> Snesl [a]
par [] = return []
par (t : ts) = 
  Snesl (case rSnesl t of
           Right (a, w1, s1) -> 
             case rSnesl (par ts) of
               Right (as, w2, s2) -> Right (a:as, w1+w2, s1 `max` s2)
               Left e -> Left e
           Left e -> Left e)



returnc :: (Int, Int) -> a -> Snesl a
returnc (w,s) a = Snesl $ Right (a, w, s)


-- For the operation that can consume(and produce) empty sequences
wrapWork :: Int -> Int 
wrapWork i = i + 1 


primop :: ([AVal] -> AVal) -> Val
primop f = FVal (\as -> returnc (1,1) $ AVal (f [v | AVal v <- as]))
                           
cplus [IVal n1, IVal n2] = IVal (n1 + n2)

cminus [IVal n1, IVal n2] = IVal (n1 - n2)

cuminus [IVal n] = IVal (- n)

ctimes [IVal n1, IVal n2] = IVal (n1 * n2)

cdiv [IVal n1, IVal n2] = IVal (n1 `div` n2)

cleq [IVal n1, IVal n2] = BVal (n1 <= n2)



r0 :: Env
r0 = [("_plus", primop cplus),
      ("_minus", primop cminus),
      ("_uminus", primop cuminus),
      ("_times", primop ctimes),
      ("_div", primop cdiv),
      ("_eq", primop (\ [v1, v2] -> BVal (v1 == v2))),
      ("_leq", primop cleq),
      ("not", primop (\ [BVal b] -> BVal (not b))),

      -- iota for sequence
      ("index", FVal (\ [AVal (IVal n)] ->
                          returnc (wrapWork n, 1) $ SVal [AVal (IVal i) | i <- [0..n-1]])),

      -- sequence append
      ("_append", FVal (\ [SVal v1, SVal v2] -> 
                         let v = v1 ++ v2 
                         in returnc (wrapWork $ length v, 1) (SVal v))),
      -- sequence concat
      ("concat", FVal (\ [SVal vs] -> 
                           let v = concat [v | SVal v <- vs]
                           in returnc (wrapWork $ length v,1) (SVal v))),

      ---- sequence empty check, zero work 
      --("empty", FVal(\[SVal vs] -> returnc (0,1) $ AVal (BVal (null vs)))),

      ---- singleton seq
      --("the", FVal (\[SVal x ] -> 
      --    if (length x == 1) 
      --    then returnc (0,1) $ head x
      --    else fail "the: length mismatch")),

      -- -- zip for two seqs, zero work
      --("zip", FVal (\[SVal v1, SVal v2] -> 
      --           if (length v1) == (length v2)
      --           then returnc (0,1) $ SVal (map (\(x,y) -> TVal x y) (zip v1 v2) )
      --           else fail "zip: lengths mismatch")),
      
      ---- seq partition with flags     
      --("part", FVal (\ [SVal vs, SVal flags] -> 
      --                      let bs = [b | AVal (BVal b) <- flags]
      --                          l = length vs
      --                      in if sum [1| b <- bs, not b] == l then
      --                           returnc (l,1) $ SVal [SVal v | v <- seglist (flags2len bs) vs]
      --                         else fail "part: flags mismatch")),


      --("scanIncPlus", FVal (\ [SVal vs] -> 
      --     let is = [i | AVal (IVal i) <- vs]
      --         l = length is 
      --         rs = tail $ scanl (+) 0 is  
      --      in returnc (l, ceiling (log $ fromIntegral l)) 
      --                $ SVal [AVal (IVal i) | i <-rs])),

      ("scanExPlus", FVal (\ [SVal vs] -> 
           let is = [i | AVal (IVal i) <- vs]
               l = length is 
               rs = init $ scanl (+) 0 is  
            in returnc (wrapWork l, 1) 
                      $ SVal [AVal (IVal i) | i <-rs])), 

      ("reducePlus", FVal (\ [SVal vs] -> 
           let is = [i | AVal (IVal i) <- vs]
               l = length is 
            in returnc (wrapWork l, 1) $ AVal $ IVal (sum is) ))]
   


-- [f,f,t,t,f,t] -> [2,0,1]
flags2len :: [Bool] -> [Int]
flags2len fs = zipWith (\x y -> x-y-1) tidx (-1:(init tidx)) 
               where tidx = [t| (t,True) <- (zip [0..(length fs)-1] fs)] 
                     len = length fs
                    
-- [3,2] -> [FFT FT] => [[FFT],[FT]] -- i.e. partition
seglist :: [Int] -> [a] -> [[a]]
seglist [] [] = []
seglist (n:ns) l = take n l : seglist ns (drop n l)




--doDef :: FilePath -> IO ()
--doDef file = 
--   do input <- readFile file 
--      case parseString input of
--          Right e ->
--              case rSnesl (sneslInterp e r0) of
--                 Right (v, nw, ns) ->  
--                    do putStrLn (show v)             
--                       putStrLn ("[Work: " ++ show nw ++ ", step: " ++ show ns ++ "]")
--                 Left s -> putStrLn ("Runtime error: " ++ s)
--          Left err -> putStrLn ("Parsing error")


