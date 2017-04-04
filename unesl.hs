{-

uNESL: An instrumented interpreter for a NESL subset
Andrzej Filinski, University of Copenhagen

Version 0.04, 7 December 2016

This is a simplistic interpreter for a subset of the NESL language,
instrumented to also compute work and step costs of the expressions
evaluated. For educational purposes only: the absolute performance
is atrocious. Also, bugs are quite likely, and the current version
of the code is NOT a good example of proper Haskell style.

The main function is "nesl". Top level commands are of the form:

  exp                         Type-check and evaluare the NESL expression
  function f(x1..xn)[:type] = exp    Define a NESL function
  def x = exp                 Define a NESL value
  :load filename.nesl         Read ";"-terminated function defs from file
  :quit                       Exit the interpreter

Changelog:

 0.01   Initial "release"
 0.01a  Indicate (by comments) how to compile w/o Haskeline
 0.01b  Use ";" as separator in comprehensions, as per NESL docs.
        ("," still works, but is deprecated.)
 0.02   Added value definitions ("def x = exp")
 0.03   Added typing of functions (ignored for now)
 0.04   Adapted for GHC 7.10 Applicative => Monad brain damage.
-}

import Text.ParserCombinators.Parsec
import Data.Char (chr, ord)
import Data.List (transpose)
import Control.Monad.Trans (lift)
import Data.Bits ((.&.), (.|.), xor, shift)

-- Comment out the next line if you don't have (and can't install) Haskeline
-- On your system. (And comment out "hlrepl" and "nesl" below.)
import qualified System.Console.Haskeline as HL   

import qualified Control.Exception as C

---- Syntax

type Id = String

data AVal =
    IVal Int
  | RVal Double
  | BVal Bool
  | CVal Char
  deriving Eq

data Exp =
    Var Id
  | Lit AVal
  | Tup [Exp]
  | Vec [Exp]
  | If Exp Exp Exp
  | Let Pat Exp Exp
  | Over Exp [(Pat, Exp)] 
  | Call Id [Exp]
  | Time Exp
  | Tag SourcePos Exp  -- not really used yet
  deriving Show

data Pat =
    PVar Id
  | PWild 
  | PTup [Pat]
  deriving Show

data Def = 
    FDef Id [Id] Exp
  | VDef Id Exp

data Top =
    TExp Exp
  | TDef Def
  | TLoad String
  | TExit

----- Evaluation

type Vect a = [a] -- should really use an array here

data Val =
    AVal AVal
  | TVal [Val]
  | VVal (Vect Val)
  | FVal ([Val] -> Comp Val)

instance Show AVal where
  show (IVal i) = show i
  show (RVal r) = show r
  show (BVal b) = if b then "true" else "false"
  show (CVal c) = show c

instance Show Val where
  show (AVal a) = show a
  show (TVal vs) = "(" ++ showelts vs ++ ")"
  show (VVal vs) = "[" ++ showelts vs ++ "]"
  show (FVal _) = "<function>"

showelts :: [Val] -> String
showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs

type RCtx = String

newtype Comp a = Comp {rComp :: RCtx -> Either (a, Int, Int) String}
  
instance Monad Comp where
  return a = Comp (\c -> Left (a,0,0))
  t >>= f = Comp (\c -> case rComp t c of
              Left (a, w1, s1) -> case rComp (f a) c of
                                Left (b, w2, s2) -> Left (b, w1+w2, s1+s2)
                                Right e -> Right e
              Right e -> Right e)
  fail s = Comp (\c -> Right ("In " ++ c ++ ": " ++ s))

-- never used
instance Functor Comp where
  fmap f t = t >>= return . f

-- never used
instance Applicative Comp where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta

returnc :: (Int, Int) -> a -> Comp a
returnc (w,s) a = Comp (\c -> Left (a, w, s))

par :: [Comp a] -> Comp [a]
par [] = return []
par (t : ts) = 
  Comp (\c -> case rComp t c of
               Left (a, w1, s1) -> 
                 case rComp (par ts) c of
                   Left (as, w2, s2) -> Left (a:as, w1+w2, s1 `max` s2)
                   Right e -> Right e
               Right e -> Right e)

type Env = [(Id, Val)]

eval :: Exp -> Env -> Comp Val

eval (Var s) r = 
  case lookup s r of
    Just a -> return a
    Nothing -> error ("bad variable: " ++ s)
eval (Lit l) r = 
  return (AVal l)
eval (Tup es) r = 
  do vs <- mapM (\e -> eval e r) es
     return $ TVal vs
eval (Vec es) r = 
  do vs <- mapM (\e -> eval e r) es
     returnc (length vs, 1) $ VVal vs
eval (If e0 e1 e2) r =
  do AVal (BVal b) <- eval e0 r
     if b then eval e1 r else eval e2 r 
eval (Let p e1 e2) r =
  do v1 <- eval e1 r
     eval e2 (bind p v1 ++ r)
eval (Over e0 qs) r =
  do vs <- mapM (\(p, e) -> do VVal v <- eval e r; return v) qs
     let ls = map length vs
     let ps = map (\ (p, e) -> p) qs
     (if all (\ l -> l == head ls) (tail ls) then
       do vs0 <- par [eval e0 (concat (zipWith bind ps w) ++ r) 
                      | w <- transpose vs]
          return $ VVal vs0
      else fail $ "Over: length mismatch: " ++ show ls)
eval (Call i es) r =
  do vs <- mapM (\e -> eval e r) es
     case lookup i r of
       Just (FVal f) -> f vs
       Nothing -> error ("bad function: " ++ i)
eval (Time e) r =
  Comp (\c ->
    case rComp (eval e r) "Time" of
      Left (v, nw, ns) -> Left (TVal [v, AVal (IVal nw), AVal (IVal ns)], nw, ns)
      Right s -> Right s)
eval (Tag t e) r = 
  Comp (\c -> rComp (eval e r) (show t))

bind :: Pat -> Val -> Env
bind (PVar x) v = [(x,v)]
bind PWild v = []
bind (PTup ps) (TVal vs) = concat (zipWith bind ps vs)


primop :: ([AVal] -> AVal) -> Val
primop f = FVal (\as -> returnc (1,1) $ AVal (f [v | AVal v <- as]))
                           
cplus [IVal n1, IVal n2] = IVal (n1 + n2)
cplus [RVal n1, RVal n2] = RVal (n1 + n2)

cminus [IVal n1, IVal n2] = IVal (n1 - n2)
cminus [RVal n1, RVal n2] = RVal (n1 - n2)

cuminus [IVal n] = IVal (- n)
cuminus [RVal n] = RVal (- n)

ctimes [IVal n1, IVal n2] = IVal (n1 * n2)
ctimes [RVal n1, RVal n2] = RVal (n1 * n2)

cdiv [IVal n1, IVal n2] = IVal (n1 `div` n2)
cdiv [RVal n1, RVal n2] = RVal (n1 / n2)

cpow [IVal n1, IVal n2] = IVal (n1 ^ n2)
cpow [RVal n1, IVal n2] = RVal (n1 ^ n2)

cmin [IVal n1, IVal n2] = IVal (n1 `min` n2)
cmin [RVal n1, RVal n2] = RVal (n1 `min` n2)

cmax [IVal n1, IVal n2] = IVal (n1 `max` n2)
cmax [RVal n1, RVal n2] = RVal (n1 `max` n2)

cleq [IVal n1, IVal n2] = BVal (n1 <= n2)
cleq [RVal n1, RVal n2] = BVal (n1 <= n2)


seglist :: [Int] -> [a] -> [[a]]
seglist [] [] = []
seglist (n:ns) l = take n l : seglist ns (drop n l)

scatter :: [(Int, a)] -> Int -> [[a]]
scatter ps n = 
  let ins ls (i,x) = let (h:t) = drop i ls in take i ls ++ (x:h):t
  in map reverse (foldl ins (replicate n []) ps)


r0 :: Env
r0 = [("true", AVal (BVal True)),
      ("false", AVal (BVal False)),
      ("pi", AVal (RVal pi)),

      ("_plus", primop cplus),
      ("_minus", primop cminus),
      ("_uminus", primop cuminus),
      ("_times", primop ctimes),
      ("_div", primop cdiv),
      ("_mod", primop (\ [IVal n1, IVal n2] -> IVal (n1 `mod` n2))),
      ("pow", primop cpow),
      ("min", primop cmin),
      ("max", primop cmax),
      ("_and", primop (\ [IVal n1, IVal n2] -> IVal (n1 .&. n2))),
      ("_or", primop (\ [IVal n1, IVal n2] -> IVal (n1 .|. n2))),
      ("_xor", primop (\ [IVal n1, IVal n2] -> IVal (n1 `xor` n2))),
      ("lshift", primop (\ [IVal n1, IVal n2] -> IVal (shift n1 n2))),
      ("rshift", primop (\ [IVal n1, IVal n2] -> IVal (shift n1 (- n2)))),
      ("_eq", primop (\ [v1, v2] -> BVal (v1 == v2))),
      ("_leq", primop cleq),
      ("not", primop (\ [BVal b] -> BVal (not b))),
      ("chr", primop (\ [IVal n] -> CVal (chr n))),
      ("ord", primop (\ [CVal c] -> IVal (ord c))),
      ("real", primop (\ [IVal n] -> RVal (fromIntegral n))),
      ("floor", primop (\ [RVal n] -> IVal (floor n))),
      ("sqrt", primop (\ [RVal n] -> RVal (sqrt n))),
      ("sin", primop (\ [RVal n] -> RVal (sin n))),
      ("cos", primop (\ [RVal n] -> RVal (cos n))),
      ("ln", primop (\ [RVal n] -> RVal (log n))),
      ("exp", primop (\ [RVal n] -> RVal (exp n))),

      ("_length", FVal (\ [VVal vs] -> return $ AVal (IVal (length vs)))),
      ("_sub", FVal (\ [VVal vs, AVal (IVal n)] ->
                if 0 <= n && n < length vs then returnc (1,1) $ vs !! n
                else fail $ "bad subscript: " ++ show n ++ 
                            ", vector length: " ++ show (length vs))),

      ("index", FVal (\ [AVal (IVal n)] -> 
                          returnc (n,1) $ VVal [AVal (IVal i) | i <- [0..n-1]])),
      ("_append", FVal (\ [VVal v1, VVal v2] -> 
                         let v = v1 ++ v2
                         in returnc (length v,1) (VVal v))),

      ("concat", FVal (\ [VVal vs] -> 
                           let v = concat [v | VVal v <- vs]
                           in returnc (length v,1) (VVal v))),

      ("partition", FVal (\ [VVal vs, VVal ls] ->
                            let ns = [n | AVal (IVal n) <- ls]
                                l = length vs
                            in if sum ns == l then
                                 returnc (l,1) $ VVal [VVal v | v <- seglist ns vs]
                               else fail "partition: length mismatch")),
      ("scatter", FVal (\ [VVal vs, AVal (IVal n)] ->
                         let ps = [(i,a) | TVal [AVal (IVal i), a] <- vs]
                         in if all (\(i, _) -> 0 <= i && i < n) ps then
                              returnc (n+length ps,1) $ VVal (map VVal (scatter ps  n))
                         else fail "scatter: bad index")),

      ("error", FVal (\ [VVal s] -> fail [c | AVal (CVal c) <- s]))]
--  sum, scan

-------- Typing

type TyVar = Int

data Kind = 
    KType
  | KNum

data Type = 
    TCon String [Type]
  | TPar Int
  | TVar TyVar Kind

btype :: String -> Type
btype s = TCon s []

instance Show Type where
  show (TCon "()" ts) = 
   "(" ++ showl ts ++ ")" where
     showl [] = ""
     showl [x] = show x
     showl (x:xs) = show x ++ ", " ++ showl xs
  show (TCon "[]" [t]) = "[" ++ show t ++ "]"
  show (TCon "->" [t1,t2]) = show t1 ++ " -> " ++ show t2
  show (TCon s []) = s
  show (TPar n) = [chr (ord 'a' + n)]

data PType =
    PTBody Type
  | PTAbs Int Kind PType

instance Show PType where
  show (PTBody t) = show t
  show (PTAbs i KType pt) = "@" ++ show (TPar i) ++ "." ++ show pt
  show (PTAbs i KNum pt) = "@#" ++ show (TPar i) ++ "." ++ show pt

type TEnv = [(Id, PType)]

type UEnv = [(TyVar,Type)]

type TCtx = String

newtype Constr a = 
  Constr {rConstr :: UEnv -> TyVar -> TCtx -> Either (a, UEnv, TyVar) String}

instance Monad Constr where
  return a = Constr (\u n c -> Left (a, [], 0))
  t >>= f = Constr (\u n c -> case rConstr t u n c of
                             Left (a, u1, n1) -> 
                                case rConstr (f a) (u++u1) (n+n1) c of
                                   Left (b,u2,n2)-> Left (b,u1++u2,n1+n2)
                                   Right s -> Right s
                             Right s -> Right s)
  fail s = Constr (\u n c -> Right ("In " ++ c ++ ": " ++ s))

-- never used
instance Functor Constr where
  fmap f t = t >>= return . f

-- never used
instance Applicative Constr where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta

newvar :: Constr Type
newvar = Constr (\u n c -> Left (TVar n KType, [], 1))

newkvar k = Constr (\u n c -> Left (TVar n k, [], 1))

chase :: TyVar -> Kind -> UEnv -> Type
chase x k u = case lookup x u of
                Nothing -> TVar x k
                Just (TVar y k') -> chase y k' u
                Just t -> t

fetch :: Type -> Constr Type
fetch (TVar x k) = Constr (\u n c -> Left (chase x k u, [], 0))
fetch t = return t

-- clean up!
addb :: TyVar -> Kind -> Type -> Constr ()
addb x KType t = Constr (\u n c -> Left ((), [(x,t)], 0))
addb x KNum t@(TPar n) = Constr (\u n c -> Left ((), [(x,t)], 0))
addb x KNum t@(TCon "int" []) = Constr (\u n c -> Left ((), [(x,t)], 0))
addb x KNum t@(TCon "real" []) = Constr (\u n c -> Left ((), [(x,t)], 0))
addb x KNum t@(TCon s _) = fail $ "non-numeric type: " ++ s
addb x KNum t@(TVar _ KNum) = Constr (\u n c -> Left ((), [(x,t)], 0))
addb x KNum (TVar y KType) = Constr (\u n c -> Left ((), [(y,TVar x KNum)], 0))

nooccur :: TyVar -> Type -> Constr ()
nooccur x t =
  do t' <- fetch t
     case t' of
       TVar y _ -> if x == y then fail "circularity" else return ()
       TPar _ -> return ()
       TCon _ ts -> mapM_ (nooccur x) ts


gcon :: Type -> Type -> Constr ()
gcon t1 t2 = do t1' <- fetch t1
                t2' <- fetch t2
                gconc (t1', t2')

gconc (TVar x _, TVar y _) | x == y = return ()
gconc (TVar x k, t) = nooccur x t >> addb x k t
gconc (t, TVar x k) = nooccur x t >> addb x k t
gconc (TCon s1 ts1, TCon s2 ts2) =
  if s1 == s2 then
    if length ts1 == length ts2 then
      sequence_ (zipWith gcon ts1 ts2)
    else fail $ "clash: " ++ s1 ++ " found arity " ++
                show (length ts1) ++ " , expected " ++ show (length ts2)
  else fail $ "clash: found " ++ s1 ++ ", expected " ++ s2


chkExp :: Exp -> TEnv -> Constr Type
chkExp (Lit (IVal _)) g = return $ btype "int"
chkExp (Lit (RVal _)) g = return $ btype "real"
chkExp (Lit (BVal _)) g = return $ btype "bool"
chkExp (Lit (CVal _)) g = return $ btype "char"
chkExp (Var i) g = 
  case lookup i g of
    Just pt -> do t <- inst pt
                  return t
    Nothing -> fail ("Unbound var: " ++ i)
chkExp (Tup es) g = 
  do ts <- mapM (\e -> chkExp e g) es
     return $ TCon "()" ts
chkExp (Vec es) g =
  do t0 <- newvar
     mapM_ (\e -> do t <- chkExp e g; gcon t t0) es
     return $ TCon "[]" [t0]
chkExp (If e0 e1 e2) g =
  do t0 <- chkExp e0 g
     gcon t0 (btype "bool")
     t1 <- chkExp e1 g
     t2 <- chkExp e2 g
     gcon t2 t1
     return t1
chkExp (Let p e0 e1) g =
 do t0 <- chkExp e0 g
    (tp, gp) <- chkPat p 
    gcon t0 tp
    chkExp e1 (gp ++ g)
chkExp (Over e0 qs) g =
  do gs <- mapM (\ (p,e) -> do t <- chkExp e g
                               (tp, gp) <- chkPat p
                               gcon t (TCon "[]" [tp])
                               return gp) qs
     let g1 = concat gs -- should check disjointness
     t0 <- chkExp e0 (g1 ++ g)
     return $ TCon "[]" [t0]
chkExp (Call f es) g =
  case lookup f g of
    Nothing -> fail ("Unbound function: " ++ f)
    Just pt -> do tf <- inst pt
                  ts <- mapM (\e -> chkExp e g) es
                  t <- newvar
                  gcon (TCon "->" [TCon "()" ts, t]) tf
                  return t
chkExp (Time e) g =
  do t <- chkExp e g
     return $ TCon "()" [t, btype "int", btype "int"]
chkExp (Tag t e) g = 
  Constr (\u n c -> rConstr (chkExp e g) u n (show t))

chkPat :: Pat -> Constr (Type, TEnv)
chkPat (PVar x) = 
  do t <- newvar
     return (t, [(x,PTBody t)])
chkPat PWild = 
  do t <- newvar
     return (t, [])
chkPat (PTup ps) =
  do trs <- mapM chkPat ps
     return (TCon "()" (map fst trs), concat (map snd trs)) 
       -- should check disjointness

gener :: Type -> Constr PType
gener t =
  let abs [] n t = PTBody t
      abs (k:ks) n t = PTAbs n k (abs ks (n+1) t)
  in do (t', l) <- gener1 t []
        return $ abs l 0 t'

gener1 :: Type -> [Kind] -> Constr (Type, [Kind])
gener1 t l =
  do t0 <- fetch t
     case t0 of
       TVar x k -> do gcon (TVar x k) (TPar (length l))
                      return $ (TPar (length l), l++[k])
       TPar i -> return $ (TPar i, l)
       TCon s ts -> let gl [] m = return ([], m)
                        gl (t0:tr) m = do (t0',m') <- gener1 t0 m
                                          (tr',m'') <- gl tr m'
                                          return (t0':tr', m'')
                    in do (ts',l') <- gl ts l
                          return $ (TCon s ts', l')

chkDef :: Def -> TEnv -> Constr (Id, PType)
chkDef (FDef f xs e) g =
  do ts <- mapM (\x -> newvar) xs
     t0 <- newvar
     let tf = TCon "->" [TCon "()" ts, t0]
     let g1 = (f, PTBody tf) : zipWith (\x t -> (x,PTBody t)) xs ts ++ g
     te <- chkExp e g1
     gcon te t0
     ptf <- gener tf
     return (f, ptf)
chkDef (VDef x e) g =
  do te <- chkExp e g
     pte <- gener te
     return (x, pte)

subst :: Type -> Int -> Type -> Type
subst t0@(TPar x) y t1 = if x == y then t1 else t0
subst t@(TVar v k) y t1 = t
subst (TCon s ts) y t1 = TCon s (map (\t -> subst t y t1) ts)

inst :: PType -> Constr Type
inst (PTBody t) = return t
inst (PTAbs i k pt) = do tx <- newkvar k
                         t <- inst pt
                         return $ subst t i tx

---- Top-level interaction

type CEnv = (Env, TEnv)

doExp :: Exp -> CEnv -> IO ()
doExp e (vr,tr) =
  case rConstr (chkExp e tr >>= gener) [] 0 "Top" of
    Left (pt, _, _) -> 
      C.catch
        (case rComp (eval e vr) "TopExp" of
           Left (v, nw, ns) -> 
              do putStrLn (show v ++ " :: " ++ show pt)
                 putStrLn ("[Work: " ++ show nw ++ ", step: " ++ show ns ++ "]")
           Right s -> putStrLn ("Runtime error: " ++ s))
        (\e -> putStrLn ("Low-level error: " ++ show (e::C.SomeException)))
    Right s ->
      putStrLn ("Type error: " ++ s)

doDef :: Def -> CEnv -> IO (Maybe CEnv)
doDef d (r,g) =
  case rConstr (chkDef d g) [] 0 "TopDef" of
    Left ((i, pt), _, _) -> 
      case d of
        FDef i xs e -> 
          let f vs = Comp(\c -> rComp (eval e (zip xs vs ++ r1)) i)
              r1 = (i,FVal f) : r
          in do putStrLn ("Defined " ++ i ++ " :: " ++ show pt)
                return $ Just (r1, (i,pt):g)
        VDef x e ->
          case rComp (eval e r) ("Def " ++ x) of
            Left (v, _, _) -> 
              do putStrLn ("Defined " ++ x ++ " :: " ++ show pt)
                 return $ Just ((i,v):r, (x,pt):g)
            Right s -> 
              do putStrLn ("Runtime error in def " ++ x ++ ": " ++ s)
                 return Nothing
    Right s ->
      do putStrLn ("Type error in definition: " ++ s)
         return Nothing

doDefs :: [Def] -> CEnv -> IO CEnv
doDefs [] r = return r
doDefs (d:ds) r = do mr1 <- doDef d r
                     case mr1 of
                       Nothing -> return r
                       Just r1 -> doDefs ds r1

doTop :: Top -> CEnv -> IO CEnv
doTop (TExp e) r  =
  do doExp e r
     return r
doTop (TDef d) r =
  do mr1 <- doDef d r
     case mr1 of
       Nothing -> return r
       Just r1 -> return r1
doTop (TLoad fn) r =
  do s <- {-C.catch-} (readFile fn)
                 {- (\e -> print e >> return "")-}
     case parse parseDefs fn s of
       Right ds -> do r1 <- doDefs ds r
                      return r1
       Left err -> print err >> return r

brepl :: CEnv -> IO ()
brepl r = 
  do putStr "$> "
     s <- getLine
     case parse (do {whitespace; e <- parseTop; eof; return e}) "" s of
       Right TExit ->
         putStrLn "Bye!"
       Right tc ->
         doTop tc r >>= brepl
       Left s -> 
         print s >> brepl r

-- comment out startiing here if you don't have Haskeline
hlrepl :: CEnv -> HL.InputT IO ()
hlrepl r = 
  do ms <- HL.getInputLine "$> "
     case ms of
       Nothing -> return ()
       Just "" -> hlrepl r
       Just s ->
         case parse (do {whitespace; e <- parseTop; eof; return e}) "" s of
           Right TExit ->
             lift $ putStrLn "Bye!"
           Right tc ->
             lift (doTop tc r) >>= hlrepl
           Left s -> 
             lift (print s) >> hlrepl r

nesl :: IO ()
nesl = HL.runInputT HL.defaultSettings (hlrepl (r0, tr0))
-- comment out ending here if you don't have Haskeline (and use "bnesl")

bnesl :: IO ()
bnesl = brepl (r0, tr0)

main :: IO ()
main = nesl -- or bnesl


tfun :: [Type] -> Type -> PType
tfun ts t = PTBody (TCon "->" [TCon "()" ts, t])

tprimop :: [String] -> String -> PType
tprimop ts t = tfun (map btype ts) (btype t)


tr0 :: TEnv
tr0 = [("true", PTBody (btype "bool")),
       ("false", PTBody (btype "bool")),
       ("pi", PTBody (btype "real")),

       ("_plus", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("_minus", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("_uminus", PTAbs 0 KNum (tfun [TPar 0] (TPar 0))),
       ("_times", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("_div", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("_mod", (tfun [btype "int", btype "int"] (btype "int"))),
       ("pow", PTAbs 0 KNum (tfun [TPar 0, btype "int"] (TPar 0))),
       ("min", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("max", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (TPar 0))),
       ("_and", tprimop ["int", "int"] "int"),
       ("_or", tprimop ["int", "int"] "int"),
       ("_xor", tprimop ["int", "int"] "int"),
       ("lshift", tprimop ["int", "int"] "int"),
       ("rshift", tprimop ["int", "int"] "int"),
       ("_eq", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (btype "bool"))),
       ("_leq", PTAbs 0 KNum (tfun [TPar 0, TPar 0] (btype "bool"))),
       ("not", tprimop ["bool"] "bool"),
       ("chr", tprimop ["int"] "char"),
       ("ord", tprimop ["char"] "int"),
       ("real", tprimop ["int"] "real"),
       ("floor", tprimop ["real"] "int"),
       ("sqrt", tprimop ["real"] "real"),
       ("sin", tprimop ["real"] "real"),
       ("cos", tprimop ["real"] "real"),
       ("ln", tprimop ["real"] "real"),
       ("exp", tprimop ["real"] "real"),

       ("_length", PTAbs 0 KType (tfun [TCon "[]" [TPar 0]] (btype "int"))),
       ("_sub", PTAbs 0 KType (tfun [TCon "[]" [TPar 0], btype "int"] (TPar 0))),
       ("index", tfun [btype "int"] (TCon "[]" [btype "int"])),
       ("_append", PTAbs 0 KType (tfun [TCon "[]" [TPar 0], TCon "[]" [TPar 0]]
                                      (TCon "[]" [TPar 0]))),
       ("concat", PTAbs 0 KType (tfun [TCon "[]" [TCon "[]" [TPar 0]]]
                                      (TCon "[]" [TPar 0]))),
       ("partition", PTAbs 0 KType (tfun [TCon "[]" [TPar 0],
                                          TCon "[]" [btype "int"]]
                                         (TCon "[]" [TCon "[]" [TPar 0]]))),
       ("scatter", PTAbs 0 KType (tfun [TCon "[]" [TCon "()" [btype "int", TPar 0]],
                                        btype "int"]
                                       (TCon "[]" [TCon "[]" [TPar 0]]))),
       ("error", PTAbs 0 KType (tfun [TCon "[]" [btype "char"]] (TPar 0)))]


------- parsing

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
parseVar = do v <- letter <|> char '_'
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
              l <- parsePat `sepBy` (symbol ",")
              symbol ")"
              return $ PTup l

binop :: String -> Exp -> Exp -> Exp
binop s x y = Call s [x,y]

parseExp :: Parser Exp
parseExp =  do symbol "let"
               binds <- (do p <- parsePat
                            symbol "="
                            e1 <- parseExp
                            return (p,e1))
                        `sepBy1` (symbol ";")
               symbol "in"
               e2 <- parseExp
               return $ foldr (\(p,e1) e2 -> Let p e1 e2) e2 binds
            <|>
            do sp <- getPosition
               symbol "if"
               e1 <- parseExp
               symbol "then"
               e2 <- parseExp
               symbol "else"
               e3 <- parseExp
               return $ Tag sp (If e1 e2 e3)
            <|>
            do symbol "time"
               e <- parseExp
               return $ Time e
            <|>
            parseComp

parseComp = do e <- parseSum
               option e (do sp <- getPosition
                            o <- compop
                            e' <- parseSum
                            return $ Tag sp (o e e'))
             where compop = do {symbol "=="; return $ binop "_eq"} <|>
                            do {symbol "<="; return $ binop "_leq"} <|>
                            do {symbol ">="; return $ \x y -> binop "_leq" y x} <|>
                            do {symbol "<"; return $ \x y -> Call "not" [Call "_leq" [y,x]]} <|>
                            do {symbol ">"; return $ \x y -> Call "not" [Call "_leq" [x,y]]}


parseSum :: Parser Exp
parseSum = parseTerm `chainl1` (do sp <- getPosition
                                   o <- addop
                                   return $ \e1 e2 -> Tag sp (o e1 e2))
              where addop = do {try (symbol "++"); return $ binop "_append"} <|>
                            do {symbol "+"; return $ binop "_plus"} <|>
                            do {symbol "-"; return $ binop "_minus"} <|>
                          -- the following don't really belong at this 
                          -- precedence level
                            do {symbol "AND"; return $ binop "_and"} <|>
                            do {symbol "OR"; return $ binop "_or"} <|>
                            do {symbol "XOR"; return $ binop "_xor"}
                            

parseTerm :: Parser Exp
parseTerm = parsePrefix `chainl1` (do sp <- getPosition
                                      o <- mulop
                                      return $ \e1 e2 -> Tag sp (o e1 e2))
              where mulop = do {symbol "*"; return $ binop "_times"} <|>
                            do {symbol "/"; return $ binop "_div"} <|>
                            do {symbol "%"; return $ binop "_mod"} 

parsePrefix :: Parser Exp
parsePrefix =
  do sp <- getPosition
     symbol "#"
     e <- parsePrefix
     return $ Tag sp (Call "_length" [e])
  <|>
  do sp <- getPosition     
     symbol "&"
     e <- parsePrefix
     return $ Tag sp (Call "index" [e])
  <|>
  do sp <- getPosition
     symbol "-"
     e <- parsePrefix
     return $ Tag sp (Call "_uminus" [e])
  <|>
  parseSub

parseSub :: Parser Exp
parseSub = do e <- parseAtom
              ss <- many sub
              return $ foldl (\e1 (e2,p) -> Tag p (Call "_sub" [e1,e2])) e ss
            where sub = do sp <- getPosition 
                           symbol "["
                           e1 <- parseExp
                           symbol "]"
                           return (e1, sp)

parseAtom :: Parser Exp
parseAtom = do sp <- getPosition
               x <- parseVar
               (do symbol "("
                   es <- parseExp `sepBy` (symbol ",")
                   symbol ")"
                   return $ Tag sp (Call x es)
                <|>
                (return $ Tag sp (Var x)))
            <|>
            do s <- many1 digit
               (do char '.'
                   s2 <- many1 digit
                   whitespace
                   return $ Lit (RVal (read (s ++ "." ++ s2)))
                <|>
                do whitespace
                   return $ Lit (IVal (read s)))
            <|>
            do char '\''
               c <- anyChar
               char '\''
               whitespace
               return $ Lit (CVal c)
            <|>
            do char '"'
               s <- manyTill anyChar (try (char '"'))
               whitespace
               return $ Vec (map (Lit . CVal) s)
            <|>
            do symbol "("
               es <- parseExp `sepBy` (symbol ",")
               symbol ")"
               case es of
                 [e] -> return e
                 _ -> return $ Tup es
            <|>
            do symbol "{"
               e <- parseExp
               symbol ":"
               qs <- parseQual `sepBy1` (symbol ";" <|> symbol ",")
               me <- option Nothing
                      (do symbol "|"
                          e <- parseExp
                          return $ Just e)
               symbol "}"
               case me of
                 Nothing -> return $ Over e qs 
                 Just ef -> return $ Call "concat" [Over (If ef (Vec [e]) (Vec [])) qs]
--             return $ Over e qs me
            <|>
            do sp <- getPosition
               symbol "["
               es <- parseExp `sepBy` (symbol ",")
               symbol "]"
               return $ Tag sp (Vec es)

parseQual :: Parser (Pat, Exp)
parseQual = do p <- parsePat
               (symbol "<-" <|> symbol "in")
               e <- parseExp
               return (p, e)

parseType :: Parser Type
parseType = do parseAType `chainr1` (do symbol "->"
                                        return $ \t1 t2 -> TCon "->" [t1,t2])

parseAType :: Parser Type
parseAType = do symbol "int"
                return $ TCon "int" []
             <|>
             do symbol "float"
                return $ TCon "real" []
             <|>
             do symbol "bool"
                return $ TCon "bool" []
             <|>
             do symbol "char"
                return $ TCon "char" []
             <|>
             do symbol "["
                t <- parseType
                symbol "]"
                return $ TCon "[]" [t]
             <|>
             do symbol "("
                ts <- parseType `sepBy` (symbol ",")
                symbol ")"
                case ts of
                  [t] -> return t
                  _ -> return $ TCon "()" ts

parseDef :: Parser Def
parseDef = do symbol "function"
              f <- parseVar
              symbol "("
              as <- parseVar `sepBy` (symbol ",")
              symbol ")"
              option (TPar 0)
                     (do symbol ":"
                         parseType)
              symbol "="
              e <- parseExp         
              return $ FDef f as e
           <|> 
           do symbol "def"
              x <- parseVar
              symbol "="
              e <- parseExp         
              return $ VDef x e
            

parseDefs :: Parser [Def]
parseDefs = do whitespace
               ds <- many (do d <- parseDef; symbol ";"; return d)
               whitespace
               eof
               return ds

parseTop :: Parser Top
parseTop = do d <- parseDef
              return $ TDef d
           <|>
           do d <- symbol ":load"
              s <- many1 anyChar
              return $ TLoad s
           <|>              
           do d <- symbol ":quit"
              return $ TExit
           <|>
           do e <- parseExp
              return $ TExp e
