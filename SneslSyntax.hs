{- Streaming NESL Syntax -}

module SneslSyntax where

import Data.List (union)


data AVal = IVal Int 
          | BVal Bool 
          deriving Eq --,Show) 


data Val = AVal AVal  
         | TVal Val Val 
         | SVal [Val] 
         | FVal ([Val] -> Snesl Val) 


type Id = String

type FId = String

data Exp = Var Id
         | Lit AVal    
         | Tup Exp Exp 
         | SeqNil Type 
         | Seq [Exp]
         | Let Pat Exp Exp  
         | Call Id [Exp]     
         | GComp Exp [(Pat,Exp)] 
         | RComp Exp Exp    
         deriving Show


data Def = FDef FId [(Id,Type)] Type Exp  -- function definition
         -- | EDef Id Exp   -- expression definition
         --deriving Show



data Pat = PVar Id
         | PWild 
         | PTup Pat Pat
  deriving Show


instance Show AVal where
    show (IVal i) = show i
    show (BVal b) = if b then "T" else "F"


instance Show Val where
  show (AVal a) = show a
  show (TVal v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
  show (SVal vs) = "{" ++ showelts vs "," ++ "}"
  show (FVal _) = "<function>"

--instance  Show Pat where
--  show (PVar i)  = i
--  show PWild = "_"
--  show (PTup p1 p2) = "(" ++ show p1 ++ "," ++ show p2 ++ ")"

instance Show Def where
  show (FDef fname _ _ _) = "function: " ++ fname
  --show (EDef i e) = i ++ ": " ++ show e 


showelts :: Show a => [a] -> String -> String
showelts [] _ = ""
showelts [x] _ = show x
showelts (x:xs) symbol = show x ++ symbol ++ (showelts xs symbol)


type TId = String 

-- Snesl types
data Type = TInt 
          | TBool
          | TTup Type Type
          | TSeq Type
          | TFun [Type] Type 
          | TVar ([Type] -> SneslTyping Type)  -- type variable
          --deriving Eq 


instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TTup t1 t2) = "(" ++ show t1 ++ "," ++ show t2 ++ ")"
  show (TSeq t) = "{" ++ show t ++"}" 
  show (TFun tps tp) = showelts tps "->" ++ "->" ++ show tp
  show (TVar _)  = "<Type Var>" 


instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  (TTup t1 t2) == (TTup t3 t4) = (t1 == t3) && (t2 == t4)
  (TSeq t1) == (TSeq t2) = t1 == t2
  (TFun tps1 t1) == (TFun tps2 t2) = (tps1 == tps2) && (t1 == t2)
  _ == _ = False 


newtype SneslTyping a = SneslTyping {rTyping :: Either String a}

instance Monad SneslTyping where
  return a = SneslTyping $ Right a 

  m >>= f = SneslTyping $ 
      case rTyping m of 
        Right a -> case rTyping (f a) of 
                     Right a' -> Right a'
                     Left err' -> Left err'
        Left err -> Left err

  fail err = SneslTyping $ Left err

instance Functor SneslTyping where
  fmap f t = t >>= return . f

instance Applicative SneslTyping where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta




type SEnv = [(Id, Val)]

newtype Snesl a = Snesl {rSnesl :: SEnv -> Either String (a, Int,Int)}

instance Monad Snesl where
  return a = Snesl $ \ _ -> Right (a,0,0)

  m >>= f = Snesl $ \ env ->
    case rSnesl m env of 
       Left err -> Left err
       Right (a,w,d) -> case rSnesl (f a) env of 
                          Left err' -> Left err'
                          Right (a',w',d') -> Right (a',w+w',d+d')
 
  fail err = Snesl $ \ _ -> Left err


instance Functor Snesl where
  fmap f t = t >>= return . f

instance Applicative Snesl where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



-- get the free varibales in the expression
getVars :: Exp -> [Id]
getVars (Var x) = [x]
getVars (Lit a) = []
getVars (Tup e1 e2) = foldl union [] $ map getVars [e1,e2]
getVars (SeqNil tp) = []
getVars (Seq es) = foldl union [] $ map getVars es
getVars (Let p e1 e2) = e1Vars ++ filter (\x -> not $ x `elem` binds) (getVars e2) 
    where binds = getPatVars p 
          e1Vars = getVars e1 

getVars (Call fname es) = foldl union [] $ map getVars es 

getVars (GComp e0 ps) = pVars ++ filter (\x -> not $ x `elem` binds) e0vs
    where e0vs = getVars e0
          binds = foldl union [] $ map (\(p,_) -> getPatVars p) ps
          pVars = foldl union [] $ map (\(_,e) -> getVars e) ps 

getVars (RComp e0 e1) = foldl union [] $ map getVars [e0,e1]


getPatVars :: Pat -> [Id]
getPatVars (PVar x) = [x]
getPatVars PWild = [] 
getPatVars (PTup p1 p2) = concat $ map getPatVars [p1,p2] 



i2flags :: Int -> [Bool]
i2flags i = replicate i (False) ++ [True]



-- [f,f,t,t,f,t] -> [2,0,1]
flags2len :: [Bool] -> [Int]
flags2len fs = zipWith (\x y -> x-y-1) tidx (-1:(init tidx)) 
               where tidx = [t| (t,True) <- (zip [0..(length fs)-1] fs)] 
                     len = length fs
                    
-- [3,2] -> [FFT FT] => [[FFT],[FT]] -- i.e. partition
seglist :: [Int] -> [a] -> [[a]]
seglist [] [] = []
seglist (n:ns) l = take n l : seglist ns (drop n l)