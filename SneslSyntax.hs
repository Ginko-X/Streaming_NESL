{- Streaming NESL Syntax
-}

module SneslSyntax where

import Data.Bits ((.&.))


data AVal = IVal Int 
          | BVal Bool 
          deriving Eq --,Show) 


data Val = AVal AVal  
         | TVal Val Val 
         | SVal [Val]   
         | FVal ([Val] -> Snesl Val) 


type Id = String

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


data Def = FDef Id [Id] Exp  -- function definition
         | EDef Id Exp   -- expression definition
         --deriving Show



data Pat = PVar Id
         | PWild 
         | PTup Pat Pat
  --deriving Show


instance Show AVal where
    show (IVal i) = show i
    show (BVal b) = if b then "T" else "F"


instance Show Val where
  show (AVal a) = show a
  show (TVal v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
  show (SVal vs) = "{" ++ showelts vs ++ "}"
  show (FVal _) = "<function>"

instance  Show Pat where
  show (PVar i)  = i
  show PWild = "_"
  show (PTup p1 p2) = "(" ++ show p1 ++ "," ++ show p2 ++ ")"

instance Show Def where
  show (FDef fname args e) = "<function> " ++ fname
  show (EDef i e) = i ++ ": " ++ show e 


showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs



-- Snesl types
data Type = TInt 
          | TBool
          | TTup Type Type
          | TSeq Type
          | TFun ([Type] -> SneslTyping Type)
          --deriving Eq 


instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TTup t1 t2) = "(" ++ show t1 ++ "," ++ show t2 ++ ")"
  show (TSeq t) = "{" ++ show t ++"}" 
  show (TFun _) = "<function>"


instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  (TTup t1 t2) == (TTup t3 t4) = (t1 == t3) .&. (t2 == t4)
  (TSeq t1) == (TSeq t2) = t1 == t2
  _ == _ = False

type SneslTyping a = Either String a 


newtype Snesl a = Snesl {rSnesl :: Either String (a, Int,Int)}

instance Monad Snesl where
  return a = Snesl $ Right (a,0,0)

  m >>= f = Snesl $ 
    case rSnesl m of 
       Left err -> Left err
       Right (a,w,d) -> case rSnesl (f a) of 
                          Left err' -> Left err'
                          Right (a',w',d') -> Right (a',w+w',d+d')



instance Functor Snesl where
  fmap f t = t >>= return . f

instance Applicative Snesl where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta
