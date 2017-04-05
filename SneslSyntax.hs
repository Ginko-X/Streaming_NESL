{- Streaming NESL Syntax
-}

module SneslSyntax where

data AVal = IVal Int 
          | BVal Bool 
          deriving Eq 


data Val = AVal AVal  
         | TVal Val Val -- tuple
         | SVal [Val]   -- sequence
         | FVal ([Val] -> Snesl Val) 


type Id = String

data Exp = Var Id
         | Lit AVal    
         | Tup Exp Exp  
         | Let Pat Exp Exp  
         | Call Id [Exp]     
         | GComp Exp [(Pat,Exp)] 
         | RComp Exp Exp    
         deriving Show


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
  show (SVal vs) = "{" ++ showelts vs ++ "}"
  show (FVal _) = "<function>"


showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs



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
