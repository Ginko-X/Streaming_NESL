{- Streaming NESL Syntax
-}

module SneslSyntax where

data AVal = IVal Int 
          | BVal Bool 
          deriving Eq 

--data CVal = AVal AVal
--          | CTVal CVal CVal
--          | VVal [CVal]  -- vector


data Val = AVal AVal  
         | TVal Val Val -- tuple
         | SVal [Val]   -- sequence
         | FVal ([Val] -> Snesl Val)  


type Id = String

data Exp = Var Id
         | Lit AVal    
         | Seq [Exp]   -- sequence
         | Tup Exp Exp   -- tuple
         | Let Exp Exp Exp  -- need correction
         | Call Id [Exp]     -- only for built-in functions
         | GComp Exp [(Exp,Exp)]  -- general comprehension
         | RComp Exp Exp    -- Restricted comprehension
         deriving Show


instance Show AVal where
    show (IVal i) = show i
    show (BVal b) = if b then "T" else "F"

--instance Show CVal where
--  show (AVal a) = show a
--  show (CTVal v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
--  show (VVal vs) = "[" ++ showelts vs ++ "]"


instance Show Val where
  show (AVal a) = show a
  show (TVal v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
  show (SVal vs) = "{" ++ showelts vs ++ "}"
  show (FVal _) = "<function>"


showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs



newtype Snesl a = Snesl {rSnesl :: Either (a, Int,Int) String}

instance Monad Snesl where
  return a = Snesl $ Left (a,0,0)

  m >>= f = Snesl $ 
    case rSnesl m of 
       Right err -> Right err
       Left (a,w,d) -> case rSnesl (f a) of 
                          Right err' -> Right err'
                          Left (a',w',d') -> Left (a',w+w',d+d')



instance Functor Snesl where
  fmap f t = t >>= return . f

instance Applicative Snesl where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta
