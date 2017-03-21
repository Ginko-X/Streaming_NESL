{- Streaming NESL Syntax
   reference from PFP SNESL Lecture slides and Madsen's PhD Thesis Page 33-34 
-}

module SneslSyntax where

data AVal = IVal Int 
          | BVal Bool 
          | RVal Double
          | CVal Char
          deriving Eq 

data Val = AVal AVal 
         | TVal [Val]
         | VVal [Val]
         | SVal [Val]
         | FVal ([Val] -> Snesl Val)  

type Id = String

data Exp = Var Id
         | Lit AVal    
         | Seq [Exp]   -- sequence
         | Tup [Exp]   -- tuple
         | Let Exp Exp Exp 
         | Call Id [Exp]  -- only for built-in functions
         | GComp Exp [(Exp,Exp)]  -- general comprehension
         | RComp Exp Exp   -- Restricted comprehension
         deriving Show


instance Show AVal where
    show (IVal i) = show i
    show (BVal b) = show b
    show (RVal r) = show r
    show (CVal c) = show c  


instance Show Val where
  show (AVal a) = show a
  show (TVal vs) = "(" ++ showelts vs ++ ")"
  show (VVal vs) = "[" ++ showelts vs ++ "]"
  show (SVal vs) = "{" ++ showelts vs ++ "}"
  show (FVal _) = "<function>"


showelts :: [Val] -> String
showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs


type RCtx = String

newtype Snesl a = Snesl {rSnesl :: RCtx -> Either (a,Int,Int) String}

instance Monad Snesl where
    return a = Snesl $ \c -> Left (a,0,0)

    m >>= f = Snesl $ \c -> 
        case rSnesl m c of
            Right err -> Right err
            Left (a,w,d) -> case rSnesl (f a) c of 
                                Right err' -> Right err'
                                Left (a',w',d') -> Left (a',w+w',d+d')

    fail s = Snesl $ \c -> Right ("In " ++ c ++ ": " ++ s)


instance Functor Snesl where
  fmap f t = t >>= return . f

instance Applicative Snesl where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta
