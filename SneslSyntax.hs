module SneslSyntax where

-- Refer to Madsen's PhD Thesis Page 33-34

data AVal = IVal Int 
          | BVal Bool 
          | RVal Double
          | CVal Char 
          deriving (Eq, Show)

data Val = AVal AVal  --
         | TVal [Val]
         | VVal [Val]
         | SVal [Val]
         deriving Eq

type Id = String

data Exp = Var Id
         | Lit AVal 
         | Seq [Exp]
         | Tup [Exp]  
         | Idx Exp Exp 
         | Let Exp Exp Exp  -- let-bindings
         | Call Id [Exp]  -- application of built-in functions
         | GComp Exp [(Exp, Exp)]  -- general comprehension
         | RComp Exp Exp -- Restricted 
         deriving Show


instance Show Val where
  show (AVal a) = show a
  show (TVal vs) = "(" ++ showelts vs ++ ")"
  show (VVal vs) = "[" ++ showelts vs ++ "]"
  show (SVal vs) = "{" ++ showelts vs ++ "}"

showelts :: [Val] -> String
showelts [] = ""
showelts [x] = show x
showelts (x:xs) = show x ++ ", " ++ showelts xs