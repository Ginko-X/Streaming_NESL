module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

-- streaming operations/transducers
data Instr = Ctrl              
           | ToFlags SId
           | Usum SId
           | MapAdd SId SId
           | MapConst SId AVal
           | SegscanPlus SId SId 
           | Pack SId SId
           | UPack SId SId
           | Iotas SId
           | Distr SId SId
           | B2u SId
           deriving Show
data SDef = SDef SId Instr deriving Show 

data SSym = SSym [SDef] STree deriving Show -- STree is the returned streams


--instance Show Instr where
--  show Ctrl = "Ctrl"

--  show (ToFlags sid) = "ToFlags "++ "S" ++ show sid
--  show (Usum sid) = "Usum "++ "S" ++ show sid
--  show (Iotas sid) = "Iotas "++ "S" ++ show sid
--  show (B2u sid) = "B2u "++ "S" ++ show sid

  
--  show (MapAdd s1 s2) = "MapAdd " ++ "S" ++ show s1 ++ " S" ++ show s2
--  show (MapConst s1 a) = "MapConst " ++ "S" ++ show s1 ++ " " ++ show a 
--  show (SegscanPlus s1 s2) = "SegscanPlus " ++ "S" ++ show s1 ++ " S" ++ show s2
--  show (Pack s1 s2) = "Pack " ++ "S" ++ show s1 ++ " S" ++ show s2
--  show (UPack s1 s2) = "UPack " ++ "S" ++ show s1 ++ " S" ++ show s2
--  show (Distr s1 s2) = "Distr " ++ "S" ++ show s1 ++ " S" ++ show s2


--instance Show SDef where
--  show (SDef sid i) = "S" ++ show sid ++ " := " ++ show i 


--instance Show SSym where
--  show (SSym sdefs t) = "\n" ++ showseq "; \n" sdefs ++ "\nReturn: " ++ show t

-- svcode values
data SvVal = SIVal [Int]
           | SBVal [Bool] 
           | SSVal SvVal [Bool]  -- Sequence
           | SPVal SvVal SvVal -- Pair
           deriving Show
--instance Show SvVal where
--    show (SIVal is) = "<" ++ showseq "," is ++ ">"
--    show (SBVal bs) = "<" ++ showseq "," bs ++ ">"    
--    show (SSVal v bs) = "(" ++ show v ++ ","++ show bs ++")"    
--    show (SPVal v1 v2) = "(" ++ show v1 ++"," ++ show v2 ++ ")"


showseq delim [] = ""
showseq delim [x] = show x
showseq delim (x:xs) = show x ++ delim ++ showseq delim xs

-- stream trees 
data STree = STId SId
           | STPair STree STree
           deriving Show

