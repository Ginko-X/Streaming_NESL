module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

data Instr = Ctrl              
           | ToFlags SId
           | Usum SId
           | MapAdd SId SId
           | MapConst SId AVal
           | SegscanPlus SId SId 
           | ReducePlus SId SId
           | Pack SId SId
           | UPack SId SId
           | Distr SId SId
           | SegDistr SId SId
           | SegFlagDistr SId SId SId
           | PrimSegFlagDistr SId SId SId 
           | B2u SId
           | MapEqual SId SId
           | SegConcat SId SId
           | SegMerge SId SId --
           | InterMerge SId SId
           | SegInter SId SId SId SId  -- segment interleave for flags
           | PriSegInter SId SId SId SId  -- segment interleave for primitive streams
           | MapTimes SId SId
           | MapDiv SId SId 
           | Empty PType
           deriving Show
           
data SDef = SDef SId Instr  -- deriving Show 

data SSym = SSym [SDef] STree  -- deriving Show 

data PType = SInt | SBool deriving Show



instance Show SDef where
  show (SDef sid i) = "S" ++ show sid ++ " := " ++ show i 


instance Show SSym where
  show (SSym sdefs t) = "\n" ++ showseq "; \n" sdefs ++ "\nReturn: " ++ show t

-- svcode values
data SvVal = SIVal [Int]
           | SBVal [Bool] 
           | SSVal SvVal [Bool]  -- Sequence
           | SPVal SvVal SvVal -- Pair
           --deriving Show

instance Show SvVal where
    show (SIVal is) = "Ints <" ++ showseq "," is ++ ">"
    show (SBVal bs) = "Bools <" ++ showseq "," bs ++ ">"    
    show (SSVal v bs) = "Seq (" ++ show v ++ ","++ show bs ++")"    
    show (SPVal v1 v2) = "Pair (" ++ show v1 ++"," ++ show v2 ++ ")"


showseq delim [] = ""
showseq delim [x] = show x
showseq delim (x:xs) = show x ++ delim ++ showseq delim xs

-- stream trees 
data STree = STId SId
           | STPair STree STree
           deriving Show

