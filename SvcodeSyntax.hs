module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

data Instr = Ctrl 
           | EmptyCtrl 
           | WithCtrl SId [SDef] STree Type -- 'Type' is the type of the return value
           | ToFlags SId
           | Usum SId
           | Const AVal
           | MapConst SId AVal
           | MapOne OP SId
           | MapTwo OP SId SId
           | SegscanPlus SId SId 
           | ReducePlus SId SId
           | Pack SId SId
           | UPack SId SId
           | Distr SId SId
           | SegDistr SId SId
           | SegFlagDistr SId SId SId
           | PrimSegFlagDistr SId SId SId 
           | B2u SId
           | SegConcat SId SId
           | SegMerge SId SId 
           | InterMergeS [SId]
           | SegInterS [(SId,SId)]
           | PriSegInterS [(SId,SId)]            
           deriving Show
         
data SDef = SDef SId Instr  -- deriving Show 

data SSym = SSym [SDef] STree  -- deriving Show 


data OP = Uminus | Not  -- unary 
        | Add | Minus | Times | Div | Equal | Leq   -- binary
        deriving (Eq,Show)

type OpEnv = [(OP, [SvVal] -> SvVal)]
opEnv0 = [(Uminus, \[SIVal as] -> SIVal $ map (\x -> -x) as),
          (Not, \[SBVal as] -> SBVal $ map not as),
          (Add, \[SIVal as, SIVal bs] -> SIVal $ zipWith (+) as bs),
          (Minus, \[SIVal as, SIVal bs] -> SIVal $ zipWith (-) as bs),
          (Times, \[SIVal as, SIVal bs] -> SIVal $ zipWith (*) as bs),
          (Div, \[SIVal as, SIVal bs] -> SIVal $ zipWith div as bs),
          (Equal, \[SIVal as, SIVal bs] -> SBVal $ zipWith (==) as bs),
          (Leq, \[SIVal as, SIVal bs] -> SBVal $ zipWith (<=) as bs)]


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

