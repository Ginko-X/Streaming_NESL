module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

-- streaming operations/transducers
data Instr = Ctrl              
           | ToFlags SId
           | Usum SId
           | MapAdd SId SId
           | MapConst SId AVal
           | Segscan SId SId
           | Pack SId SId
           | Iotas SId
           deriving Show

data SDef = SDef SId Instr deriving Show 

data SSym = SSym [SDef] STree deriving Show  -- STree is the returned streams

-- svcode values
data SvVal = SIVal [Int]
           | SBVal [Bool] 
           | SSVal SvVal [Bool]  -- Sequence
           | SPVal SvVal SvVal -- Pair

instance Show SvVal where
    show (SIVal is) = "<" ++ showseq is ++ ">"
    show (SBVal bs) = "<" ++ showseq bs ++ ">"    
    show (SSVal v bs) = "(" ++ show v ++ ","++ show bs ++")"    
    show (SPVal v1 v2) = "(" ++ show v1 ++"," ++ show v2 ++ ")"



--data SvPVal = SIVal [Int]
--            | SBVal [Bool]          

--data SvVal = SvPVal SvPVal
--           | SSVal SvPVal [Bool]  -- Sequence
--           | SPVal SvVal SvVal -- Pair


--instance Show SvPVal where
--    show (SIVal is) = "<" ++ showseq is ++ ">"
--    show (SBVal bs) = "<" ++ showseq bs ++ ">"    


--instance Show SvVal where
--    show (SvPVal v) = show v
--    show (SSVal v bs) = "(" ++ show v ++ ","++ show bs ++")"    
--    show (SPVal v1 v2) = "(" ++ show v1 ++"," ++ show v2 ++ ")"

showseq [] = ""
showseq [x] = show x
showseq (x:xs) = show x ++ ", " ++ showseq xs

-- stream trees 
data STree = STnil 
           | STcons SId STree
           deriving Show