{- Data representation transformations between SNESL and SVCODE values
 -}

module DataTrans where

import SneslSyntax
import SvcodeSyntax
import SneslParser
import SneslInterp

-- Snesl types
data Type = TInt 
          | TBool
          | TTup Type Type
          | TSeq Type


--- data transformation from SNESL to SVOCDE ---

dataTrans :: Type -> Val -> SvVal
dataTrans TInt (AVal (IVal i)) = SIVal [i]
dataTrans TBool (AVal (BVal b)) = SBVal [b]
dataTrans (TSeq t) (SVal vs) = SSVal vs' fs   
    where vs' = seqTrans t vs 
          fs = i2flags $ length vs 
dataTrans (TTup t1 t2) (TVal v1 v2) = SPVal v1' v2'
    where v1' = dataTrans t1 v1  
          v2' = dataTrans t2 v2 


--dataTransBack :: Type -> SvVal -> Val 



seqTrans :: Type -> [Val] -> SvVal

-- [1,2] -> <1,2>
seqTrans TInt vs = SIVal vs' 
    where vs' = [a | (AVal (IVal a)) <- vs]
         
seqTrans TBool vs = SBVal vs'
    where vs' = [a | (AVal (BVal a)) <- vs]

seqTrans (TSeq t) vs = SSVal vss fs  
    where vs' = [s | SVal s <- vs ]
          vss = concatSsval t (map (seqTrans t) vs') 
          fs =  concat $ map (i2flags.length) vs' 

seqTrans (TTup t1 t2) vs = SPVal vs1 vs2
    where vsp = unzip [(v1, v2) | (TVal v1 v2) <- vs]  
          vs1 = seqTrans t1 (fst vsp)
          vs2 = seqTrans t2 (snd vsp)


concatSsval :: Type -> [SvVal] -> SvVal 
concatSsval TInt vs = SIVal $ concat [is | (SIVal is) <- vs] 
concatSsval TBool vs = SBVal $ concat [bs | (SBVal bs) <- vs]  
concatSsval (TSeq t) vs = SSVal vs' fs'
    where (s1,s2) = unzip [(s ,f)| (SSVal s f) <- vs] -- SSVal $ concat $ map (\(SSVal v) -> v) ss 
          vs' = concatSsval t s1
          fs' = concat s2
concatSsval (TTup t1 t2) vs = SPVal s1' s2'
    where  (s1,s2) = unzip [(v1,v2) | (SPVal v1 v2) <-vs ]
           s1' = concatSsval t1 s1
           s2' = concatSsval t2 s2 


i2flags :: Int -> [Bool]
i2flags i = replicate i (False) ++ [True]


--- transformation from SVCODE to SNESL ----

seqTransBack :: Type -> SvVal -> [Val]

seqTransBack TInt (SIVal vs) = vs'
    where vs' = [AVal (IVal i) | i <- vs]

seqTransBack TBool (SBVal vs) = vs' 
    where vs' = [AVal (BVal b) | b <- vs] 

seqTransBack (TSeq t) (SSVal vs fs) = vs'
    where vss = seqTransBack t vs
          vs' = flagSeg vss fs  

seqTransBack (TTup t1 t2) (SPVal v1 v2) = vs'
    where v1s = seqTransBack t1 v1 
          v2s = seqTransBack t2 v2 
          vs' = zipWith TVal v1s v2s

-- e.g. {1,2,3} <F,F,T,T,F,T> => {{1,2}, {}, {3}}
flagSeg :: [Val] -> [Bool] -> [Val]
flagSeg vs flags =  segSeq seglen vs 
    where seglen = flags2len flags          


segSeq :: [Int] -> [Val] -> [Val]
segSeq [] [] = []
segSeq (n:ns) vs = (SVal $ take n vs) : segSeq ns (drop n vs) 




---- examples  ---

-- SNESL: [{{3,4}},{2}}, {{}}, {{1}}]
testExample1 = seqTransBack type1 $ seqTrans type1 example1
type1 = TSeq (TSeq TInt)
example1 = [SVal [SVal [(AVal (IVal 3)),(AVal (IVal 4))], 
                       SVal [(AVal (IVal 2))]], 
                 SVal [SVal []],
                 SVal [SVal [(AVal (IVal 1))]]]
-- SVCODE: 
-- ((<3, 4, 2, 1>,[False,False,True,False,True,True,False,True]),
-- [False,False,True,False,True,False,True])


-- SNESL : { ({3,4},T), ({},F), ({7,8},F)}
-- SVCODE: ((<3, 4, 7, 8>,[False,False,True,True,False,False,True]),<True, False, False>)
testExample2 = seqTransBack type2 $ seqTrans type2 example2
type2 = TTup (TSeq TInt) TBool
example2 = [TVal (SVal [(AVal (IVal 3)), AVal (IVal 4)]) (AVal (BVal True)), 
                  TVal (SVal []) (AVal (BVal False)), 
                  TVal (SVal [(AVal (IVal 7)), (AVal (IVal 8))]) (AVal (BVal False))]



---- seqTransBack example
---- ((<3,1,4,1,5,9>, <F F F T T F T F F T>), <F F F F T> )
testExample3 = seqTransBack type3 example3
type3 = TSeq TInt
example3 = (SSVal (SIVal [3,1,4,1,5,9])
                  [False,False,False,True, True, False,True,False,False,True])


