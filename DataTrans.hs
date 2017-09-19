{- Data representation transformations between SNESL and SVCODE values
 -}

module DataTrans where

import SneslSyntax
import SvcodeSyntax
import SneslTyping


--- data transformation from SNESL to SVOCDE ---

dataTrans :: Type -> Val -> SvVal
dataTrans TInt (AVal (IVal i)) = SIVal [i]
dataTrans TBool (AVal (BVal b)) = SBVal [b]
dataTrans (TSeq t) (SVal vs) = SSVal vs' fs 
    where vs' = concatSsval t $ map (dataTrans t) vs 
          fs = i2flags $ length vs  
dataTrans (TTup t1 t2) (TVal v1 v2) = SPVal v1' v2'
    where v1' = dataTrans t1 v1  
          v2' = dataTrans t2 v2 


concatSsval :: Type -> [SvVal] -> SvVal 
concatSsval TInt vs = SIVal $ concat [is | (SIVal is) <- vs] 
concatSsval TBool vs = SBVal $ concat [bs | (SBVal bs) <- vs]  
concatSsval (TSeq t) vs = SSVal vs' fs'
    where (s1,s2) = unzip [(s ,f)| (SSVal s f) <- vs] 
          vs' = concatSsval t s1
          fs' = concat s2
concatSsval (TTup t1 t2) vs = SPVal s1' s2'
    where  (s1,s2) = unzip [(v1,v2) | (SPVal v1 v2) <-vs ]
           s1' = concatSsval t1 s1
           s2' = concatSsval t2 s2 


---- transformation from SVCODE to SNESL ------

dataTransBack :: Type -> SvVal -> Either String Val
dataTransBack t v = 
  do vs <- seqTransBack t v 
     case vs of 
       [v] -> return v 
       _ -> return $ SVal vs

--dataTransBack TInt (SIVal [i]) = return $ AVal $ IVal i 
--dataTransBack TBool (SBVal [b]) = return $ AVal $ BVal b

--dataTransBack (TSeq t) (SSVal vs fs) = 
--    do vs' <- seqTransBack t vs
--       return $ SVal vs'

--dataTransBack (TTup t1 t2) (SPVal v1 v2) = 
--    do v1' <- dataTransBack t1 v1 
--       v2' <- dataTransBack t2 v2 
--       return $ TVal v1' v2'

--dataTransBack t v = Left $ "dataTransBack: type and value does not match:" 
--                           ++ show t ++ "," ++ show v 


seqTransBack :: Type -> SvVal -> Either String [Val]

seqTransBack TInt (SIVal vs) = Right vs'
    where vs' = [AVal (IVal i) | i <- vs]

seqTransBack TBool (SBVal vs) = Right vs' 
    where vs' = [AVal (BVal b) | b <- vs] 

seqTransBack (TSeq t) (SSVal vs fs) = 
    do vss <- seqTransBack t vs
       return $ flagSeg vss fs  

seqTransBack (TTup t1 t2) (SPVal v1 v2) = 
    do v1s <- seqTransBack t1 v1 
       v2s <- seqTransBack t2 v2 
       return $ zipWith TVal v1s v2s

seqTransBack t v = Left $ "seqTransBack: type and value does not mismatch."
                           ++ show t ++ "," ++ show v                            


-- e.g. {1,2,3} <F,F,T,T,F,T> => {{1,2}, {}, {3}}
flagSeg :: [Val] -> [Bool] -> [Val]
flagSeg vs flags =  segSeq seglen vs 
    where seglen = flags2len flags          


segSeq :: [Int] -> [Val] -> [Val]
segSeq [] [] = []
segSeq (n:ns) vs = (SVal $ take n vs) : segSeq ns (drop n vs) 




{--- examples  ---

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
-}
