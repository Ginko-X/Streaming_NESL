module TypeTrans where

import SneslSyntax
import SvcodeSyntax
import SneslParser
import SneslInterp

-- type transformation from SNESL to SVCODE
typeTrans :: Val -> SvVal

typeTrans (AVal a) = SSVal [a]

typeTrans (SVal vs) = SPVal vs' fs
    where vs' = case vs of 
                     [] -> SSVal []
                     v@(x:xs) -> case x of 
                         (AVal _) -> concatAval v 
                         (SVal _) -> flatPair $ map typeTrans v 
                         (TVal _ _) -> flatPair $ map typeTrans v 
          fs = SSVal $ map BVal $ i2flags (length vs)  

typeTrans (TVal v1 v2) = SPVal (typeTrans v1) (typeTrans v2)


concatAval :: [Val] -> SvVal
concatAval as = SSVal $ map (\(AVal a) -> a) as 

-- [(SPVal v f), (SPVal v f),...]  -> SPVal vs fs
flatPair :: [SvVal] -> SvVal
flatPair ps = SPVal (concatSsval ss) (concatSsval fs)
    where (ss,fs) = unzip $ map (\(SPVal v f) -> (v,f)) ps

concatSsval :: [SvVal] -> SvVal
concatSsval ss@(x:xs) = case x of 
    SSVal _ -> SSVal $ concat $ map (\(SSVal v) -> v) ss 
    SPVal _ _ -> flatPair ss 


i2flags :: Int -> [Bool]
i2flags i = replicate i (False) ++ [True]



-- type transformation from SVCODE to SNESL
typeTransBack :: SvVal -> Val

typeTransBack (SSVal [a]) = AVal a

typeTransBack (SPVal s1@(SPVal _ _) (SSVal s2)) = 
    nestPair (TVal s1' s2')     
    where s1' = typeTransBack s1 
          s2' = SVal $ map AVal s2

typeTransBack (SPVal s1@(SSVal s1') s2@(SSVal s2')) = 
      TVal (SVal $ map AVal s1') (SVal $ map AVal s2')

         
nestPair :: Val -> Val

nestPair (TVal s1'@(SVal s1) (SVal s2)) = 
  if isFlattened s1 s2
  then s1'
  else if (length s1) == (length s2) 
       then SVal $ zipWith TVal s1 s2
       else SVal $ flagSeg s1 (map (\(AVal s) -> s) s2)

nestPair (TVal s1 s2) = nestPair (TVal s1' s2)
  where s1' = nestPair s1 



isFlattened :: [Val] -> [Val] -> Bool
isFlattened vs fs = i2flags (length vs) == (map (\(AVal (BVal b)) -> b) fs)


-- e.g. {1,2,3} <F,F,T,T,F,T> => {{1,2}, {}, {3}}
flagSeg :: [Val] -> [AVal] -> [Val]
flagSeg vs flags =  segSeq seglen vs 
    where seglen = flags2len flags'          
          flags' = map (\(BVal b) -> b) flags

segSeq :: [Int] -> [Val] -> [Val]
segSeq [] [] = []
segSeq (n:ns) vs = (SVal $ take n vs) : segSeq ns (drop n vs) 



testExample = typeTransBack $ typeTrans example1


-- { {{3,4}},{2}}, {{}}, {{1}} }
example1 = (SVal [SVal [SVal [(AVal (IVal 3)),(AVal (IVal 4))], 
                       SVal [(AVal (IVal 2))]], 
                 SVal [SVal []],
                 SVal [SVal [(AVal (IVal 1))]]])
-- (((<3, 4, 2, 1>,<False, False, True, False, True, True, False, True>),
--   <False, False, True, False, True, False, True>),<False, False, False, True>)


-- { ({3,4},T), ({},F), ({7,8},F)}
example2 = (SVal [TVal (SVal [(AVal (IVal 3)), AVal (IVal 4)]) (AVal (BVal True)), 
                  TVal (SVal []) (AVal (BVal False)), 
                  TVal (SVal [(AVal (IVal 7)), (AVal (IVal 8))]) (AVal (BVal False))])
-- (((<3, 4, 7, 8>,<False, False, True, True, False, False, True>),
--       <True, False, False>),<False, False, False, True>)

-- typeTransBack example
-- ((<3,1,4,1,5,9>, <F F F T T F T F F T>), <F F F F T> )
example3 = (SPVal (SPVal (SSVal [(IVal 3),(IVal 1), (IVal 4), (IVal 1), (IVal 5), (IVal 9)]) 
                         (SSVal [(BVal False), (BVal False), (BVal False), (BVal True), 
                                  (BVal True), (BVal False), (BVal True), (BVal False), 
                                  (BVal False), (BVal True)])) 
                  (SSVal [ (BVal False), (BVal False), (BVal False), (BVal False), (BVal True)]))


