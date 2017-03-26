module TypeTrans where

import SneslSyntax
import SvcodeSyntax
import SneslParser


typeTrans :: Val -> SvVal

typeTrans (AVal a) = SSVal [a]

typeTrans (SVal vs) = SPVal vs' fs
    where vs' = case vs of 
                     [] -> SSVal []
                     v@(x:xs) -> case x of 
                         (AVal _) -> concatAval v 
                         (SVal _) -> flatPair $ map typeTrans v 
                         (TVal _ _) -> flatPair $ map typeTrans v 
                         (VVal _) ->  
          fs = SSVal $ map BVal $ i2flags (length vs)  



typeTrans (TVal v1 v2) = SPVal (typeTrans v1) (typeTrans v2)



lenVal :: Val -> Int
lenVal (VVal vs) = length vs
lenVal (SVal vs) = length vs


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


testExample :: SvVal
testExample = typeTrans example2

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
--   	<True, False, False>),<False, False, False, True>)


typeTransBack :: SvVal -> Val
typeTransBack (SSVal [a]) = AVal a