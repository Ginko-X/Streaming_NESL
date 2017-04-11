import SneslInterp
import SneslParser
import SvcodeInterp
import SneslCompiler
import SvcodeSyntax
import SneslSyntax
import SneslTyping
import DataTrans


-- An example program: compute all the primes less than the number 'count'
prog1 = "let count = 50; " ++
        "    rs1 = {{{x+1 | a / (x+1) * (x+1) == a} : x in &a}: a in &count} ;"  ++
         "   rs2 = {reducePlus(concat(z)): z in rs1} "  ++
        "in  concat({{x | x+1 == y}: x in &count, y in rs2})"


-- An example for '_append' 
-- a = {{{0, 1}}, {{3}}} 
-- b = {{{4}},    {{5, 9}}}
prog2 = "let a = {{&2|T}|T} ++ {{{3|T}|T} |T} ; "++  
        "    b = {{{4|T}|T}|T} ++ {{{5|T} ++ {9|T}|T}|T} " ++ 
        " in {x ++ y : x in a, y in b}"
-- correct result: {{{0, 1}, {4}}, {{3}, {5, 9}}}

prog3 = "let a = {{&2|T}|T} ++ {{{3|T}|T} |T} ; "++  
        "    b = {{{4|T}|T}|T} ++ {{{5|T} ++ {9|T}|T}|T} " ++ 
        " in {a ++ b : _ in &2}"


-- the last 'Bool' indicates the comparison result 
testExample :: String ->  Either String (Val,Type,Bool) 
testExample p = 
    do absProg <- parseString p    -- parse the SNESL expression
       sneslTy <- typing absProg    -- get the expression's type
       sneslRes <- runSneslInterp absProg  -- SNESL interpreting result
       svcode <- compiler absProg     -- SVCODE generated from the SNESL expression
       svcodeRes <-runSvcodeProg svcode    -- SVCODE interpreting result
       --return (sneslRes, svcode, svcodeRes)
       let compRes = compValSvVal sneslRes sneslTy svcodeRes  -- compare the two results       
       return (sneslRes,sneslTy,compRes)




compValSvVal :: Val -> Type -> SvVal -> Bool 
compValSvVal v t sv = compareVal v v'
    where v' = dataTransBack t (if isSeq t then sv' else sv)
          sv' = pair2seq t sv 
          

compareVal :: Val -> Val -> Bool
compareVal (AVal (IVal i1)) (AVal (IVal i2)) = i1 == i2
compareVal (AVal (BVal b1)) (AVal (BVal b2)) = b1 == b2
compareVal (TVal v1 v2) (TVal v1' v2') = (compareVal v1 v1') && (compareVal v2 v2')
compareVal (SVal vs1) (SVal vs2) = all (\x -> x) $ (zipWith (\v1 v2 -> compareVal v1 v2) vs1 vs2)
compareVal _ _ = False 



pair2seq :: Type -> SvVal -> SvVal
pair2seq (TSeq TInt) (SPVal v1 (SBVal v2)) = SSVal v1 v2 
pair2seq (TSeq TBool) (SPVal v1 (SBVal v2)) = SSVal v1 v2
pair2seq (TSeq (TTup _ _)) (SPVal v1@(SPVal _ _) (SBVal v2)) = SSVal v1 v2
pair2seq (TSeq t) (SPVal v1 (SBVal v2)) = SSVal (pair2seq t v1) v2        

