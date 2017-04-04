import SneslInterp
import SneslParser
import SvcodeInterp
import SneslCompiler
import SvcodeSyntax
import SneslSyntax
import SneslTyping
import DataTrans


-- An example program: split a string into words
prog1 = 
    "let x = \" streaming   nesl \" ; " ++  -- a string as argument
    "    len = #tab(x)       ;" ++   -- length 
    "    idx = &len          ;" ++   -- indices 
    "    xfilter = {c: c in x| not(ord(c)==32)} ;"++  -- remove the spaces
    "    vx = tab(x)  ;  "++          -- convert sequence to vector
    -- get the indices of spaces -- never used 
    "    spaceidx = {i: i in idx | ord(vx[i])== 32} ;"++ 
    "    y = {let b = ord(c)==32 ; "++ -- set flag sequence for partition 
                " the({True | b} ++ {False | not(b)}) : c in x } ;"++ 
    "    y2 = y ++ {True|True} ;"++  -- add a "True" flag as the end
    "    res = part(xfilter,y2) "++  -- partition into words
    "in {seg: seg in res | not(#tab(seg)==0)} " -- remove spaces


-- average 
prog2 = "let ((x,y),z) = ((10,1),100) " ++
        --"    y = &10   ;" ++
        --"    xy = zip(x,y) " ++ 
        "in  x+y+z "

prog3 = "let x = 2 in {x+y : y in &10 }"

compareVal :: Val -> Val -> Bool
compareVal (AVal (IVal i1)) (AVal (IVal i2)) = i1 == i2
compareVal (AVal (BVal b1)) (AVal (BVal b2)) = b1 == b2
compareVal (TVal v1 v2) (TVal v1' v2') = (compareVal v1 v1') && (compareVal v2 v2')
compareVal (SVal vs1) (SVal vs2) = all (\x -> x) $ (zipWith (\v1 v2 -> compareVal v1 v2) vs1 vs2)
compareVal _ _ = False 



compValSvVal :: Val -> Type -> SvVal -> Bool 
compValSvVal v t sv = compareVal v v'
    where v' = dataTransBack t (if isSeq t then sv' else sv)
          sv' = pair2seq t sv 
          

pair2seq :: Type -> SvVal -> SvVal
pair2seq (TSeq TInt) (SPVal v1 (SBVal v2)) = SSVal v1 v2 
pair2seq (TSeq TBool) (SPVal v1 (SBVal v2)) = SSVal v1 v2
pair2seq (TSeq (TTup _ _)) (SPVal v1@(SPVal _ _) (SBVal v2)) = SSVal v1 v2
pair2seq (TSeq t) (SPVal v1 (SBVal v2)) = SSVal (pair2seq t v1) v2        



--testExample :: String ->  Either String (Val,Type,Bool)
testExample p = 
    do absProg <- parseString p 
       sneslTy <- typing absProg

       sneslRes <- runSneslInterp absProg

       svcode <- compiler absProg

       svcodeRes <-runProg svcode
       let compRes = compValSvVal sneslRes sneslTy svcodeRes
       
       return (sneslRes,sneslTy, compRes)
