import SneslInterp
import SneslParser
import SvcodeInterp
import SneslCompiler
import SvcodeSyntax
import SneslSyntax
import SneslTyping
import DataTrans

import System.Environment

{-
main = do args <- getArgs
          case args of
            [file] -> runFile file 
            _ -> putStrLn "Input file error."



-- disregard the expression evaluation results
testExample' :: String -> IO()
testExample' prog =  
    case runProg prog of 
        Left err -> putStrLn err 
        Right ((_,w,s),tp, b, (_,w',s')) 
           -> if b then 
                     do putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
                                      ++ show s ++ "]"
                        putStrLn $ "SVCODE [work: " ++ show w' ++ ", step: " 
                                      ++ show s' ++ "]"
                   else 
                     do putStrLn $ "Error: SNESL and SVCODE results are different!"




-}

--runFile :: FilePath -> IO () 
runFile file =
   do prog <- readFile file 
      return $ runProg prog

-- formatted return value
--runProg :: String ->  Either String (Val,Int,Int) -- ((Val,Int,Int),Type,Bool,(Val,Int,Int)) 
runProg p = 
    do absProg <- parseString p
       sneslTyEnv <- runTyping absProg   
       (sneslInterEnv,w,s) <- runSneslInterp absProg
       svcode <- runCompiler absProg      
       svcodeRes <- runSvcodeProgs svcode
       return svcodeRes   
       --let (ids,svcodeval) = unzip svcodeRes
       --    (svVal, svcost) = unzip svcodeval
       --tps <- lookupMany ids sneslTyEnv
       --sneslVals <- lookupMany ids sneslInterEnv

       --let svVal' =  zipWith dataTransBack tps svVal 
       --    compRes = zipWith compareVal sneslVals svVal'  
       -- return (zip ids compRes)
       --return ((sneslInterEnv,w,s),sneslTy, compRes, (svcodeRes', w',s'))


--lookupMany :: [Id] -> [()] 


-- only for expressions
testExample :: String -> IO()
testExample prog =  
    case runExp prog of 
        Left err -> putStrLn err 
        Right ((v,w,s),tp, b, (sv,w',s')) 
           -> if b then 
                     do putStrLn $ show v ++ " :: " ++ show tp 
                        putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
                                      ++ show s ++ "]"
                        putStrLn $ "SVCODE [work: " ++ show w' ++ ", step: " 
                                      ++ show s' ++ "]"
                   else 
                     do putStrLn $ "Error: SNESL and SVCODE results are different!"
                        putStrLn $ "SNESL: " ++ show v 
                        putStrLn $ "SVCODE: " ++ show sv



runExp p = 
    do absProg <- parseStringExp p    
       sneslTy <- typingExp absProg    
       (sneslRes,w,s) <- runSneslExp absProg 
       svcode <- compiler absProg     
       (svcodeRes,(w',s')) <- runSvcodeProg svcode   
       let svcodeRes' = dataTransBack sneslTy svcodeRes
           compRes = compareVal sneslRes svcodeRes'  
       return ((sneslRes,w,s),sneslTy, compRes, (svcodeRes', w',s'))



-- helper functions for comparing a SNESL value and a SVCODE value
          
-- compare two SNESL values
compareVal :: Val -> Val -> Bool
compareVal (AVal (IVal i1)) (AVal (IVal i2)) = i1 == i2
compareVal (AVal (BVal b1)) (AVal (BVal b2)) = b1 == b2
compareVal (TVal v1 v2) (TVal v1' v2') = (compareVal v1 v1') && (compareVal v2 v2')
compareVal (SVal vs1) (SVal vs2) = 
    if length vs1 == length vs2 
    then all (\x -> x) $ zipWith compareVal vs1 vs2
    else False
compareVal _ _ = False 



-- some examples

manyTest ps = 
  let res = map runExp ps
  in  [ b | Right (_, _, b, _) <- res ]


progs = [prog1,prog2,prog3,prog4,prog5,prog6,prog7,prog8,prog9, prog10]
                          

-- An example program: compute all the primes less than the number 'count'
prog1 = "let count = 50; " ++
        "    rs1 = {{{x+1 | a / (x+1) * (x+1) == a} : x in &a}: a in &count} ;"  ++
         "   rs2 = {reducePlus(concat(z)): z in rs1} "  ++
        "in  concat({{x | x+1 == y}: x in &count, y in rs2})"


-- An example for '_append', a = {{{0, 1}}, {{3}}} , b = {{{4}},    {{5, 9}}}
prog2 = "let a = {{&2|T}|T} ++ {{{3|T}|T} |T} ; "++  
        "    b = {{{4|T}|T}|T} ++ {{{5|T} ++ {9|T}|T}|T} " ++ 
        " in {x ++ y : x in a, y in b}"

-- same as prog2, using primitive sequences instead of guards
prog3 = "let a = {{&2}} ++ {{{3}}} ; "++  
        "    b = {{{4}}} ++ {{{5} ++ {9}}} " ++ 
        " in {a ++ b : _ in &2}"

-- more bug-fixed  programs

prog4 = "let n = 10 in {{x: _ in &n} : x in &n}" -- #8

prog5 = "{concat({}{int}) : _ in &2}" -- #5

prog6 = "let x = &5 in let x = {x: _ in &2} in x "

prog7 = "let x = ({&2|T}, {3|T}) in x"

prog8 = "let x = {(1,2) : _ in &2} in {{x|T} : _ in &3}"

prog9 = "let x = &2 in {{x: _ in &a} : a in &3}"

prog10 = "{}{int}++{{1}}"

-- Wrong example; shoulg throw an Excpetion
prog11 = "let x = 5; (y,z) = x in 5"  
