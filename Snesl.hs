import SneslInterp
import SneslParser
import SvcodeInterp
import SneslCompiler
import SvcodeSyntax
import SneslSyntax
import SneslTyping
import DataTrans
--import SvcodeSInterp
import SvcodeProcInterp 

import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans (lift)


{- Usage: 
   <expression>   Evaluate an expression (also include type-check, compiling
                    to SVCODE, and comparison of SNESL and SVCODE results)
   <function>     Syntax: function f(x1:type1,...,xn:typen):type = expression
   :l <file>   Load functions from a file
   :q          
-}


type InterEnv = (SEnv, TyEnv, VEnv, FEnv)

ie0 :: InterEnv
ie0 = (se0,tyEnv0, compEnv0,[])


main :: IO ()
main = do args <- getArgs
          case args of
            ["-s"] -> sCompiler True
            _ -> sCompiler False 


sCompiler :: Bool -> IO ()
sCompiler b =
    runInputT defaultSettings (loop ie0)
    where
         loop :: InterEnv -> InputT IO ()
         loop env = do
             minput <- getInputLine "> "
             case minput of
                 Nothing -> return ()
                 Just "" -> loop env
                 Just input -> 
                     case runParseTop input of
                       Right TExit -> return ()
                       Right top -> (lift $ runTop b top env) >>= loop
                       Left err -> (lift $ putStrLn err) >> loop env



runTop :: Bool -> Top -> InterEnv -> IO InterEnv
runTop b (TDef def@(FDef fname _ _ _)) env =
    case runDef b def env of 
      Right env' -> (putStrLn $ "Defined function: " ++ fname) >> return env'
      Left err -> putStrLn err >> return env

runTop b (TExp e) env = 
    case runExp b e env of 
      Left err -> putStrLn err >> return env             
      Right (v,t,(w,s),(w',s')) ->
          do putStrLn $ show v ++ " :: " ++ show t
             putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
                          ++ show s ++ "]" ++ ", " ++ 
                        "SVCODE [work: " ++ show w' ++ ", step: " 
                          ++ show s' ++ "]"
             putStrLn $ "SVCODE/SNESL ratio [work: " 
                ++ show (ceiling $ (fromIntegral w')/ (fromIntegral w)) 
                ++ ", step: " ++
                 show ((ceiling $ (fromIntegral s')/ (fromIntegral s))) ++ "]" 
             return env 


runTop b (TFile file) env =
    do str <- readFile file
       case runFile b str env of 
           Right env' -> (putStrLn $ "Loading file done.") >> return env'
           Left err -> putStrLn err >> return env



runExp :: Bool -> Exp -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
runExp b e env@(e0,t0,v0,f0) = 
    do sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0        
       svcode <- runCompileExp e v0     
       --(svcodeRes,(w',s')) <- if b then runSvcodeSExp svcode else runSvcodeExp svcode f0
       (svcodeRes,(w',s')) <- runSvcodeExp svcode f0       
       svcodeRes' <- dataTransBack sneslTy svcodeRes
       if compareVal sneslRes svcodeRes' 
         then return (sneslRes, sneslTy,(w,s),(w',s')) 
         else Left $ "SNESL and SVCODE results are different:" ++ show sneslRes 
                     ++ ", " ++ show svcodeRes' 

  
runDef :: Bool -> Def -> InterEnv -> Either String InterEnv
runDef b def env@(e0,t0,v0,f0) = 
   do funcTyEnv <- runTypingDefs [def] t0
      sneslEnv <- runSneslInterpDefs [def] e0 
      --(ve,fe) <- (if b then runSCompileDefs else runCompileDefs) [def] (v0,f0) 
      (ve,fe) <-  runCompileDefs [def] (v0,f0) 
      return (sneslEnv,funcTyEnv,ve,fe)


runFile :: Bool -> String -> InterEnv -> Either String InterEnv
runFile b str env@(e0,t0,v0,f0) = 
   do funcs <- runParseDefs str 
      funcTyEnv <- runTypingDefs funcs t0
      sneslEnv <- runSneslInterpDefs funcs e0
      --(ve,fe) <- (if b then runSCompileDefs else runCompileDefs) funcs (v0,f0)
      (ve,fe) <- runCompileDefs funcs (v0,f0)
      return (sneslEnv,funcTyEnv,ve,fe)



-- old API, for interpreting an independent expression
--testString :: String -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
testString str env@(e0,t0,v0,f0) = 
    do e <- runParseExp str
       sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0 
       svcode <- runCompileExp e v0
       return svcode
       --(svcodeRes, (w',s')) <- runSvcodeExp svcode f0
       --svcodeRes' <- dataTransBack sneslTy svcodeRes
       --if compareVal sneslRes svcodeRes'  
       --  then return (sneslRes, sneslTy,(w,s),(w',s')) 
       --  else fail "SNESL and SVCODE results are different." 



--testExample :: String -> IO()
--testExample str = 
--    case testString str ie0 of 
--        Left err' -> putStrLn err' 
--        Right (v,tp,(w,s),(w',s')) ->
--               do putStrLn $ show v ++ " :: " ++ show tp 
--                  putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
--                                ++ show s ++ "]"
--                  putStrLn $ "SVCODE [work: " ++ show w' ++ ", step: " 
--                                ++ show s' ++ "]"
  


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
manyTest :: [String] -> Either String String
manyTest ps = 
  do mapM_ (\e -> testString e ie0) ps
     return "All correct!"



progs = [prog1,prog2,prog3,prog4,prog5,prog6,prog7,prog8,prog9, prog10, prog11]
                          

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

prog11 = "let bs = {{T,T},{F,T,F,F,T,T}} in {part(a,b): a in {{}int, {4,5,6}}, b in bs}"


-- bads example; should throw an Excpetion
prog12 = "let x = 5; (y,z) = x in 5"  
