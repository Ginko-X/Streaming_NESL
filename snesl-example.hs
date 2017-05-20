import SneslInterp
import SneslParser
import SvcodeInterp
import SneslCompiler
import SvcodeSyntax
import SneslSyntax
import SneslTyping
import DataTrans

import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans (lift)

type InterEnv = (SEnv, TyEnv, VEnv, FEnv)

ie0 :: InterEnv
ie0 = (se0,tyEnv0, compEnv0,[])


main :: IO ()
main = runInputT defaultSettings (loop ie0)
   where
       loop :: InterEnv -> InputT IO ()
       loop env = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just "" -> loop env
               Just input -> 
                   case runParseTop input of
                     Right TExit -> lift $ putStrLn "Bye!" 
                     Right top -> lift (runTop top env) >>= loop
                     Left err -> (lift $ putStrLn err) >> loop env



runTop :: Top -> InterEnv -> IO InterEnv
runTop (TDef def@(FDef fname _ _ _)) env =
    case runDef def env of 
      Right env' -> (putStrLn $ "Defined function: " ++ fname) >> return env'
      Left err -> putStrLn err >> return env

runTop (TExp e) env = 
    case runExp e env of 
      Left err -> do putStrLn err; return env
      Right (v,t,(w,s),(w',s')) -> -- do putStrLn $ show v; return env
          do putStrLn $ show v ++ " :: " ++ show t
             putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
                          ++ show s ++ "]"
             putStrLn $ "SVCODE [work: " ++ show w' ++ ", step: " 
                          ++ show s' ++ "]"
             return env 


runTop (TFile file) env =
    do str <- readFile file
       case runFile str env of 
           Right env' -> (putStrLn $ "Loading file done.") >> return env'
           Left err -> putStrLn err >> return env



runExp :: Exp -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
runExp e env@(e0,t0,v0,f0) = 
    do sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0 
       svcode <- runCompileExp e v0 f0     
       (svcodeRes,(w',s')) <- runSvcodeExp svcode   
       let svcodeRes' = dataTransBack sneslTy svcodeRes
           compRes = compareVal sneslRes svcodeRes'  
       if compRes then return (sneslRes, sneslTy,(w,s),(w',s')) 
           else fail "SNESL and SVCODE results are different." 

  
runDef ::  Def -> InterEnv -> Either String InterEnv
runDef def env@(e0,t0,v0,f0) = 
   do funcTyEnv <- runTypingDefs [def] t0
      sneslEnv <- runSneslInterpDefs [def] e0 
      svcodeEnv <- runCompileDefs [def] v0 f0 
      return $ addInterEnv (sneslEnv,funcTyEnv,[],svcodeEnv) env 


runFile :: String -> InterEnv -> Either String InterEnv
runFile str env@(e0,t0,v0,f0) = 
   do funcs <- runParseDefs str 
      funcTyEnv <- runTypingDefs funcs t0
      sneslEnv <- runSneslInterpDefs funcs e0
      svcodeEnv <- runCompileDefs funcs v0 f0
      return $ addInterEnv (sneslEnv,funcTyEnv,[],svcodeEnv) env 



addInterEnv :: InterEnv -> InterEnv -> InterEnv
addInterEnv (a1,a2,a3,a4) (b1,b2,b3,b4) = (a1++b1, a2++b2,a3++b3,a4++b4)


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

--runString :: String -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
runString str env@(e0,t0,v0,f0) = 
    do e <- runParseExp str
       sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0 
       svcode <- runCompileExp e v0 f0 
       return svcode    
       (svcodeRes,(w',s')) <- runSvcodeExp svcode   
       let svcodeRes' = dataTransBack sneslTy svcodeRes
           compRes = compareVal sneslRes svcodeRes'  
       if compRes then return (sneslRes, sneslTy,(w,s),(w',s')) 
           else fail "SNESL and SVCODE results are different." 


-- old API, for interpreting an independent expression
testExample :: String -> IO()
testExample str = 
    case runString str ie0 of 
        Left err' -> putStrLn err' 
        Right (v,tp,(w,s),(w',s')) ->
               do putStrLn $ show v ++ " :: " ++ show tp 
                  putStrLn $ "SNESL [work: " ++ show w ++ ", step: "
                                ++ show s ++ "]"
                  putStrLn $ "SVCODE [work: " ++ show w' ++ ", step: " 
                                ++ show s' ++ "]"
  

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
  case mapM runParseExp ps of 
    Left err -> fail err 
    Right es -> 
        do res <- mapM (\e -> runExp e ie0) es
           return "All correct!"



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
