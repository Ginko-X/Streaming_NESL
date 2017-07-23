import SneslInterp
import SneslParser
import SneslCompiler
import SneslSyntax
import SvcodeSyntax
import SneslTyping
import DataTrans

-- provide two kinds of SVCODE eager interperter inside
import SvcodeInterp

-- provide three streaming SVCODE interpreters:
-- 1. bufferSize 1, simple loop-scheduler
-- 2. arbitrary buffer size, full-filled loop-scheduling with stealing
-- 3. arbitrary buffer size, two-phase scheduler
--import SvcodeProcInterp 
--import SvcodeProcInterpLong 
import SvcodeSInterp  


import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans (lift)
import Data.Foldable (foldlM)
import Numeric (showGFloat)

{- Usage: 
   <expression>   Evaluate an expression (also include type-check, compiling
                    to SVCODE, and comparison of SNESL and SVCODE results)

   <function>     Syntax: function f(x1:type1,...,xn:typen):type = expression

   :l <file>      Load functions from a file

   :d <exp> <file> Generate the DAG file that can be used in graph-easy

   :r <exp> <Int> Interpret the `exp` streamingly in a round robin fashion 
                  and print out the first `int` rounds of Svctx (unless all
                  the Procs already shutdown before that round)

   :c <exp>       Display the translated SVCODE program

   :bs <Int>      Set buffer size, i.e., the maximum number of elements 
                  a buffer can hold

   :m <T/F>       F:SIMD(default),T:MIMD, only works for streaming interpreter

   :q             Exit
-}


type InterEnv = (SEnv, TyEnv, VEnv, FEnv, Int,Bool)

ie0 :: InterEnv
ie0 = (se0,tyEnv0, compEnv0,[], 10, False)


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

runTop b (TExp e) env@(_,_,_,_,bufSize,_) = 
    case runExp b e env of 
      Left err -> putStrLn err >> return env             
      Right (v,t,(w,s),(w',s')) ->
          do putStrLn $ show v ++ " :: " ++ show t
             putStrLn $ "[W_H: " ++ show w ++ ", S_H: " ++ show s ++ "]" 
               ++ ", " ++ "[W_L: " ++ show w' ++ ", S_L: "++ show s' ++ "]" 
             let d = map fromIntegral [w,s,w',s',bufSize] 
             if b then putStrLn $ "S_L/(W_H/bufSize + S_H): " 
                         ++ showGFloat (Just 1) (d!!3/( d!!0 / d!!4 + d!!1)) ""
             else putStrLn $ "W_L/W_H: " ++ showGFloat (Just 1) (d!!2/ d!!0) ""                 
                    ++ ", S_L/S_H: " ++ showGFloat (Just 1) (d!!3/ d!!1) ""
             return env 


runTop b (TFile file) env =
    do str <- readFile file
       case runFile b str env of 
           Right env' -> (putStrLn $ "Loading file done.") >> return env'
           Left err -> putStrLn err >> return env


runTop _ (TDag e fname) env@(_,_,v0,_,_,_) = 
    case runCompileExp e v0  of
         Right svcode -> geneDagFile svcode fname >> return env 
         Left err -> putStrLn err >> return env 


runTop _ (TRr e count) env@(_,_,v0,_,bs,_) = 
    case (do code <- runCompileExp e v0; runSvcodePExp' code count bs) of
         Right ctx -> putStr ctx >> return env 
         Left err -> putStrLn err >> return env 


runTop _ (TCode e) env@(_,_,v0,_,_,_) = 
    case runCompileExp e v0 of
         Right code -> putStr (show code) >> return env 
         Left err -> putStrLn err >> return env 

runTop _ (TBs bs) env@(e0,t0,v0,f0,_,mflag) = 
    do putStr $ "Buffer size: " ++ show bs ++ "\n"
       return (e0,t0,v0,f0,bs,mflag)

runTop _ (TMflag f) env@(e0,t0,v0,f0,bs,_) = 
    do if f then putStr $ "set MIMD model" ++ "\n"
         else putStr $ "set SIMD model" ++ "\n"
       return (e0,t0,v0,f0,bs,f)


runExp :: Bool -> Exp -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
runExp b e env@(e0,t0,v0,f0,bs,mflag) = 
    do sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0        
       svcode <- runCompileExp e v0  
       (svcodeRes,(w',s')) <- if b then runSvcodePExp svcode bs mflag else runSvcodeExp svcode f0
       --(svcodeRes,(w',s')) <- runSvcodeExp svcode f0
       svcodeRes' <- dataTransBack sneslTy svcodeRes
       if compareVal sneslRes svcodeRes' 
         then return (sneslRes, sneslTy,(w,s),(w',s')) 
         else Left $ "SNESL and SVCODE results are different:" ++ show sneslRes 
                     ++ ", " ++ show svcodeRes' 

  
runDef :: Bool -> Def -> InterEnv -> Either String InterEnv
runDef b def env@(e0,t0,v0,f0,bs,mflag) = 
   do funcTyEnv <- runTypingDefs [def] t0
      sneslEnv <- runSneslInterpDefs [def] e0 
      (ve,fe) <- (if b then runSCompileDefs else runCompileDefs) [def] (v0,f0) 
      return (sneslEnv,funcTyEnv,ve,fe,bs,mflag)


runFile :: Bool -> String -> InterEnv -> Either String InterEnv
runFile b str env = 
   do funcs <- runParseDefs str 
      foldlM (\e def -> runDef b def e) env funcs  


-- old API, for interpreting an independent expression
--testString :: String -> InterEnv -> Either String (Val,Type,(Int,Int),(Int,Int)) 
testString str env@(e0,t0,v0,f0,bs,mflag) = 
    do e <- runParseExp str
       sneslTy <- runTypingExp e t0   
       (sneslRes,w,s) <- runSneslExp e e0 
       svcode <- runCompileExp e v0
       --(svcodeRes, (w',s')) <- runSvcodeExp svcode f0  -- eager interp
       (svcodeRes, (w',s')) <- runSvcodePExp svcode bs mflag -- streaming interp       
       svcodeRes' <- dataTransBack sneslTy svcodeRes
       if compareVal sneslRes svcodeRes'  
         then return (sneslRes, sneslTy,(w,s),(w',s')) 
         else fail $ "SNESL and SVCODE results are different." ++ show sneslRes 
                      ++ " " ++ show svcodeRes'


geneExpCode :: String -> [SInstr]
geneExpCode str = 
  case runParseExp str of 
    Right e -> case runCompileExp e compEnv0 of 
                 Right (SFun _ _ code _) -> code 
                 Left _ -> []
    Left _ -> []

geneExpSFun :: String -> Either String SFun
geneExpSFun str = 
  do e <- runParseExp str 
     runCompileExp e compEnv0 
 

testGeneLev prog = 
  let code = geneExpCode prog -- "{x+y : x in &2, y in {10,20}}" 
      (sup,dag) = geneSupDag code 0 
  in geneLevels dag sup   


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
prog1 = "let count = 10; " ++
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

prog13 = "let s = {1,3,5,2,4,6}; " ++  -- #18 real deadlock when bs < 3
            " t = concat({{x| x%2==0 }: x in s}); " ++ 
            " u = concat({{x| x%2 > 0 }: x in s}) " ++ 
            " in {a + b : a in t, b in u}"
