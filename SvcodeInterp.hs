{- Svcode Interpreter -}

module SvcodeInterp where

import SvcodeSyntax
import SneslSyntax
import SneslCompiler (tree2Sids)
import DataTrans (i2flags)
import SneslInterp (flags2len, seglist, wrapWork)
import SvcodeProc

import Data.List (transpose)


type Svctx = [(SId, SvVal)]

newtype Svcode a = Svcode {rSvcode :: Svctx -> SId -> (Int, Int) -> FEnv -> 
                                  Either String (a,(Int,Int), Svctx)}

instance Monad Svcode where
    return a = Svcode $ \ ctx ctrl cost _ -> Right (a, cost, ctx)

    m >>= f = Svcode $ \ ctx ctrl cost fe -> 
        case rSvcode m ctx ctrl cost fe of 
            Right (a, cost', ctx') -> case rSvcode (f a) ctx' ctrl cost' fe of 
                               Right (b, cost'', c'') -> Right (b, cost'',c'')
                               Left err' -> Left err'      
            Left err -> Left err

    fail err = Svcode $ \ _ _ _ _ -> Left $ "SVCODE runtime error: " ++ err 

instance Functor Svcode where
  fmap f t = t >>= return . f

instance Applicative Svcode where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



-- run the svcode translated from an SNESL expression
runSvcodeExp :: SFun -> FEnv -> Either String (SvVal, (Int,Int))
runSvcodeExp (SFun [] st code _) fe = 
  case rSvcode (mapM_ sInstrInterp code) [] 0 (0,0) fe of 
    Right (_, (w,s), ctx) -> 
        case lookupTreeCtx st ctx of                      
            Nothing -> Left "SVCODE runtime error: undefined streams." 
            Just vs -> Right (vs, (w,s))
    Left err -> Left err  



lookupTreeCtx :: STree -> Svctx -> Maybe SvVal
lookupTreeCtx (IStr t1) ctx = lookup t1 ctx
lookupTreeCtx (BStr t1) ctx = lookup t1 ctx  
lookupTreeCtx (PStr t1 t2) ctx = case lookupTreeCtx t1 ctx of 
    Just v1 -> case lookupTreeCtx t2 ctx of 
                   Just v2 -> Just $ SPVal v1 v2  
                   Nothing -> Nothing
    Nothing -> Nothing

lookupTreeCtx (SStr t1 t2) ctx = case lookupTreeCtx t1 ctx of 
    Just v1 -> case lookupTreeCtx (BStr t2) ctx of 
                   Just (SBVal v2) -> Just $ SSVal v1 v2  
                   Nothing -> Nothing
    Nothing -> Nothing


-- look up the stream value according to its SId 
lookupSid :: SId -> Svcode SvVal 
lookupSid s = Svcode $ \c ctrl cost _ -> 
    case lookup s c of 
        Nothing -> Left $ "referring to an undefined stream: " ++ show s  
        Just v -> Right (v,cost,c)  


lookupFId :: FId -> Svcode SFun
lookupFId fid = Svcode $ \c ctrl cost fe -> 
    case lookup fid fe of 
        Nothing -> Left $ "undefined function: "  ++ show fid
        Just f -> Right (f,cost,c)  



addCtx :: SId -> SvVal -> Svcode ()
addCtx s v = Svcode $ \c ctrl cost _ -> Right ((), cost, c ++ [(s,v)])


streamLenM :: SvVal -> Svcode Int 
streamLenM s  = return $ streamLen s 

streamLen :: SvVal -> Int 
streamLen (SIVal s) = length s 
streamLen (SBVal s) = length s 
streamLen (SPVal s1 s2) = s1l + s2l 
    where s1l = streamLen s1 
          s2l = streamLen s2
streamLen (SSVal s1 s2) = s1l + s2l
    where s1l = streamLen s1 
          s2l = length s2

-- look up the function definition of the operation
lookupOP :: OP -> OpEnv -> Svcode ([SvVal] -> SvVal)  
lookupOP key ps = 
  do case lookup key ps of
       Just v -> return v 
       Nothing -> fail $ "SVCODE: can't find " ++ show key



-- explicitly add a cost
returnsvc :: (Int,Int) -> a -> Svcode a
returnsvc (w,s) a = Svcode $ \ c ctrl (w0,s0) _ -> Right (a, (w0+w,s0+s), c)


-- compute the cost of an instruction when return the interpretation result
returnInstrC :: [SvVal] -> SvVal -> Svcode SvVal
returnInstrC inVs outV  = 
    do ls <- mapM streamLenM inVs
       let inWork = sum ls  
           outWork = streamLen outV
       returnsvc (wrapWork (inWork + outWork), 1) outV  


getCtrl :: Svcode SId 
getCtrl = Svcode $ \ ctx ctrl cost _ -> Right (ctrl, cost, ctx)

localCtrl :: SId -> Svcode a -> Svcode a 
localCtrl ctrl m = Svcode $ \ ctx _ cost fe -> rSvcode m ctx ctrl cost fe


getCtx :: Svcode Svctx
getCtx = Svcode $ \ ctx ctrl cost _ -> Right (ctx, cost, ctx)


setCtx :: Svctx -> Svcode ()
setCtx c = Svcode $ \ _ ctrl cost _ -> Right ((), cost, c)



-- set empty streams for the SIds in the STree 
emptyStream :: STree -> Svcode ()
emptyStream (IStr s) = addCtx s $ SIVal [] 
emptyStream (BStr s) = addCtx s $ SBVal []
emptyStream (PStr st1 st2)  = emptyStream st1 >> emptyStream st2
emptyStream (SStr st1 st2) = emptyStream st1 >> (addCtx st2 $ SBVal [])
 

makeCtx :: [SId] -> [SId] -> Svcode Svctx
makeCtx s1s s2s =　
  do vs <- mapM lookupSid s1s 
     return $ zip s2s vs 
 

---- interpret a stream definition ---- 

sInstrInterp :: SInstr -> Svcode ()

-- provide two kinds of eager interpreting solutions:
sInstrInterp (SDef sid i) = 
  --sExpInterp i >>= addCtx sid   -- old eager interpreter 
  sExpInterpProc i >>= addCtx sid -- transducer interpreter using Proc 


sInstrInterp (WithCtrl c defs st) =
  do ctrl@(SBVal bs) <- lookupSid c 
     if null bs  
       then emptyStream st    
       else localCtrl c $ mapM_ sInstrInterp defs

sInstrInterp (SCall fid sids retSids) = 
  do (SFun sids' st code _) <- lookupFId fid 
     gloCtrl <- getCtrl
     oldCtx <- getCtx
     c <- makeCtx (gloCtrl:sids) (0:sids')
     setCtx c
     localCtrl 0 $ mapM_ sInstrInterp code
     retc <- makeCtx (tree2Sids st) retSids     
     setCtx $ oldCtx ++ retc



------ interpret SExps   --------

sExpInterp :: SExp -> Svcode SvVal

sExpInterp Ctrl = returnInstrC [] (SBVal [False]) 

sExpInterp EmptyCtrl = returnInstrC [] (SBVal [])     

-- MapConst: Map the const 'a' to the stream 'sid2'
sExpInterp (MapConst sid a) = 
    do v <- lookupSid sid
       l <- streamLenM v
       let as = case a of 
                 IVal i -> SIVal $ replicate l i 
                 BVal b -> SBVal $ replicate l b
       returnInstrC [v] as


sExpInterp (Const a) = 
    do ctrl <- getCtrl
       sExpInterp (MapConst ctrl a)
    

-- toflags: generate flag segments for a stream of integers
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
sExpInterp (ToFlags sid) = 
    do v'@(SIVal v) <- lookupSid sid
       returnInstrC [v'] $ SBVal $ concat $ map i2flags v  


-- count the number of 'False'
sExpInterp (Usum sid) = 
    do vs'@(SBVal vs) <- lookupSid sid
       returnInstrC [vs'] $ SBVal $ usum vs 

sExpInterp (MapOne op s1) = 
    do v1 <- lookupSid s1
       fop <- lookupOP op opEnv0
       returnInstrC [v1] $ fop [v1]
      
sExpInterp (MapTwo op s1 s2) = 
    do v1 <- lookupSid s1
       v2 <- lookupSid s2
       primLenChk v1 v2 "MapTwo"
       fop <- lookupOP op opEnv0
       returnInstrC [v1,v2] $ fop [v1,v2]


sExpInterp (Pack s1 s2) = 
    do v1 <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       primLenChk v1 v2' "Pack"
       let v1' = case v1 of               
                     (SIVal is) -> SIVal $ ppack is v2 
                     (SBVal bs) -> SBVal $ ppack bs v2
       returnInstrC [v1,v2'] v1'

sExpInterp (UPack s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       if (length [v | v <- v1, v] ) == (length v2)
         then returnInstrC [v1',v2'] $ SBVal $ upack v1 v2
         else fail "UPack: segments mismatch"


sExpInterp (Distr s1 s2) =
    do v1 <- lookupSid s1  
       l1 <- streamLenM v1
       v2'@(SBVal v2) <- lookupSid s2  
       if not $ l1 == (length [v | v <- v2, v])
         then fail $ "Distr: segments mismatch: " ++ show v1 ++ ", " ++ show v2'
         else let v1' = case v1 of
                         (SIVal is) -> SIVal $ pdist is v2 
                         (SBVal bs) -> SBVal $ pdist bs v2
              in returnInstrC [v1,v2'] v1'

sExpInterp (SegDistr s1 s2) =
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       segCountChk v1 v2 "SegDistr"
       returnInstrC [v1',v2'] $ SBVal $ segDistr v1 v2          


sExpInterp (SegFlagDistr s1 s2 s3) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     segCountChk v2 v3 "SegFlagDistr"
     segDescpChk v1 v2 "SegFlagDistr"
     returnInstrC [v1',v2',v3'] $ SBVal $ segFlagDistr v1 v2 v3 

sExpInterp (PrimSegFlagDistr s1 s2 s3) = 
  do v1 <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     segCountChk v2 v3 "PrimSegFlagDistr"
     --elemDescpChk v1 v2 "SegFlagDistr" 
     case v1 of 
        (SIVal is) -> returnInstrC [v1,v2',v3'] $ SIVal $ primSegFlagDistr is v2 v3 
        (SBVal bs) -> returnInstrC [v1,v2',v3'] $ SBVal $ primSegFlagDistr bs v2 v3 


sExpInterp (B2u sid) = 
    do v'@(SBVal v) <- lookupSid sid 
       returnInstrC [v'] $ SBVal $ b2u v


sExpInterp (SegscanPlus s1 s2) = 
    do v1'@(SIVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 

       if not $ (sum $ flags2len v2) == (length v1)
         then fail "SegscanPlus: segments mismatch"
         else returnInstrC [v1',v2'] $ SIVal $ segExScanPlus v1 v2 


sExpInterp (ReducePlus s1 s2) =
    do v1'@(SIVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       let ls = flags2len v2          
       returnInstrC [v1',v2'] $ SIVal $ segSum ls v1 


-- remove all the 'T' flags except for the last one
sExpInterp (SegConcat s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       returnInstrC [v1',v2'] $ SBVal $ segConcat v1 v2 


sExpInterp (USegCount s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       elemDescpChk v1 v2 "USegCount"
       returnInstrC [v1',v2'] $ SBVal $ uSegCount v1 v2 


sExpInterp (InterMergeS ss) = 
  do vs <- mapM lookupSid ss  -- segCountChk v1 v2 "InterMerge"
     let vs' = map (\(SBVal v) -> v) vs 
     returnInstrC vs $ SBVal $ interMergeS vs' 


sExpInterp (SegInterS ss) = 
  do vs <- mapM (\(s1,s2) -> 
                    do v1'@(SBVal v1) <- lookupSid s1
                       v2'@(SBVal v2) <- lookupSid s2 
                       return ((v1,v2),[v1',v2'])) 
                ss  
     let (vs1,vs2) = unzip vs    
     --segCountChk v2 v4 "SegInter" 
     mapM_ (\(v1,v2) -> segDescpChk v1 v2 "SegInter") vs1
     returnInstrC (concat vs2) $ SBVal $ segInterS vs1


sExpInterp (PriSegInterS ss) = 
  do vs <- mapM (\(s1,s2) -> 
                     do v1 <- lookupSid s1
                        v2'@(SBVal v2) <- lookupSid s2
                        return ((v1,v2),[v1,v2'])) 
                ss
     let (vs1,vs2) = unzip vs  
     returnInstrC (concat vs2) $ priSegInterS vs1 


sExpInterp (SegMerge s1 s2) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     returnInstrC [v1',v2'] $ SBVal $ segMerge v1 v2 


sExpInterp (Check s1 s2) = 
   do v1 <- lookupSid s1
      v2 <- lookupSid s2
      if v1 == v2 then returnInstrC [v1,v2] $ SBVal []
        else fail $ "streams are not identical:" ++ show v1 ++ "," ++ show v2




----------- interpret SExps using Proc ----------------

sExpInterpProc :: SExp -> Svcode SvVal

sExpInterpProc Ctrl = returnInstrC [] (SBVal [False])

sExpInterpProc EmptyCtrl = returnInstrC [] (SBVal [])

sExpInterpProc (Const a) = 
  do c <- getCtrl
     sExpInterpProc (MapConst c a)


sExpInterpProc (MapConst sid a) = 
  do let v0 = case a of 
                IVal _ -> SIVal []
                BVal _ -> SBVal []
     svProc [sid] (mapConst a) v0 


sExpInterpProc (ToFlags sid) = svProc [sid] toFlags (SBVal []) 

sExpInterpProc (Usum sid) = svProc [sid] usumProc  (SBVal [])
 

sExpInterpProc (MapTwo op s1 s2) = 
  do (fop, tp) <- lookupOpA op opAEnv0
     let v0 = case tp of 
                TInt -> SIVal []
                TBool -> SBVal []
     svProc [s1,s2] (mapTwo fop) v0


sExpInterpProc (MapOne op s1) = 
  do (fop,tp) <- lookupOpA op opAEnv0
     let v0 = case tp of 
                TInt -> SIVal []
                TBool -> SBVal []
     svProc [s1] (mapOne fop) v0 


sExpInterpProc (Pack s1 s2) = svProc' [s2,s1] packProc 1


sExpInterpProc (UPack s1 s2) = svProc [s2,s1] upackProc (SBVal [])


sExpInterpProc (Distr s1 s2) = svProc' [s1,s2] pdistProc 0 
 

sExpInterpProc (SegDistr s1 s2) = svProc [s1,s2] segDistrProc (SBVal [])


sExpInterpProc (SegFlagDistr s1 s2 s3) = 
  svProc [s2,s1,s3] segFlagDistrProc (SBVal []) 
 

sExpInterpProc (PrimSegFlagDistr s1 s2 s3) = 
  svProc' [s2,s1,s3] primSegFlagDistrProc 1


sExpInterpProc (B2u sid) = svProc [sid] b2uProc (SBVal [])


sExpInterpProc (SegscanPlus s1 s2) = svProc [s2,s1] segScanPlusProc (SIVal [])
 

sExpInterpProc (ReducePlus s1 s2) = svProc [s2,s1] segReducePlusProc (SIVal [])
  

sExpInterpProc (SegConcat s1 s2) = svProc [s2,s1] segConcatProc (SBVal [])
 

sExpInterpProc (USegCount s1 s2) = svProc [s2,s1] uSegCountProc (SBVal [])
 

sExpInterpProc (InterMergeS ss) = 
  svProc ss (interMergeProc $ length ss) (SBVal [])


sExpInterpProc (SegInterS ss) = 
  do vs <- mapM (\(s1,s2) -> 
                    do v1 <- lookupSid s1
                       v2 <- lookupSid s2 
                       return [v1,v2])
                ss
     let vss = concat vs
         cs = [(i*2,i*2+1) | i <- [0..length vss `div` 2 -1]] -- channel numbers
     returnInstrC vss $ evalProc (segInterProc cs) vss (SBVal [])


sExpInterpProc (PriSegInterS ss) = 
  do vs <- mapM (\(s1,s2) -> 
                     do v1 <- lookupSid s1
                        v2 <- lookupSid s2
                        return [v1,v2]) 
                ss
     let vss = concat vs 
         cs = [(i*2,i*2+1) | i <- [0..length vss `div` 2 -1]]
         v0 = sv0 (head vss)
     returnInstrC vss $ evalProc (priSegInterProc cs) vss v0


sExpInterpProc (SegMerge s1 s2) = svProc [s2,s1] segMergeProc (SBVal [])
 

sExpInterpProc (Check s1 s2) = sExpInterp (Check s1 s2) -- same as sExpInterp



lookupOpA :: OP -> OpAEnv -> Svcode ([AVal] -> AVal, Type) 
lookupOpA op r = 
  do case lookup op r of
       Just v -> return v 
       Nothing -> fail $ "SVCODE: can't find " ++ show op 


sv0 :: SvVal -> SvVal
sv0 v = case v of 
    SIVal _ -> SIVal []
    SBVal _ -> SBVal []


svProc :: [SId] -> Proc () -> SvVal -> Svcode SvVal 
svProc sids proc v0 = 
  do vs <- mapM lookupSid sids      
     returnInstrC vs $ evalProc proc vs v0


svProc' :: [SId] -> Proc () -> Int -> Svcode SvVal 
svProc' sids proc i = 
  do vs <- mapM lookupSid sids      
     returnInstrC vs $ evalProc proc vs $ sv0 $ vs !! i 

--------------------------------------------------




-- segment interleave
segInter :: [Bool] -> [Bool] -> [Bool] -> [Bool] -> [Bool]
segInter b1 b2 b3 b4 = concat $ interleaveList segs b1' b3'
    where b1' = partFlags b1
          b3' = partFlags b3 
          segs = zip b2' b4'
          b2' = flags2len b2
          b4' = flags2len b4

-- a general version of segInter
segInterS :: [([Bool],[Bool])] -> [Bool]
segInterS [(b1,b2)] = b1 
segInterS bs = fst bs'
    where bs' = foldl (\(x1,y1) (x2,y2) -> 
                          ((segInter x1 y1 x2 y2),(interMerge y1 y2))) b0 bs 
          b0 = ([], replicate n True)
          n = length.flags2len.snd $ head bs
 

-- a general version of priSegInter
priSegInterS :: [(SvVal,[Bool])] -> SvVal
priSegInterS [(s,b)] = s  

priSegInterS vs@((SIVal _, _):vs') = SIVal $ concat segs 
    where (ss,bs) = unzip vs
          ss' = [v | (SIVal v) <- ss]
          lens = map flags2len bs  
          segs = map concat $ transpose $ zipWith seglist lens ss' 

priSegInterS vs@((SBVal _,_):vs') = SBVal $ concat segs 
    where (ss,bs) = unzip vs
          ss' = [v | (SBVal v) <- ss]
          lens = map flags2len bs  
          segs = map concat $ transpose $ zipWith seglist lens ss' 



interleaveList :: [(Int, Int)] -> [a] -> [a] -> [a]
interleaveList [] _ _ = []
interleaveList ((l1,l2):ps) vs1 vs2 = 
  (take l1 vs1) ++ (take l2 vs2) ++ (interleaveList ps (drop l1 vs1) (drop l2 vs2))


-- partition flags by 'T's
-- [FFT FT T FFT] => [[FFT], [FT], [T], [FFT]]
partFlags :: [Bool] -> [[Bool]]
partFlags bs = seglist (map (+1) $ flags2len bs) bs  

-- partition flags by another descriptor flag
-- [FFT FT T FFT] , [FT FFFT] => [[FFT], [FT,T,FFT]]
flagPartFlags :: [Bool] -> [Bool] -> [[Bool]]
flagPartFlags bs fs = map concat $ seglist ls (partFlags bs)
    where ls = flags2len fs 

-- [1,5,2,4] , [FT FFFT] => [[1], [5,2,4]]
flagPartPrim :: [a] -> [Bool] -> [[a]]
flagPartPrim vs fs = seglist ls vs
    where ls = flags2len fs 


-- segment merge
-- e.g [FFTFT FT] , [FTFFT] => [FFT FFT]
segMerge :: [Bool] -> [Bool] -> [Bool]
segMerge b1 b2 = concat $ takeSeg ls b1' 
    where ls = flags2len b2
          b1' = map init $ partFlags b1

takeSeg :: [Int] -> [[Bool]] -> [[Bool]]
takeSeg [] _ = []
takeSeg (i:is) bs = take i bs ++ [[True]] ++ (takeSeg is (drop i bs))




-- interleave merge
-- [F,T,F,T] ++ [F,T,F,F,T]  => [F,F,T,F,F,F,T]
interMerge :: [Bool] -> [Bool] -> [Bool]
interMerge [] f2 = f2 
interMerge (False:fs1) f2 = False : interMerge fs1 f2
interMerge f1@(True:fs1) (False:fs2) = False : interMerge f1 fs2
interMerge (True:fs1) (True:fs2) = True : interMerge fs1 fs2

-- a general version of interMerge
interMergeS :: [[Bool]] -> [Bool]
interMergeS bs = foldl interMerge b0 bs
    where b0 = replicate n True
          n = length $ flags2len $ head bs

 --Another implementation of interMergeS without using interMerge
--interMergeS bs = concat $ map ((++[True]).(filter (==False))) bs'
--  where bs' = map concat $ transpose $  map partFlags bs 


-- 
segConcat :: [Bool] -> [Bool] -> [Bool]
segConcat [] [] = []
segConcat (False:fs1) f2@(False:fs2) = False: segConcat fs1 f2
segConcat (True:fs1) (False:fs2) = segConcat fs1 fs2
segConcat f1 (True:fs2) = True : segConcat f1 fs2

-- count unary segments
-- [F,T,F,F,T, F,F,T]  [FFFFFT,FFFT](flags) -> [F,F,T, F,T]
uSegCount :: [Bool] -> [Bool] -> [Bool]
uSegCount [] [] = []
uSegCount (False:fs1) (False:fs2) = uSegCount fs1 fs2
uSegCount (True:fs1) (False:fs2) = False : uSegCount fs1 fs2
uSegCount f1 (True:fs2) = True : uSegCount f1 fs2



-- [1,2,3,4,5] [F,T,F,F,T] = [2,5]
ppack :: [a] -> [Bool] -> [a]
ppack [] [] = []
ppack (a:as) (False:fs) = ppack as fs
ppack (a:as) (True:fs) = a: ppack as fs

-- pack unary numbers(i.e., subsequences of the form <F,F,..T>)
upack :: [Bool] -> [Bool] -> [Bool] 
upack b1 b2 = concat $ fst $ unzip $ filter (\(s,f) -> f) (zip segs b2) 
    where segs = partFlags b1 


-- unary sum of the number of Fs 
-- e.g. <F,F,T,F,T> => <F,F,F> representing <*,*,*> 
usum :: [Bool] -> [Bool]
usum [] = []
usum (True:s) = usum s
usum (False:s) = False : usum s     


-- <F> -> <T>
-- <T> -> <F,T>
b2u :: [Bool] -> [Bool]
b2u [] = []
b2u (False:fs) = True : b2u fs 
b2u (True: fs) = False: True: b2u fs



-- the number of 'True' must be equal to the length of the first list
-- <1,2,3> <F,F,T,T,F,T> => <1,1,3>
pdist :: [a] -> [Bool] -> [a]
pdist [] [] = []
pdist v@(a:s) (False:fs) = a : pdist v fs 
pdist (a:s) (True:fs) = pdist s fs 


---- [FT,FFT] ->[FFFT,FT] => [FTFTFT FFT]
segDistr :: [Bool] -> [Bool] -> [Bool]
segDistr f1 f2 = concat $ zipWith segReplicate f1s ls2
   where f1s = partFlags f1 
         ls2 = flags2len f2

--【FFT FT】 [FT FT] [FFT FFT] => [FFT FFT FT FT] 
segFlagDistr :: [Bool] -> [Bool] -> [Bool] -> [Bool]
segFlagDistr v1 v2 v3  = concat $ zipWith segReplicate segs ls3 
    where segs = flagPartFlags v1 v2 
          ls3 = flags2len v3
          
-- [1,2,3] [FT FFT T] [FFT FFT FT] => [11 23 23]
primSegFlagDistr :: [a] -> [Bool] -> [Bool] -> [a]
primSegFlagDistr v1 v2 v3  = concat $ zipWith segReplicate segs ls3 
    where segs = flagPartPrim v1 v2 
          ls3 = flags2len v3




-- replicate [a] 'Int' times
segReplicate :: [a] -> Int -> [a]
segReplicate [] _ = []
segReplicate bs 1 = bs
segReplicate bs i = if i >1 then bs ++ segReplicate bs (i-1) else []



-- segment exclusive scan for plus; segment delimiter is 0
segExScanPlus :: [Int] -> [Bool] -> [Int]
segExScanPlus is bs = concat $ map (init.(scanl (+) 0)) segs  
    where ls = flags2len bs
          segs = seglist ls is 



segSum :: [Int] -> [Int] -> [Int]
segSum [] [] = []
segSum (n:ns) l = (sum $ take n l) : segSum ns (drop n l)



-- helper functions for runtime error check :

-- check if two primitive streams have the same length
primLenChk :: SvVal -> SvVal -> String -> Svcode ()
primLenChk s1 s2 instrName =
  do l1 <- streamLenM s1
     l2 <- streamLenM s2 
     if l1 == l2
     then return () 
     else fail $ instrName ++ ": stream lengths mismatch: "
                   ++ show s1 ++ ", " ++ show s2 

-- check if two bool lists have the same number of segments (i.e. number of 'T's)
segCountChk :: [Bool] -> [Bool] -> String -> Svcode ()
segCountChk b1 b2 instrName = 
    do let segCount1 = length [b | b <- b1, b]
           segCount2 = length [b | b <- b2, b]
       if segCount1 == segCount2 
       then return ()
       else fail $ instrName ++ ": segment numbers mismatch: " 
                      ++ show b1 ++ ", " ++ show b2 

-- the number of 'F's in b2 is equal to the number of 'T's in b1
segDescpChk :: [Bool] -> [Bool] -> String -> Svcode ()
segDescpChk b1 b2 instrName = 
    do let segCount1 = length [b | b <- b1, b]
           l2 = length [b | b <- b2, not b]
       if segCount1 == l2
       then return ()
       else fail $ instrName ++ ": segment descriptor mismatch: "
                    ++ show b1 ++ ", " ++ show b2  

-- the number of 'T's in b2 is equal to the number of elements in b1
--elemSegChk :: [a] -> [Bool] -> String -> Svcode ()
--elemSegChk as bs instrName = 
--    do let al = length as 
--           segs = length [b | b <- bs, b]
--       if al == segs 
--       then return ()
--       else fail $ instrName ++ ": segment mismatch: " 
--                      ++ show as ++ ", " ++ show bs


-- the number of 'F's in bs is equal to the length of as
-- and the last element of bs must be a 'T'
elemDescpChk :: Show a => [a] -> [Bool] -> String -> Svcode ()
elemDescpChk as bs instrName = 
    do let fs = [b | b <- bs , not b]
       if (length as == length fs) && (last bs) 
        then return ()
        else fail $ instrName ++ ": segment descriptor mismatch "
                     ++ show as  ++ ", " ++ show bs 

