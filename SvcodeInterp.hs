{- Svcode Interpreter -}

module SvcodeInterp where

import SvcodeSyntax
import SneslSyntax
import SneslInterp (wrapWork)
import SvcodeXducer

import Data.List (transpose)


type Svctx = [(SId, SvVal)]

newtype Svcode a = Svcode {rSvcode :: Svctx -> SvVal -> (Int, Int) -> FEnv -> 
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
runSvcodeExp :: SFun -> FEnv -> Either String ([(SId, SvVal)], (Int,Int))
runSvcodeExp (SFun [] retSids code _) fe = 
  case rSvcode (mapM_ sInstrInterp (tail code) >> mapM lookupSid retSids) 
                 [] (SUVal [()]) (0,0) fe of 
    Right (svs, (w,s), ctx) -> return (zip retSids svs, (w,s)) 
    Left err -> Left err  


-------- for TDebug ----------
runSvcodeDebug :: SFun -> FEnv -> Int -> Either String ([SInstr],Svctx)
runSvcodeDebug (SFun [] _ code _) fe count = 
  case roundN [] (SUVal [()]) fe count code of 
    Right ctx -> Right (take count code, ctx)
    Left err -> Left err  
 
roundN ctx ctrl fe count code = 
  case rSvcode (sInstrInterp (head code) >> getCtrl) ctx ctrl (0,0) fe of 
    Right (ctrl',_,ctx') -> 
      if count > 0 && length code > 1 then roundN ctx' ctrl' fe (count-1) (tail code)
      else Right ctx' 
    Left err -> Left err
-------------------



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
streamLen (SUVal s) = length s 
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
returnInstrC inVs outV = 
    do ls <- mapM streamLenM inVs
       let inWork = sum ls  
           outWork = streamLen outV
       returnsvc (wrapWork (inWork + outWork), 1) outV  


-- for counting the cost of Xducer interpreter
returnXducerC :: [SvVal] -> SvVal -> Svcode SvVal
returnXducerC inVs outV = 
    do ls <- mapM streamLenM inVs
       let inWork = sum ls 
           outWork = streamLen outV
           work = inWork + outWork
       if work == 0 then return outV           
       else returnsvc (work,1) outV


getCtrl :: Svcode SvVal 
getCtrl = Svcode $ \ ctx ctrl cost _ -> Right (ctrl, cost, ctx)

localCtrl :: SvVal -> Svcode a -> Svcode a 
localCtrl ctrl m = Svcode $ \ ctx _ cost fe -> rSvcode m ctx ctrl cost fe


getCtx :: Svcode Svctx
getCtx = Svcode $ \ ctx ctrl cost _ -> Right (ctx, cost, ctx)

setCtx :: Svctx -> Svcode ()
setCtx c = Svcode $ \ _ ctrl cost _ -> Right ((), cost, c)


emptyStream :: [(PType,SId)] -> Svcode ()
emptyStream ps = mapM f ps >> return ()
  where f (PInt,s) = addCtx s $ SIVal []
        f (PBool,s) = addCtx s $ SBVal [] 
        f (PUnit,s) = addCtx s $ SUVal []


-- value copy
makeCtx :: [SId] -> [SId] -> Svcode Svctx
makeCtx s1s s2s =　
  do vs <- mapM lookupSid s1s 
     return $ zip s2s vs 
 

---- interpret a stream definition ---- 

sInstrInterp :: SInstr -> Svcode ()

-- provide two kinds of eager interpreting solutions:
sInstrInterp (SDef sid i) = 
  --sExpInterp i >>= addCtx sid   -- old eager interpreter 
  sExpInterpXducer i >>= addCtx sid -- transducer interpreter


sInstrInterp (WithCtrl c _ code retSids) =
  do ctrl@(SUVal bs) <- lookupSid c 
     if null bs  
       then emptyStream $ filter (\(_,s) -> s > c) retSids
       else localCtrl ctrl $ mapM_ sInstrInterp code

sInstrInterp (SCall fid sids retSids) = 
  do (SFun sids' frets code _) <- lookupFId fid 
     oldCtx <- getCtx
     c <- makeCtx sids sids'
     setCtx c
     mapM_ sInstrInterp code
     retc <- makeCtx frets retSids     
     setCtx $ oldCtx ++ retc



------ interpret SExps   --------

sExpInterp :: SExp -> Svcode SvVal

--sExpInterp Ctrl = returnInstrC [] (SUVal [()]) 

sExpInterp EmptyCtrl = returnInstrC [] (SUVal [])     

sExpInterp (Const a t) = 
    do ctrl <- getCtrl
       l <- streamLenM ctrl
       let as = case a of 
                 IVal i -> SIVal $ replicate l i 
                 BVal b -> SBVal $ replicate l b
       returnInstrC [ctrl] as
    

-- toflags: generate flag segments for a stream of integers
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
sExpInterp (ToFlags sid) = 
    do v'@(SIVal v) <- lookupSid sid
       returnInstrC [v'] $ SBVal $ concat $ map i2flags v  


-- count the number of 'False'
sExpInterp (Usum sid) = 
    do vs'@(SBVal vs) <- lookupSid sid
       returnInstrC [vs'] $ SUVal $ usum vs 

sExpInterp (MapOne op s1 t) = 
    do v1 <- lookupSid s1
       fop <- lookupOP op opEnv0
       returnInstrC [v1] $ fop [v1]
      
sExpInterp (MapTwo op s1 s2 t) = 
    do v1 <- lookupSid s1
       v2 <- lookupSid s2
       primLenChk v1 v2 "MapTwo"
       fop <- lookupOP op opEnv0
       returnInstrC [v1,v2] $ fop [v1,v2]


sExpInterp (Pack s1 s2 t) = 
    do v1 <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       primLenChk v1 v2' "Pack"
       let v1' = case v1 of               
                   SIVal is -> SIVal $ ppack is v2 
                   SBVal bs -> SBVal $ ppack bs v2
       returnInstrC [v1,v2'] v1'

sExpInterp (UPack s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       if (length [v | v <- v1, v] ) == (length v2)
         then returnInstrC [v1',v2'] $ SBVal $ upack v1 v2
         else fail "UPack: segments mismatch"


sExpInterp (Distr s1 s2 t) =
    do v1 <- lookupSid s1  
       l1 <- streamLenM v1
       v2'@(SBVal v2) <- lookupSid s2  
       if not $ l1 == (length [v | v <- v2, v])
         then fail $ "Distr: segments mismatch: " ++ show v1 ++ ", " ++ show v2'
         else let v1' = case v1 of
                         SIVal is -> SIVal $ pdist is v2 
                         SBVal bs -> SBVal $ pdist bs v2
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

sExpInterp (PrimSegFlagDistr s1 s2 s3 t) = 
  do v1 <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     segCountChk v2 v3 "PrimSegFlagDistr"
     --elemDescpChk v1 v2 "SegFlagDistr" 
     case v1 of 
       SIVal is -> returnInstrC [v1,v2',v3'] $ SIVal $ primSegFlagDistr is v2 v3 
       SBVal bs -> returnInstrC [v1,v2',v3'] $ SBVal $ primSegFlagDistr bs v2 v3 


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


sExpInterp (PriSegInterS ss _) = 
  do vs <- mapM (\(s1,s2) -> 
                     do v1 <- lookupSid s1
                        v2'@(SBVal v2) <- lookupSid s2
                        return ((v1,v2),[v1,v2'])) 
                ss
     let (vs1,vs2) = unzip vs  
     returnInstrC (concat vs2) $ priSegInterS vs1 


sExpInterp (Check s1 s2) = 
   do v1 <- lookupSid s1
      v2 <- lookupSid s2
      if v1 == v2 then returnInstrC [v1,v2] $ SBVal []
        else fail $ "streams are not identical:" ++ show v1 ++ "," ++ show v2

sExpInterp (IsEmpty s1) = 
   do v1'@(SBVal v1) <- lookupSid s1
      returnInstrC [v1'] $ SBVal $ isEmpty True v1 



----------- interpret SExps using Xducers ----------------

sExpInterpXducer :: SExp -> Svcode SvVal

sExpInterpXducer Ctrl = returnXducerC [] (SUVal [()])

sExpInterpXducer EmptyCtrl = returnXducerC [] (SUVal [])
sExpInterpXducer (Const a t) = svXducer [] (constXducerN a) t 
sExpInterpXducer (ToFlags sid) = svXducer [sid] toFlagsN PBool 
sExpInterpXducer (Usum sid) = svXducer [sid] usumXducerN PUnit 
 
sExpInterpXducer (MapTwo op s1 s2 t) = 
  do (fop, _) <- lookupOpA op opAEnv0
     svXducer [s1,s2] (mapTwoN fop) t

sExpInterpXducer (MapOne op s1 t) = 
  do (fop,_) <- lookupOpA op opAEnv0
     svXducer [s1] (mapOneN fop) t


sExpInterpXducer (Pack s1 s2 t) = svXducer [s2,s1] packXducerN t
sExpInterpXducer (UPack s1 s2) = svXducer [s2,s1] upackXducerN PBool
sExpInterpXducer (Distr s1 s2 t) = svXducer [s1,s2] pdistXducerN t
sExpInterpXducer (SegDistr s1 s2) = svXducer [s1,s2] segDistrXducerN PBool 

sExpInterpXducer (SegFlagDistr s1 s2 s3) =   -- 2.
  svXducer [s2,s1,s3] segFlagDistrXducerN PBool 
 

sExpInterpXducer (PrimSegFlagDistr s1 s2 s3 t) =  -- 3.
  svXducer [s2,s1,s3] primSegFlagDistrXducerN t


sExpInterpXducer (B2u sid) = svXducer [sid] b2uXducerN PBool
sExpInterpXducer (SegscanPlus s1 s2) = svXducer [s2,s1] segScanPlusXducerN PInt
sExpInterpXducer (ReducePlus s1 s2) = svXducer [s2,s1] segReducePlusXducerN PInt
sExpInterpXducer (SegConcat s1 s2) = svXducer [s2,s1] segConcatXducerN PBool
sExpInterpXducer (USegCount s1 s2) = svXducer [s2,s1] uSegCountXducerN PBool
 
-- 4.
sExpInterpXducer (InterMergeS ss) = 
  svXducer ss (interMergeXducerN $ length ss + 1) PBool  -- add a ctrl stream so length grows 1

--  !
sExpInterpXducer (SegInterS ss) = 
  do vs <- mapM (\(s1,s2) -> 
                    do v1 <- lookupSid s1
                       v2 <- lookupSid s2 
                       return [v1,v2])
                ss
     let vss = concat vs
         cs = [(i*2,i*2+1) | i <- [0..length vss `div` 2 -1]] -- channel numbers
         cs' = [(x+1,y+1) | (x,y) <- cs]  -- add a ctrl stream 
     vctrl <- getCtrl
     returnXducerC (vctrl:vss) $ evalXducer (segInterXducerN cs') (vctrl:vss) (SBVal [])

-- !
sExpInterpXducer (PriSegInterS ss t) = 
  do vs <- mapM (\(s1,s2) -> 
                     do v1 <- lookupSid s1
                        v2 <- lookupSid s2
                        return [v1,v2]) 
                ss  
     let vss = concat vs 
         cs = [(i*2,i*2+1) | i <- [0..length vss `div` 2 -1]]
         cs' = [(x+1,y+1) | (x,y) <- cs]  -- add a ctrl stream          
         v0 = sv0 t
     vctrl <- getCtrl
     returnXducerC (vctrl:vss) $ evalXducer (priSegInterXducerN cs') (vctrl:vss) v0

 
sExpInterpXducer (Check s1 s2) = svXducer [s1,s2] checkXducerN PBool  -- check type ??
sExpInterpXducer (IsEmpty s1) = svXducer [s1] isEmptyXducerN PBool


lookupOpA :: OP -> OpAEnv -> Svcode ([AVal] -> AVal, Type) 
lookupOpA op r = 
  do case lookup op r of
       Just v -> return v 
       Nothing -> fail $ "undefined operation:" ++ show op 


sv0 :: PType -> SvVal
sv0 t = case t of 
    PInt -> SIVal []
    PBool -> SBVal []
    PUnit -> SUVal []


svXducer :: [SId] -> Xducer () -> PType -> Svcode SvVal 
svXducer sids proc t = 
  do ctrl <- getCtrl
     vs <- mapM lookupSid sids
     let v0 = sv0 t  
     returnXducerC vs $ evalXducer proc (ctrl:vs) v0


--------------------------------------------------

isEmpty :: Bool -> [Bool] -> [Bool]
isEmpty _ [] = []
isEmpty True (True:bs) = True : isEmpty True bs
isEmpty True (False:bs) = False : isEmpty False bs 
isEmpty False (True:bs) = isEmpty True bs 
isEmpty False (False:bs) = isEmpty False bs 



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

-- Another implementation of interMergeS without using interMerge
-- interMergeS bs = concat $ map ((++[True]).(filter (==False))) bs'
--  where bs' = map concat $ transpose $  map partFlags bs 


-- e.g [FFTFT FT] , [FTFFT] => [FFT FFT]
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
usum :: [Bool] -> [()]
usum [] = []
usum (True:s) = usum s
usum (False:s) = () : usum s     


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

