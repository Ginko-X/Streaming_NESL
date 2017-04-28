{- Svcode Interpreter -}

module SvcodeInterp where

import SvcodeSyntax
import SneslSyntax
import Control.Monad
import DataTrans (i2flags)
import SneslInterp (flags2len, seglist)


type Svctx = [(SId, SvVal)]

newtype Svcode a = Svcode {rSvcode :: Svctx -> Either String (a,(Int,Int), Svctx)}

instance Monad Svcode where
    return a = Svcode $ \ c -> Right (a, (0,0), c)

    m >>= f = Svcode $ \ c -> 
        case rSvcode m c of 
            Right (a, (w,s), c') -> case rSvcode (f a) c' of 
                               Right (b, (w',s'), c'') -> Right (b, (w+w', s+s'),c'')
                               Left err' -> Left err'      
            Left err -> Left err

instance Functor Svcode where
  fmap f t = t >>= return . f

instance Applicative Svcode where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


runSvcodeProg :: SSym -> Either String (SvVal, (Int,Int))
runSvcodeProg (SSym sdefs st) = 
  case rSvcode (mapM sdefInterp sdefs) [] of 
    Right (_, (w,s), ctx) -> 
        case lookupTree st ctx of                      
            Nothing -> Left "Stream does not exist." 
            Just vs -> Right (vs, (w,s))
    Left err -> Left err 


lookupTree :: STree -> Svctx -> Maybe SvVal
lookupTree (STId t1) ctx = lookup t1 ctx  
lookupTree (STPair t1 t2) ctx = case lookupTree t1 ctx of 
    Just v1 -> case lookupTree t2 ctx of 
                   Just v2 -> Just $ SPVal v1 v2  -- need a 'Type' to indicate 
                                                  -- a SSVal or a SPVal
                   Nothing -> Nothing
    Nothing -> Nothing

-- only for debug, to show all the SIds and their streams
--runSvcodeProg' :: SSym -> Either String SvVal
--runSvcodeProg' (SSym sdefs st) = 
--  case rSvcode (mapM_ sdefInterp sdefs) [] of 
--    Right (_, _,ctx) -> 
--        case lookupSpeTree [10..20] ctx of                      
--            Nothing -> Left "Stream does not exist." 
--            Just vs -> Right vs
--    Left err -> Left err 

--lookupSpeTree :: [SId] -> Svctx -> Maybe SvVal
--lookupSpeTree [] ctx = Just (SIVal [])
--lookupSpeTree [t1] ctx = lookup t1 ctx  
--lookupSpeTree (t1:ts) ctx = case lookupSpeTree [t1] ctx of 
--    Just v1 -> case lookupSpeTree ts ctx of 
--                   Just v2 -> Just $ SPVal v1 v2  -- need a 'Type' to indicate 
--                                                  -- a SSVal or a SPVal
--                   Nothing -> Nothing
--    Nothing -> Nothing


-- look up the stream defined by the SId 
lookupSid :: SId -> Svcode SvVal 
lookupSid s = Svcode $ \c -> 
    case lookup s c of 
        Nothing -> Left $ "Referring to a stream that does not exist: " 
                             ++ show s
        Just v -> Right (v,(0,0),c)  


addCtx :: SId -> SvVal -> Svcode SvVal
addCtx s v = Svcode $ \c -> Right (v, (0,0), c ++ [(s,v)])


streamLenM :: SvVal -> Svcode Int 
streamLenM (SIVal s) = return $ length s 
streamLenM (SBVal s) = return $ length s 


streamLen :: SvVal -> Int 
streamLen (SIVal s) = length s 
streamLen (SBVal s) = length s 


-- look up the function definition of the operation
lookupOP :: OP -> OpEnv -> Svcode ([SvVal] -> SvVal)  
lookupOP key ps = 
  do case lookup key ps of
       Just v -> return v 
       Nothing -> fail $ "SVCODE: can't find " ++ show key



sdefInterp :: SDef -> Svcode SvVal
sdefInterp (SDef sid i) = 
    do v <- instrInterp i
       addCtx sid v 


-- explicitly add general cost in return
returnsvc :: (Int,Int) -> a -> Svcode a
returnsvc (w,s) a = Svcode $ \ c -> Right (a, (w,s), c)


-- compute the cost for an instruction when return the interpretation result
returnInstrC :: [SvVal] -> SvVal -> Svcode SvVal
returnInstrC inVs outV  = 
    do ls <- mapM streamLenM inVs
       let inWork = sum ls  
           outWork = streamLen outV
       returnsvc (inWork + outWork, 1) outV  -- for each instr, step is 1


 ---- Instruction interpretation  ------ 

instrInterp :: Instr -> Svcode SvVal

instrInterp Ctrl = returnsvc (1,1) (SBVal [False])  -- (0,1) ? 

-- MapConst: Map the const 'a' to the stream 'sid2'
instrInterp (MapConst sid a) = 
    do v <- lookupSid sid
       l <- streamLenM v
       let as = case a of 
                 IVal i -> SIVal $ replicate l i 
                 BVal b -> SBVal $ replicate l b
       returnInstrC [v] as

-- toflags: generate flag segments for a stream of integers
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
instrInterp (ToFlags sid) = 
    do v'@(SIVal v) <- lookupSid sid
       returnInstrC [v'] $ SBVal $ concat $ map i2flags v  


-- count the number of 'False'
instrInterp (Usum sid) = 
    do vs'@(SBVal vs) <- lookupSid sid
       returnInstrC [vs'] $ SBVal $ usum vs 

instrInterp (MapOne op s1) = 
    do v1 <- lookupSid s1
       fop <- lookupOP op opEnv0
       returnInstrC [v1] $ fop [v1]
      
instrInterp (MapTwo op s1 s2) = 
    do v1 <- lookupSid s1
       v2 <- lookupSid s2
       primLenChk v1 v2 "MapTwo"
       fop <- lookupOP op opEnv0
       returnInstrC [v1,v2] $ fop [v1,v2]


instrInterp (Pack s1 s2) = 
    do v1 <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       primLenChk v1 v2' "Pack"
       let v1' = case v1 of               
                     (SIVal is) -> SIVal $ ppack is v2 
                     (SBVal bs) -> SBVal $ ppack bs v2
       returnInstrC [v1,v2'] v1'

instrInterp (UPack s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       if (length [v | v <- v1, v] ) == (length v2)
         then returnInstrC [v1',v2'] $ SBVal $ upack v1 v2
         else fail "UPack: segments mismatch"


instrInterp (Distr s1 s2) =
    do v1 <- lookupSid s1  
       l1 <- streamLenM v1
       v2'@(SBVal v2) <- lookupSid s2  
       if not $ l1 == (length [v | v <- v2, v])
         then fail $ "Distr: segments mismatch: " ++ show v1 ++ ", " ++ show v2
         else let v1' = case v1 of
                         (SIVal is) -> SIVal $ pdist is v2 
                         (SBVal bs) -> SBVal $ pdist bs v2
              in returnInstrC [v1,v2'] v1'

instrInterp (SegDistr s1 s2) =
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       segCountChk v1 v2 "SegDistr"
       returnInstrC [v1',v2'] $ SBVal $ segDistr v1 v2          


instrInterp (SegFlagDistr s1 s2 s3) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     segCountChk v2 v3 "SegFlagDistr"
     segDescpChk v1 v2 "SegFlagDistr"
     returnInstrC [v1',v2',v3'] $ SBVal $ segFlagDistr v1 v2 v3 

instrInterp (PrimSegFlagDistr s1 s2 s3) = 
  do v1 <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     segCountChk v2 v3 "PrimSegFlagDistr"
     --elemDescpChk v1 v2 "SegFlagDistr" 
     case v1 of 
        (SIVal is) -> returnInstrC [v1,v2',v3'] $ SIVal $ primSegFlagDistr is v2 v3 
        (SBVal bs) -> returnInstrC [v1,v2',v3'] $ SBVal $ primSegFlagDistr bs v2 v3 


instrInterp (B2u sid) = 
    do v'@(SBVal v) <- lookupSid sid 
       returnInstrC [v'] $ SBVal $ b2u v


instrInterp (SegscanPlus s1 s2) = 
    do v1'@(SIVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       
       if not $ (sum $ flags2len v2) == (length v1)
         then fail "SegscanPlus: segments mismatch"
         else returnInstrC [v1',v2'] $ SIVal $ segExScanPlus v1 v2 


instrInterp (ReducePlus s1 s2) =
    do v1'@(SIVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2 
       let ls = flags2len v2          
       returnInstrC [v1',v2'] $ SIVal $ segSum ls v1 


-- remove all the 'T' flags except for the last one
instrInterp (SegConcat s1 s2) = 
    do v1'@(SBVal v1) <- lookupSid s1
       v2'@(SBVal v2) <- lookupSid s2
       returnInstrC [v1',v2'] $ SBVal $ segConcat v1 v2 


instrInterp (InterMerge s1 s2) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     segCountChk v1 v2 "InterMerge"
     returnInstrC [v1',v2'] $ SBVal $ interMerge v1 v2 

instrInterp (SegInter s1 s2 s3 s4) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3'@(SBVal v3) <- lookupSid s3
     v4'@(SBVal v4) <- lookupSid s4 
     segCountChk v2 v4 "SegInter" 
     segDescpChk v1 v2 "SegInter"
     segDescpChk v3 v4 "SegInter"
     returnInstrC [v1',v2',v3',v4'] $ SBVal $ segInter v1 v2 v3 v4  

instrInterp (PriSegInter s1 s2 s3 s4) = 
  do v1 <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     v3 <- lookupSid s3
     v4'@(SBVal v4) <- lookupSid s4
     returnInstrC [v1,v2',v3,v4'] $ priSegInter v1 v2 v3 v4  

instrInterp (SegMerge s1 s2) = 
  do v1'@(SBVal v1) <- lookupSid s1
     v2'@(SBVal v2) <- lookupSid s2
     returnInstrC [v1',v2'] $ SBVal $ segMerge v1 v2 


instrInterp (Empty tp) = 
  case tp of 
    TInt -> returnInstrC [] $ SIVal [] 
    TBool -> returnInstrC [] $ SBVal []



-- segment interleave
segInter :: [Bool] -> [Bool] -> [Bool] -> [Bool] -> [Bool]
segInter b1 b2 b3 b4 = concat $ interleaveList segs b1' b3'
    where b1' = partFlags b1
          b3' = partFlags b3 
          segs = zip b2' b4'
          b2' = flags2len b2
          b4' = flags2len b4


priSegInter :: SvVal -> [Bool] -> SvVal -> [Bool] -> SvVal 
priSegInter (SIVal v1) b1 (SIVal v2) b2 = SIVal $ interleaveList segs v1 v2
    where b1' = flags2len b1
          b2' = flags2len b2 
          segs = zip b1' b2'

priSegInter (SBVal v1) b1 (SBVal v2) b2 = SBVal $ interleaveList segs v1 v2
    where b1' = flags2len b1
          b2' = flags2len b2 
          segs = zip b1' b2'


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



appendPrimStream :: SvVal -> SvVal -> SvVal
appendPrimStream (SIVal v1) (SIVal v2) = SIVal (v1 ++ v2)
appendPrimStream (SBVal v1) (SBVal v2) = SBVal (v1 ++ v2)


-- interleave merge
-- [F,T,F,T] ++ [F,T,F,F,T]  => [F,F,T,F,F,F,T]
interMerge :: [Bool] -> [Bool] -> [Bool]
interMerge [] f2 = f2 
interMerge (False:fs1) f2 = False : interMerge fs1 f2
interMerge f1@(True:fs1) (False:fs2) = False : interMerge f1 fs2
interMerge (True:fs1) (True:fs2) = True : interMerge fs1 fs2


-- 
segConcat :: [Bool] -> [Bool] -> [Bool]
segConcat [] [] = []
segConcat (False:fs1) f2@(False:fs2) = False: segConcat fs1 f2
segConcat (True:fs1) (False:fs2) = segConcat fs1 fs2
segConcat f1 (True:fs2) = True : segConcat f1 fs2

segConcat (True:fs1) [] = True: segConcat fs1 [] -- special case for empty seqs

-- primitive pack
-- [1,2,3,4,5] [F,T,F,F,T] = [2,5]
ppack :: [a] -> [Bool] -> [a]
ppack [] [] = []
ppack (a:as) (False:fs) = ppack as fs
ppack (a:as) (True:fs) = a: ppack as fs

-- pack unary numbers(subsequences of the form <F,F,..T>)
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


segFlagDistr :: [Bool] -> [Bool] -> [Bool] -> [Bool]
segFlagDistr v1 v2 v3  = concat $ zipWith segReplicate segs ls3 
    where segs = flagPartFlags v1 v2 
          ls3 = flags2len v3

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
elemDescpChk :: [a] -> [Bool] -> String -> Svcode ()
elemDescpChk as bs instrName = undefined

