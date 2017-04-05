{- Svcode Interpreter -}

module SvcodeInterp where

import SvcodeSyntax
import SneslSyntax
import Control.Monad
import DataTrans (i2flags)
import SneslInterp (flags2len, seglist)


type Svctx = [(SId, SvVal)]

newtype Svcode a = Svcode {rSvcode :: Svctx -> Either String (a, Svctx)}

instance Monad Svcode where
    return a = Svcode $ \ c -> Right (a, c)

    m >>= f = Svcode $ \ c -> 
        case rSvcode m c of 
            Right (a, c') -> case rSvcode (f a) c' of 
                               Right (b, c'') -> Right (b, c'')
                               Left err' -> Left err'      
            Left err -> Left err

instance Functor Svcode where
  fmap f t = t >>= return . f

instance Applicative Svcode where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


runSvcodeProg :: SSym -> Either String SvVal
runSvcodeProg (SSym sdefs st) = 
  case rSvcode (mapM_ sdefInterp sdefs) [] of 
    Right (_, ctx) -> 
        case lookupTree st ctx of                      
            Nothing -> Left "Stream does not exist." 
            Just vs -> Right vs
    Left err -> Left err 


lookupTree :: STree -> Svctx -> Maybe SvVal
lookupTree (STId t1) ctx = lookup t1 ctx  
lookupTree (STPair t1 t2) ctx = case lookupTree t1 ctx of 
    Just v1 -> case lookupTree t2 ctx of 
                   Just v2 -> Just $ SPVal v1 v2  -- need a 'Type' to indicate 
                                                  -- a SSVal or a SPVal
                   Nothing -> Nothing
    Nothing -> Nothing


lookupSid :: SId -> Svcode SvVal 
lookupSid s = Svcode $ \c -> 
    case lookup s c of 
        Nothing -> Left $ "Referring to a stream that does not exist: " 
                             ++ show s
        Just v -> Right (v,c) 


streamLen :: SvVal -> Svcode Int 
streamLen (SIVal s) = return $ length s 
streamLen (SBVal s) = return $ length s 


addCtx :: SId -> SvVal -> Svcode SvVal
addCtx s v = Svcode $ \c -> Right (v, c ++ [(s,v)])



sdefInterp :: SDef -> Svcode SvVal
sdefInterp (SDef sid i) = 
    do v <- instrInterp i
       addCtx sid v 


instrInterp :: Instr -> Svcode SvVal
instrInterp Ctrl = return (SBVal [False])

-- MapConst: Map the const 'a' to the stream 'sid2'
instrInterp (MapConst sid a) = 
    do v <- lookupSid sid
       l <- streamLen v
       let as = case a of 
                 IVal i -> SIVal $ replicate l i 
                 BVal b -> SBVal $ replicate l b
       return as

-- toflags: generate flag segments for a stream of integers
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
instrInterp (ToFlags sid) = 
    do (SIVal v) <- lookupSid sid
       return $ SBVal $ concat $ map i2flags v  


-- count the number of 'False'
instrInterp (Usum sid) = 
    do (SBVal vs) <- lookupSid sid
       return $ SBVal $ usum vs 

instrInterp (MapAdd s1 s2) = 
    do (SIVal v1) <- lookupSid s1
       (SIVal v2) <- lookupSid s2
       if (length v1)  == (length v2) 
         then return $ SIVal $ zipWith (+) v1 v2
         else fail "MapAdd: lengths mismatch" 


instrInterp (MapEqual s1 s2) = 
   do (SIVal v1) <- lookupSid s1
      (SIVal v2) <- lookupSid s2
      if (length v1)  == (length v2) 
        then return $ SBVal $ zipWith (==) v1 v2
        else fail "MapEqual: lengths mismatch" 



instrInterp (Pack s1 s2) = 
    do v1 <- lookupSid s1
       l1 <- streamLen v1
       (SBVal v2) <- lookupSid s2
       if not $ l1 == (length v2)
         then fail "Pack: lengths mismatch"
         else let v1' = case v1 of               
                         (SIVal is) -> SIVal $ ppack is v2 
                         (SBVal bs) -> SBVal $ ppack bs v2
              in return v1'

instrInterp (UPack s1 s2) = 
    do (SBVal v1) <- lookupSid s1
       (SBVal v2) <- lookupSid s2 
       if (length [v | v <- v1, v] ) == (length v2)
         then return $ SBVal $ upack v1 v2
         else fail "UPack: segments mismatch"


instrInterp (Distr s1 s2) =
    do v1 <- lookupSid s1
       l1 <- streamLen v1
       (SBVal v2) <- lookupSid s2
       if not $ l1 == (length [v | v <- v2, v])
         then fail "Distr: segments mismatch"
         else let v1' = case v1 of
                         (SIVal is) -> SIVal $ pdist is v2 
                         (SBVal bs) -> SBVal $ pdist bs v2
              in return v1'

instrInterp (B2u sid) = 
    do (SBVal v) <- lookupSid sid 
       return $ SBVal $ b2u v


instrInterp (SegscanPlus s1 s2) = 
    do (SIVal v1) <- lookupSid s1
       (SBVal v2) <- lookupSid s2 
       if not $ (sum $ flags2len v2) == (length v1)
         then fail "SegscanPlus: segments mismatch"
         else return $ SIVal $ segExScanPlus v1 v2 


instrInterp (ReducePlus s1 s2) =
    do (SIVal v1) <- lookupSid s1 
       (SBVal v2) <- lookupSid s2
       let ls = flags2len v2          
       return $ SIVal $ segSum ls v1 


-- remove all the 'T' flags except for the last one
instrInterp (Concat s1 s2) = 
    do (SBVal v1) <- lookupSid s1
       (SBVal v2) <- lookupSid s2
       return $ SBVal $ concatflag v1 v2 

instrInterp (Append s1 s2) = 
  do v1 <- lookupSid s1
     v2 <- lookupSid s2 
     return $ appendStream v1 v2  


instrInterp (MapTimes s1 s2) = 
  do (SIVal v1) <- lookupSid s1
     (SIVal v2) <- lookupSid s2
     if (length v1)  == (length v2) 
        then return $ SIVal $ zipWith (*) v1 v2
        else fail "MapTimes: lengths mismatch" 

instrInterp (MapDiv s1 s2) = 
  do (SIVal v1) <- lookupSid s1
     (SIVal v2) <- lookupSid s2
     if (length v1)  == (length v2) 
        then return $ SIVal $ zipWith (div) v1 v2
        else fail "MapDiv: lengths mismatch" 


concatflag [] [] = []
concatflag [] (True:fs2) = True: concatflag [] fs2
concatflag (False:fs1) f2@(False:fs2) = False: concatflag fs1 f2
concatflag (True:fs1) (False:fs2) = concatflag fs1 fs2 
concatflag f1@(False:fs1) (True:fs2) = True : concatflag f1 fs2


-- primitive pack
-- [1,2,3,4,5] [F,T,F,F,T] = [2,5]
ppack :: [a] -> [Bool] -> [a]
ppack [] [] = []
ppack (a:as) (False:fs) = ppack as fs
ppack (a:as) (True:fs) = a: ppack as fs

-- pack unary numbers (subsequences of the form <F,F,..T>)
upack :: [Bool] -> [Bool] -> [Bool]
upack [] [] = []
upack (False:fs1) f2 = upack fs1 f2 
upack (True:fs1) (False:fs2) = upack fs1 fs2
upack (True:fs1) (True:fs2) = False:True : upack fs1 fs2 
 

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


-- segment exclusive scan for plus; segment delimiter is 0
segExScanPlus :: [Int] -> [Bool] -> [Int]
segExScanPlus is bs = concat $ map (init.(scanl (+) 0)) segs  
    where ls = flags2len bs
          segs = seglist ls is 



segSum :: [Int] -> [Int] -> [Int]
segSum [] [] = []
segSum (n:ns) l = (sum $ take n l) : segSum ns (drop n l)


appendStream (SIVal v1) (SIVal v2) = SIVal (v1 ++ v2)
appendStream (SBVal v1) (SBVal v2) = SBVal (v1 ++ v2)



-- "let x = 2 in {x+y : y in &10 }"
exampleProg = SSym defs (STPair (STId 9) (STId 3))
defs = [SDef 0 Ctrl,
        SDef 1 (MapConst 0 (IVal 2)),
        SDef 2 (MapConst 0 (IVal 10)),
        SDef 3 (ToFlags 2),
        SDef 4 (Usum 3),
        SDef 5 (MapConst 4 (IVal 1)),
        SDef 6 (SegscanPlus 5 3),
        SDef 7 (Usum 3),
        SDef 8 (Distr 1 3),
        SDef 9 (MapAdd 8 6)] 

