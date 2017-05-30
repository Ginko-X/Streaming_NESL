{- Svcode Interpreter -}

module SvcodeSInterp where

import SvcodeSyntax
import SneslSyntax
import SneslCompiler (tree2Sids)
import DataTrans (i2flags)
import SneslInterp (flags2len, seglist)

import SvcodeInterp (usum, segSum,streamLen, ppack)

import Control.Monad
import Data.List (transpose)
import Data.Bits ((.&.))


type Eos = Bool 
type Buffer = SvVal 

data Acc = Acc Int | Nil deriving Show

type SState = ((Int,Int), Eos, Buffer,Acc)
type Svctx = [(SId, SState)]

newtype SvcodeS a = SvcodeS {rSvcodeS :: Svctx -> SId -> 
                                  Either String (a, Svctx)}

instance Monad SvcodeS where
    return a = SvcodeS $ \ ctx ctrl -> Right (a, ctx)

    m >>= f = SvcodeS $ \ ctx ctrl -> 
        case rSvcodeS m ctx ctrl of 
            Right (a, ctx') -> case rSvcodeS (f a) ctx' ctrl of 
                               Right (b, ctx'') -> Right (b, ctx'')
                               Left err' -> Left err'      
            Left err -> Left err

    fail err = SvcodeS $ \ _ _ -> Left $ "SVCODE runtime error: " ++ err 

instance Functor SvcodeS where
  fmap f t = t >>= return . f

instance Applicative SvcodeS where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


bufSize = 4

-- run the svcode translated from an SNESL expression
runSvcodeSExp :: SFun -> Either String (SvVal, (Int,Int))
runSvcodeSExp (SFun [] st code) = 
    do (bs, ctx) <- rSvcodeS (mapM sInstrInit code) [] 0 
       v <- lookupTreeCtx st ctx
       if all (\x -> x) bs then return (v,(0,0))  -- incorrect cost
       else 
         do (v', _) <- streamSvcodeS (mapM sInstrInterp code) ctx 0 st v
            return (v',(0,0))  


streamSvcodeS :: SvcodeS [Bool] -> Svctx -> SId -> STree -> SvVal 
                    -> Either String (SvVal, Svctx)
streamSvcodeS m ctx ctrl st v = 
    do (bs, ctx') <- rSvcodeS m ctx ctrl
       v' <- lookupTreeCtx st ctx' 
       if all (\x -> x) bs 
       then return (reduceSv $ concatSv v v', ctx')
       else streamSvcodeS m ctx' ctrl st (reduceSv $ concatSv v v')



lookupTreeCtx :: STree -> Svctx -> Either String SvVal
lookupTreeCtx (IStr t1) ctx = 
    case lookup t1 ctx of 
      Nothing -> Left "SVCODE runtime error: undefined streams." 
      Just (_,_,v,_) -> Right v 

lookupTreeCtx (BStr t1) ctx = 
    case lookup t1 ctx of 
      Nothing -> Left "SVCODE runtime error: undefined streams." 
      Just (_,_,v,_) -> Right v 

lookupTreeCtx (PStr t1 t2) ctx = 
    do v1 <- lookupTreeCtx t1 ctx 
       v2 <- lookupTreeCtx t2 ctx 
       return $ SPVal v1 v2  

lookupTreeCtx (SStr t1 t2) ctx = 
    do v1 <- lookupTreeCtx t1 ctx 
       (SBVal v2) <- lookupTreeCtx (BStr t2) ctx
       return $ SSVal v1 v2  


concatSv :: SvVal -> SvVal -> SvVal
concatSv (SIVal a) (SIVal b) = SIVal (a++b)
concatSv (SBVal a) (SBVal b) = SBVal (a++b)
concatSv (SPVal v1 v2) (SPVal v3 v4) = SPVal (concatSv v1 v3) (concatSv v2 v4)
concatSv (SSVal v1 b1) (SSVal v2 b2) = SSVal (concatSv v1 v2) (b1++b2)


reduceSv :: SvVal -> SvVal
reduceSv (SIVal is) = SIVal [sum is]
reduceSv v = v


-- look up the stream value according to its SId 
lookupSid :: SId -> SvcodeS SState 
lookupSid s = SvcodeS $ \c ctrl -> 
    case lookup s c of 
        Nothing -> Left $ "referring to an undefined stream: " ++ show s  
        Just v -> Right (v,c)  


addCtx :: SId -> SState -> SvcodeS ()
addCtx s v = SvcodeS $ \c _ -> Right ((), [(s,v)]++c)

updateCtx :: SId -> SState -> SvcodeS ()
updateCtx s state = SvcodeS $ \c _ -> Right ((), [(s,state)]++c) -- not real update



getCtrl :: SvcodeS SId 
getCtrl = SvcodeS $ \ ctx ctrl -> Right (ctrl, ctx)

localCtrl :: SId -> SvcodeS a -> SvcodeS a 
localCtrl ctrl m = SvcodeS $ \ ctx _  -> rSvcodeS m ctx ctrl


getCtx :: SvcodeS Svctx
getCtx = SvcodeS $ \ ctx _ -> Right (ctx, ctx)


setCtx :: Svctx -> SvcodeS ()
setCtx c = SvcodeS $ \ _ _ -> Right ((), c)



streamLenM :: SvVal -> SvcodeS Int 
streamLenM s  = return $ streamLen s 

lookupOP :: OP -> OpEnv -> SvcodeS ([SvVal] -> SvVal)  
lookupOP key ps = 
  do case lookup key ps of
       Just v -> return v 
       Nothing -> fail $ "SVCODE: can't find " ++ show key



----- Instructions initialization -------

sInstrInit :: SInstr -> SvcodeS Bool
sInstrInit (SDef sid i) = 
  do s@(_,eos,_,_) <- sExpInit i 
     addCtx sid s 
     return eos 

sInstrInit (WithCtrl c defs st) =
  do (_, _, ctrl@(SBVal bs),_) <- lookupSid c 
     if null bs  
       then emptyStream st >> return True   
       else 
         do bs <- localCtrl c $ mapM sInstrInit defs
            return $ all (\x -> x) bs


-- assume buffer size > 1
sExpInit :: SExp -> SvcodeS SState
sExpInit Ctrl = return ((1,1),True, SBVal [False], Nil)

sExpInit (Const a) = 
    do ctrl <- getCtrl
       (cur, eos,b,_) <- lookupSid ctrl
       l <- streamLenM b 
       let buf = case a of 
               IVal i -> SIVal $ replicate l i 
               BVal b -> SBVal $ replicate l b
       return (cur, eos, buf, Nil)


sExpInit (MapConst sid a) = 
    do (cur, eos, v, _) <- lookupSid sid
       l <- streamLenM v
       let as = case a of 
                 IVal i -> SIVal $ replicate l i 
                 BVal b -> SBVal $ replicate l b
       return (cur, eos, as, Nil)


sExpInit (ToFlags sid) = 
    do (_, eos, SIVal v,_) <- lookupSid sid
       let l = sum v + length v
       return (if l > bufSize 
          then ((l,bufSize),False,SBVal $ is2flags v bufSize 0,Acc bufSize)          
          else ((l,l), True, SBVal $ concat (map i2flags v), Nil))
            

sExpInit (Usum sid) = 
    do (cur, eos, (SBVal vs), _) <- lookupSid sid
       return $ (cur, eos, (SBVal $ usum vs), Nil) 

sExpInit (MapOne op s1) = 
    do (cur1, eos1, v1, _) <- lookupSid s1
       fop <- lookupOP op opEnv0
       return (cur1, eos1, fop [v1], Nil)
      
sExpInit (MapTwo op s1 s2) = 
    do (cur1, eos1, v1, _) <- lookupSid s1
       (_,_, v2,_) <- lookupSid s2
       fop <- lookupOP op opEnv0
       return (cur1, eos1,(fop [v1,v2]), Nil)


sExpInit (SegscanPlus s1 s2) = 
    do (_, _, (SIVal v1), _) <- lookupSid s1
       (cur2, eos2, (SBVal v2), _) <- lookupSid s2 
       let (vs, acc) = sSegExScanPlus 0 v1 v2            
       return (cur2, eos2, SIVal vs, if eos2 then Nil else Acc acc)


sExpInit (ReducePlus s1 s2) =
    do (_, _, (SIVal v1), _) <- lookupSid s1
       (cur2, eos2, (SBVal v2), _) <- lookupSid s2 
       let ls = sFlags2len v2
           v =  segSum ls v1
           acc = if last v2 then 0 else last v           
       return (cur2,eos2, SIVal v, if eos2 then Nil else Acc acc)


-- same problem as ToFlag
--sExpInit (B2u s1) = 
--    do (_, _, (SIVal v1), _) <- lookupSid s1


sExpInit (Pack s1 s2) = 
    do (cur1, eos1, v1, _) <- lookupSid s1
       (_,_, SBVal v2,_) <- lookupSid s2
       let v1' = case v1 of               
                     (SIVal is) -> SIVal $ ppack is v2 
                     (SBVal bs) -> SBVal $ ppack bs v2
       return (cur1, eos1, v1', Nil)



-- set empty streams for the SIds in the STree 
emptyStream :: STree -> SvcodeS ()
emptyStream (IStr s) = addCtx s ((0,0), True, SIVal [], Nil)
emptyStream (BStr s) = addCtx s ((0,0), True, SBVal [], Nil)
emptyStream (PStr st1 st2)  = emptyStream st1 >> emptyStream st2
emptyStream (SStr st1 st2) = emptyStream st1 >> addCtx st2 ((0,0), True, SBVal [], Nil)
 

------- Instructions streaming interpretation --------

sInstrInterp :: SInstr -> SvcodeS Bool
sInstrInterp (SDef sid i) = 
    do s@(_,eos,_,_) <- lookupSid sid
       if eos then return eos 
       else  
         do s'@(_,eos',_,_) <- sExpInterp sid i
            updateCtx sid s' 
            return eos' 

sInstrInterp i@(WithCtrl c defs st) = sInstrInit i 
 

---- SExp interpretation  ------ 

sExpInterp :: SId -> SExp -> SvcodeS SState

sExpInterp sid Ctrl = lookupSid sid  

----sExpInterp EmptyCtrl = returnInstrC [] (SBVal [])     


sExpInterp _ e@(MapConst _ _) = sExpInit e 

sExpInterp _ e@(Const _) = sExpInit e         

sExpInterp _ e@(Usum _) = sExpInit e 

sExpInterp _ e@(MapOne _ _) = sExpInit e 

sExpInterp _ e@(MapTwo _ _ _) = sExpInit e

sExpInterp _ e@(Pack _ _) = sExpInit e  


sExpInterp outs (ToFlags ins) = 
    do (_, _, SIVal v, _) <- lookupSid ins
       ((l,cur), _, _, Acc acc) <- lookupSid outs       
       return (if l - cur  > bufSize 
          then ((l,cur + bufSize),False,SBVal $ is2flags v bufSize acc, Acc $ acc + bufSize)
          else ((l,l), True, SBVal $ is2flags v bufSize acc, Nil))


sExpInterp outs (SegscanPlus ins1 ins2) = 
    do (_, _, _, Acc acc) <- lookupSid outs 
       (_, _, (SIVal v1), _) <- lookupSid ins1
       (cur2, eos2, (SBVal v2), _) <- lookupSid ins2
       let (vs, acc') = sSegExScanPlus acc v1 v2 
       return (cur2, eos2, SIVal vs, if eos2 then Nil else Acc acc')


sExpInterp outs (ReducePlus ins1 ins2) =
    do (_, _, _, Acc acc) <- lookupSid outs 
       (_, _, (SIVal v1), _) <- lookupSid ins1
       (cur2, eos2, (SBVal v2), _) <- lookupSid ins2
       let ls = sFlags2len v2
           v = segSum ls v1 
           v' = if null v then [acc] else (acc + head v) : tail v  
           acc' = if last v2 then 0 else last v'           
       return (cur2,eos2, SIVal v, if eos2 then Nil else Acc acc')


-- streaming flags2length
sFlags2len :: [Bool] -> [Int]
sFlags2len [] = []
sFlags2len (False : bs) = if null as then [1] else (1+ head as) : tail as 
    where as = sFlags2len bs    
sFlags2len (True : bs) = 0:sFlags2len bs 



sSegExScanPlus :: Int -> [Int] -> [Bool] -> ([Int], Int)
sSegExScanPlus acc is bs = 
    let ls = sFlags2len bs
        segs = seglist ls is  -- length segs >= 1 ?
        i0 = init $ scanl (+) acc $ head segs
        is' = map (init.(scanl (+) 0)) $ tail segs 
        v = i0 ++ concat is'
        acc' = if last bs then 0 else last is + last v  
    in (v, acc')



--is2flags bs int1 int2: take 'int1' elements from the index 'int2'
-- e.g. [2,3,5] ([FFT FFFT FFFFFT])
-- [2,3,5] 4 0 => [FFTF],   [2,3,5] 4 8 => [FFFF]
is2flags :: [Int] -> Int -> Int -> [Bool]
is2flags [] _ _ = []
is2flags is@(i:iss) buf acc = 
      if i + 1 < acc then is2flags iss buf (acc-i-1)
      else let bs = i2flags i  
               b0 = drop acc bs
           in if length bs < buf + acc then b0 ++ (is2flags iss (buf - length b0) 0) 
               else take buf (drop acc bs)

