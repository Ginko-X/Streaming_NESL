{- SVCODE Streaming Interpreter using Proc -}

module SvcodeProcInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc

import Control.Monad

data BufState = Buf AVal | EmptyBuf | Eos 
type SState = (BufState, [Bool], Proc ())
type Svctx = [(SId, SState)]

type Dag = [[SId]]

type CTable = [(SId,[SId])] -- channel table


newtype SvcodeP a = SvcodeP {rSvcodeP :: Dag -> CTable -> Svctx -> SId -> 
                                  Either String (a, Svctx)}

instance  Monad SvcodeP where
    return a = SvcodeP $ \ d ch ctx ctrl -> Right (a,ctx) 

    m >>= f = SvcodeP $ \ d ch ctx ctrl -> 
        case rSvcodeP m d ch ctx ctrl of 
            Right (a,ctx') -> 
                case rSvcodeP (f a) d ch ctx' ctrl of
                    Right res -> Right res 
                    Left err' -> Left err'
            Left err -> Left err 


    fail err = SvcodeP $ \ _ _ _ _ -> Left $ "SVCODE runtime error: " ++ err


instance Functor SvcodeP where
  fmap f t = t >>= return . f 

instance Applicative SvcodeP where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



runSvcodePExp :: SFun -> Either String (SvVal, (Int,Int))
runSvcodePExp (SFun [] st code) = 
  do let d = geneDag code 0 
         ch = geneCTab code 0 
     (_,ctx) <- rSvcodeP (mapM sInstrInit code) d ch [] 0
     (as, _) <- rrobin (mapM sInstrInterp code) d ch ctx 0 st [[]] 
     return (constrSv st as,(0,0))


constrSv :: STree -> [[AVal]] -> SvVal
constrSv = undefined


     
rrobin :: SvcodeP [Bool] -> Dag -> CTable -> Svctx -> SId -> 
                  STree -> [[AVal]] -> Either String ([[AVal]], Svctx)              
rrobin m d ch ctx ctrl st as0 = 
  do (bs, ctx') <- rSvcodeP m d ch ctx ctrl
     as <- lookupTreeAval st ctx'
     let as' = zipWith (++) as0 as 
     if all (\x -> x) bs 
       then return (as', ctx') 
       else rrobin m d ch ctx' ctrl st as'



lookupTreeAval :: STree -> Svctx -> Either String [[AVal]]
lookupTreeAval (IStr t1) ctx = 
    case lookup t1 ctx of 
      Nothing -> Left "SVCODE runtime error: undefined streams." 
      Just (EmptyBuf, _, _) -> Right [[]]
      Just (Buf a, _,_) -> Right [[a]]


lookupTreeAval (BStr t1) ctx = lookupTreeAval (IStr t1) ctx

lookupTreeAval (PStr t1 t2) ctx = 
    do v1 <- lookupTreeAval t1 ctx 
       v2 <- lookupTreeAval t2 ctx 
       return $ v1++v2 

lookupTreeAval (SStr t1 t2) ctx = lookupTreeAval (PStr t1 (BStr t2)) ctx




addCtx :: SId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ _ _ c _ -> Right ((), [(s,v)]++c)

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \_ _ c _ -> Right ((), updateList c s (s,v)) 


getClients :: SId -> SvcodeP [SId]
getClients sid = SvcodeP $ \ d _ c _ -> Right (d !! sid, c)


getChannels :: SId -> SvcodeP [SId]
getChannels sid = SvcodeP $ \ _ ch c _ -> 
    case lookup sid ch of 
      Nothing -> Left $ "Undefined SId: " ++ show sid
      Just v -> Right (v, c)
 

getCtrl :: SvcodeP SId 
getCtrl = SvcodeP $ \ _ _ ctx ctrl -> Right (ctrl, ctx)

localCtrl :: SId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ d ch ctx _  -> rSvcodeP m d ch ctx ctrl


getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ _ _ ctx _ -> Right (ctx, ctx)


setCtx :: Svctx -> SvcodeP ()
setCtx c = SvcodeP $ \ _ _ _ _ -> Right ((), c)


lookupSid :: SId -> SvcodeP SState
lookupSid s = SvcodeP $ \_ _ c ctrl -> 
    case lookup s c of 
        Nothing -> Left $ "Undefined SId: " ++ show s  
        Just st -> Right (st,c)  


------ instruction init

sInstrInit :: SInstr -> SvcodeP ()
sInstrInit (SDef sid i) = sExpInit sid i >>= addCtx sid 


---- exp init
sExpInit :: SId -> SExp -> SvcodeP SState
sExpInit sid Ctrl = 
  do cls <- getClients sid 
     return (EmptyBuf, map (\_ -> False) cls, rout (BVal False)) 


sExpInit sid (Const a) = 
  do cls <- getClients sid 
     return (EmptyBuf, map (\_ -> False) cls, mapConst a)
  
sExpInit s0 (MapConst s1 a) = 
  do cls <- getClients s0 
     return (EmptyBuf, map (\_ -> False) cls, mapConst a)



----- instruction interp

sInstrInterp :: SInstr -> SvcodeP Bool
sInstrInterp (SDef sid i) = 
  do (sta, bs, p) <- lookupSid sid 
     if p == Done ()
       then return True -- 
       else 
         do if all (\b -> b) bs -- all channels have read the buffer
              then 
                do ch <- getChannels sid
                   sta' <- sExpInterp i ch p bs
                   updateCtx sid sta'
                   return False
              else return False
            


sExpInterp :: SExp -> [SId] -> Proc () -> [Bool] -> SvcodeP SState
sExpInterp Ctrl _ p bs = 
  let (a,p') = evalProcA p [] 
  in return (Buf a, bs, p')


sExpInterp (Const a) chs p bs = 
  do as <- readBuf chs 
     zipWithM updateFlag chs [0] 
     let (a,p') = evalProcA p as 
     return (Buf a, bs, p')



readBuf :: [SId] -> SvcodeP [AVal]
readBuf = undefined


updateFlag :: SId -> Int -> SvcodeP ()
updateFlag s i = 
  do (buf, bs, p) <- lookupSid s 
     let bs' = updateList bs i True 
     updateCtx s (buf,bs', p) 




-- generate the channel table for an SVCODE program
geneCTab :: [SInstr] -> SId -> CTable
geneCTab [] _ = []
geneCTab ((SDef sid sexp):ss) c = (sid,cl0) : geneCTab ss c
    where cl0 = getChan sexp c

geneCTab ((WithCtrl newc ss _):ss') c = cl ++ geneCTab ss' c
    where cl = geneCTab ss newc


-- generate the DAG 
geneDag :: [SInstr] -> SId -> Dag 
geneDag ss c = let sc = sidCount ss in dagUpdate ss c $ replicate sc []


sidCount :: [SInstr] -> Int 
sidCount [] = 0 
sidCount ss = 
  let s = last ss in 
    case s of 
      SDef i _ -> i+1 
      WithCtrl _ ss _ -> sidCount ss 



getChan :: SExp -> SId -> [SId]
getChan Ctrl _ = []
getChan EmptyCtrl _ = []
getChan (Const _) c = [c]

getChan (MapConst s1 _) _ = [s1]
getChan (MapOne _ s1) _ = [s1]
getChan (MapTwo _ s1 s2) _ = [s1,s2]

getChan (InterMergeS ss) _ = ss 
getChan (SegInterS ss) _ = concat $ map (\(x,y) -> [x,y]) ss 
getChan (PriSegInterS ss) _ = concat $ map (\(x,y) -> [x,y]) ss 

getChan (Distr s1 s2) _ = [s1,s2]
getChan (SegDistr s1 s2) _ = [s1,s2]
getChan (SegFlagDistr s1 s2 s3) _ = [s2,s1,s3]
getChan (PrimSegFlagDistr s1 s2 s3) _ = [s2,s1,s3]

getChan (ToFlags s1) _ = [s1] 
getChan (Usum s1) _ = [s1]
getChan (B2u s1) _ = [s1]

getChan (SegscanPlus s1 s2) _ = [s2,s1]
getChan (ReducePlus s1 s2) _ = [s2,s1]  
getChan (Pack s1 s2) _ = [s2,s1]  
getChan (UPack s1 s2) _ = [s2,s1]  
getChan (SegConcat s1 s2) _ = [s2,s1]  
getChan (USegCount s1 s2) _ = [s2,s1]  
getChan (SegMerge s1 s2) _ = [s2,s1]  
getChan (Check s1 s2) _ = [s1,s2]  




dagUpdate :: [SInstr] -> SId -> Dag -> Dag 
dagUpdate [] _ d = d 
dagUpdate (def@(SDef _ _) : ss) c d = dagUpdate ss c $ addDefEdge c d def 
dagUpdate ((WithCtrl newc ss _): ss') c d = 
    dagUpdate ss' c $ dagUpdate ss newc d 


addDefEdge :: SId -> Dag -> SInstr -> Dag 
addDefEdge _ d (SDef _ Ctrl) = d 
addDefEdge _ d (SDef _ EmptyCtrl) = d
addDefEdge c d (SDef i (Const _)) = addEdges [c] i d

addDefEdge _ d (SDef i (MapConst j _)) = addEdges [j] i d 
addDefEdge _ d (SDef i (MapOne _ j)) = addEdges [j] i d 
addDefEdge _ d (SDef i (MapTwo _ j k)) = addEdges [j,k] i d

addDefEdge _ d (SDef i (InterMergeS ss)) = addEdges ss i d 
addDefEdge _ d (SDef i (SegInterS ss)) = 
  let sids = concat $ map (\(x,y) -> [x,y]) ss in addEdges sids i d
addDefEdge _ d (SDef i (PriSegInterS ss)) = 
  let sids = concat $ map (\(x,y) -> [x,y]) ss in addEdges sids i d

addDefEdge _ d (SDef i (Distr j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegDistr j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegFlagDistr j k m)) = addEdges [j,k,m] i d
addDefEdge _ d (SDef i (PrimSegFlagDistr j k m)) = addEdges [j,k,m] i d

addDefEdge _ d (SDef i (ToFlags j)) = addEdges [j] i d
addDefEdge _ d (SDef i (Usum j)) = addEdges [j] i d
addDefEdge _ d (SDef i (B2u j)) = addEdges [j] i d

addDefEdge _ d (SDef i (SegscanPlus j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (ReducePlus j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (Pack j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (UPack j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegConcat j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (USegCount j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegMerge j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (Check j k)) = addEdges [j,k] i d


-- add edges from js to i 
addEdges :: [SId] -> SId -> Dag -> Dag 
addEdges [] _ d = d 
addEdges (j:js) i d = addEdges js i d'
    where d' = updateList d j (e0++[i])  
          e0 = d!!j 


------ generate a file to visualize the DAG 
--- use "graph-easy": graph-easy <inputfile> --png 
geneDagFile ss c sc fname = 
  do let d = geneDag ss c
         ps = [0..length d -1]
         lines = zipWith (\x ys -> 
           if null ys then drawpoint x 
             else concat $ map (\y -> drawedge x y) ys) ps d
         content = concat lines 
     writeFile fname content 
   

drawpoint :: Int -> String
drawpoint i = "[" ++ show i ++ "]\n"

drawedge :: Int -> Int -> String 
drawedge i j = "[" ++ show i ++ "] ---> [" ++ show j ++ "] \n"

-----------------

