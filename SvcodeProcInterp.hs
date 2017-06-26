{- SVCODE Streaming Interpreter using Proc -}

module SvcodeProcInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc

import Control.Monad
import Control.Monad.Trans (lift)


data BufState = Buf AVal | EmptyBuf | Eos deriving (Show, Eq) 

type SState = (BufState, [(SId,Bool)], Proc ())
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
     (as, _) <- rrobin (mapM sInstrInterp code) d ch ctx 0 st $ initTreeAval st
     return (fst $ constrSv st as,(0,0))

     
rrobin :: SvcodeP [Bool] -> Dag -> CTable -> Svctx -> SId -> 
                  STree -> [[AVal]] -> Either String ([[AVal]], Svctx)              
rrobin m d ch ctx ctrl st as0 = 
  do (bs, ctx') <- rSvcodeP m d ch ctx ctrl
     as <- lookupTreeAval st ctx'
     let as' = zipWith (++) as0 as 
     if all (\x -> x) bs 
       then return (as', ctx') 
       else rrobin m d ch ctx' ctrl st as'



constrSv :: STree -> [[AVal]] -> (SvVal, [[AVal]])
constrSv (IStr t1) as = (SIVal [a | IVal a <- head as], tail as)
constrSv (BStr t1) as = (SBVal [b | BVal b <- head as], tail as)
constrSv (PStr t1 t2) as = 
  let (v1,as') = constrSv t1 as
      (v2,as'') = constrSv t2 as'
   in (SPVal v1 v2, as'')

constrSv (SStr t1 t2) as = 
  let (v1,as') = constrSv t1 as 
      (SBVal v2, as'') = constrSv (BStr t2) as'
   in (SSVal v1 v2, as'') 


initTreeAval :: STree -> [[a]]
initTreeAval (IStr t1) = [[]]
initTreeAval (BStr t1) = [[]]
initTreeAval (PStr t1 t2) = initTreeAval t1 ++ initTreeAval t2
initTreeAval (SStr t1 t2) = initTreeAval t1 ++ initTreeAval (BStr t2)



lookupTreeAval :: STree -> Svctx -> Either String [[AVal]]
lookupTreeAval (IStr t1) ctx = 
    case lookup t1 ctx of 
      Nothing -> Left "SVCODE runtime error: undefined streams." 
      Just (EmptyBuf, _, _) -> Right [[]]
      Just (Buf a, _,_) -> Right [[a]]
      Just (Eos, _, _) -> Right [[]]


lookupTreeAval (BStr t1) ctx = lookupTreeAval (IStr t1) ctx

lookupTreeAval (PStr t1 t2) ctx = 
    do v1 <- lookupTreeAval t1 ctx 
       v2 <- lookupTreeAval t2 ctx 
       return $ v1++v2 

lookupTreeAval (SStr t1 t2) ctx = lookupTreeAval (PStr t1 (BStr t2)) ctx




addCtx :: SId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ _ _ c _ -> Right ((), c++[(s,v)])

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \_ _ c _ -> Right ((), updateList c s (s,v)) 


getClients :: SId -> SvcodeP [SId]
getClients sid = SvcodeP $ \ d _ c _ -> Right (d !! sid, c)


getChannels :: SId -> SvcodeP [SId]
getChannels sid = SvcodeP $ \ _ ch c _ -> 
    case lookup sid ch of 
      Nothing -> Left $ "getChannels: undefined SId " ++ show sid
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
        Nothing -> Left $ "lookupSid: undefined SId " ++ show s  
        Just st -> Right (st,c)  


------ instruction init

sInstrInit :: SInstr -> SvcodeP ()
sInstrInit (SDef sid e) = 
  do cls <- getClients sid
     p <- sExpProcInit e 
     addCtx sid (EmptyBuf, map (\cl -> (cl,True)) cls, p)  

sInstrInit (WithCtrl _ code _) = mapM_ sInstrInit code


---- exp init
sExpProcInit :: SExp -> SvcodeP (Proc ())
sExpProcInit Ctrl = return $ rout (BVal False)

sExpProcInit EmptyCtrl = return $ Done () 

sExpProcInit (Const a) = return (mapConst a)
  
sExpProcInit (MapConst s1 a) = return (mapConst a)

sExpProcInit (Usum _) = return usumProc

sExpProcInit (ToFlags _) = return toFlags 

sExpProcInit (MapTwo op _ _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapTwo fop)

sExpProcInit (MapOne op _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapOne fop)

sExpProcInit (Pack _ _) = return packProc

sExpProcInit (UPack _ _) = return upackProc

sExpProcInit (Distr _ _) = return pdistProc

sExpProcInit (SegDistr _ _ ) = return segDistrProc

sExpProcInit (SegFlagDistr _ _ _) = return segFlagDistrProc

sExpProcInit (PrimSegFlagDistr _ _ _) = return primSegFlagDistrProc

sExpProcInit (B2u _) = return b2uProc

sExpProcInit (SegscanPlus _ _) = return segScanPlusProc 

sExpProcInit (ReducePlus _ _) = return segReducePlusProc

sExpProcInit (SegConcat _ _) = return segConcatProc

sExpProcInit (USegCount _ _) = return uSegCountProc

sExpProcInit (InterMergeS ss) = return (interMergeProc $ length ss)

sExpProcInit (SegInterS ss) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
  in return $ segInterProc chs 

sExpProcInit (PriSegInterS ss) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
  in return $ priSegInterProc chs 

sExpProcInit (SegMerge _ _) = return segMergeProc 

--sExpProcInit (Check )


lookupOpA :: OP -> OpAEnv -> SvcodeP ([AVal] -> AVal)
lookupOpA op r = 
  do case lookup op r of
       Just (v,_) -> return v 
       Nothing -> fail $ "SVCODE: can't find " ++ show op 


----- instruction interp

sInstrInterp :: SInstr -> SvcodeP Bool
sInstrInterp (SDef sid i) = 
  do (_, bs, p) <- lookupSid sid 
     if p == Done ()
       then updateCtx sid (Eos,bs,p) >> return True -- 0
       else 
         do if all (\(_,b) -> b) bs -- all channels have read the buffer
              then 
                do chs <- getChannels sid
                   staChs <- mapM lookupSid chs 
                   let (bufChs, flagChs, pChs) = unzip3 staChs
                   bufs <- readBuf bufChs
                   let flagChs' = map (\f -> updateWithKey f sid True) flagChs
                       staChs' = zip3 bufChs flagChs' pChs
                   zipWithM updateCtx chs staChs'
                   (buf,p') <- evalProcA p bufs 
                   let bs' = map (\(cl,_) -> (cl,False)) bs 
                   updateCtx sid (buf, bs', p')
                   return False
              else return False
 

sInstrInterp (WithCtrl newc code st) = 
  do (buf,_,_) <- lookupSid newc
     if buf == Eos 
       then doneStream st >> return True
       else 
         do bs <- localCtrl newc $ mapM sInstrInterp code
            return $ all (\x -> x) bs


doneStream :: STree -> SvcodeP ()
doneStream (IStr s) = addCtx s (Eos, [], Done ()) 
doneStream (BStr s) = addCtx s (Eos, [], Done ()) 
doneStream (PStr st1 st2)  = doneStream st1 >> doneStream st2
doneStream (SStr st1 st2) = doneStream (PStr st1 (BStr st2))
 

evalProcA :: Proc () -> [[Maybe AVal]] -> SvcodeP (BufState, Proc ())
evalProcA p0@(Pin c p1) as = 
  if null $ concat as 
    then return (EmptyBuf,p0)
    else case (p1 $ head $ as !! c) of 
           p2@(Pin c' _ ) -> if c == c'
                               then return (EmptyBuf, p2) 
                               else evalProcA p2 as
           otherP -> evalProcA otherP as  
 
evalProcA (Pout a p) _ = return (Buf a, p) 

evalProcA (Done ()) as = return (Eos, Done())


readBuf :: [BufState] -> SvcodeP [[Maybe AVal]]
readBuf bufs = 
     return $ map (\buf -> 
                   case buf of
                     Eos -> [Nothing] 
                     Buf a -> [Just a]
                     EmptyBuf -> []
                 ) bufs 


updateWithKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateWithKey [] k v = []
updateWithKey (p:ps) k v = 
  if fst p == k then (k,v):ps else p:updateWithKey ps k v


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

dagUpdate :: [SInstr] -> SId -> Dag -> Dag 
dagUpdate [] _ d = d 
dagUpdate (def@(SDef _ _) : ss) c d = dagUpdate ss c $ addDefEdge c d def 
dagUpdate ((WithCtrl newc ss _): ss') c d = 
    dagUpdate ss' c $ dagUpdate ss newc d 


-- get the count of the SIds/streams according to the last SId number
sidCount :: [SInstr] -> Int 
sidCount [] = 0 
sidCount ss = 
  let s = last ss in  
    case s of 
      SDef i _ -> i+1 
      WithCtrl _ ss' _ -> if null ss' then sidCount (init ss) else sidCount ss'



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

