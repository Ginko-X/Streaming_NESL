{- SVCODE Streaming Interpreter using Proc -}

module SvcodeProcInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc
import SneslCompiler (tree2Sids)

import Control.Monad
import Control.Monad.Trans (lift)

import Data.Bits ((.&.))

data BufState = Buf AVal | EmptyBuf | Eos deriving (Show, Eq) 

type SState = (BufState, [(SId,Bool)], Proc ())

type Svctx = [(SId, SState)]

type Dag = [[SId]]

type CTable = [(SId,[SId])] 


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



--runSvcodePExp :: SFun -> Either String (SvVal, (Int,Int))
runSvcodePExp (SFun [] st code) = 
  do let d = geneDag code 0 
         ch = geneCTab code 0
         retSids = tree2Sids st
         d' = foldl (\dag sid -> addClient dag sid (-1)) d retSids 
     (_,ctx) <- rSvcodeP (mapM_ sInstrInit code) d' ch [] 0 
     let ctx' = addEptSids ctx 0 (Eos, [], Done ())
     
     (as, _) <- rrobin (mapM (sInstrInterp retSids) code) d' ch ctx' 0 st $ initTreeAval st     
     return (fst $ constrSv st as,(0,0))

     --(as, ctxs) <- roundN 20 (round1 (mapM (sInstrInterp retSids) code) d' ch 0 st) [ctx] $ [initTreeAval st]
     --let str = map (\(a,c,i) -> "Round "++ show i ++"\n" ++ show a ++ "\n" ++ showCtx c ++ "\n") 
     --               $ zip3 (reverse as) (reverse ctxs) [0..]
     --return $ concat str



addClient :: Dag -> SId -> SId -> Dag
addClient d i c = 
  let cs0 = d !! i 
      cs = cs0 ++ [c]
  in updateList d i cs 


------ helper functions for printing out the Svctx in each round  -------------


showCtx :: Svctx -> String
showCtx [] = ""
showCtx ((sid,(buf,bs,p)):ss) = 
  "S" ++ show sid ++ " := (" ++ show buf ++ "," ++ show bs ++ "," ++ show p 
    ++ ")\n" ++ showCtx ss 


roundN :: Int -> (Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx)) -> 
            [Svctx] -> [[[AVal]]] -> Either String ([[[AVal]]], [Svctx])
roundN 0 f ctxs as = Right (as,ctxs) 
roundN c f ctxs as = 
  if all (\(_,(buf,_,p)) -> buf == Eos) (head ctxs)
    then return (as,ctxs)
    else
      do (as',ctx') <- f (head ctxs) (head as)
         roundN (c-1) f (ctx':ctxs) (as':as) 
     

round1 :: SvcodeP [Bool] -> Dag -> CTable -> SId -> 
                  STree -> Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx)              
round1 m d ch ctrl st ctx as0 = 
  do (bs, ctx') <- rSvcodeP m d ch ctx ctrl
     (as, ctx'') <- lookupTreeAval st ctx'
     let as' = zipWith (++) as0 as 
     return (as', ctx'') 

-----------------------------------

     
rrobin :: SvcodeP [Bool] -> Dag -> CTable -> Svctx -> SId -> 
                  STree -> [[AVal]] -> Either String ([[AVal]], Svctx)              
rrobin m d ch ctx ctrl st as0 = 
  do (bs, ctx') <- rSvcodeP m d ch ctx ctrl
     (as,ctx'') <- lookupTreeAval st ctx'
     let as' = zipWith (++) as0 as 
     if all (\x -> x) bs 
       then return (as', ctx'') 
       else rrobin m d ch ctx'' ctrl st as'



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



lookupTreeAval :: STree -> Svctx -> Either String ([[AVal]],Svctx)
lookupTreeAval (IStr t1) ctx = 
    case lookup t1 ctx of 
      Nothing -> Left "SVCODE runtime error: undefined streams." 

      Just (EmptyBuf, bs, p) -> 
        let bs' = markRead bs (-1) 
            ctx' = updateWithKey ctx t1 (EmptyBuf, bs', p)
         in Right ([[]], ctx')
      
      Just (Buf a, bs, p) -> 
        if allFlagT (init bs) .&. (not $ snd $ last bs)
          then Right ([[a]], updateWithKey ctx t1 (Buf a, setFlagT bs, p))
          else Right ([[]],ctx)

      Just (Eos, bs, p) -> 
        let bs' = markRead bs (-1)
            ctx' = updateWithKey ctx t1 (Eos, bs', p)
         in Right ([[]], ctx')

lookupTreeAval (BStr t1) ctx = lookupTreeAval (IStr t1) ctx

lookupTreeAval (PStr t1 t2) ctx = 
    do (v1,ctx1) <- lookupTreeAval t1 ctx 
       (v2,ctx2) <- lookupTreeAval t2 ctx1 
       return $ (v1++v2, ctx2)

lookupTreeAval (SStr t1 t2) ctx = lookupTreeAval (PStr t1 (BStr t2)) ctx




addCtx :: SId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ _ _ c _ -> Right ((), c++[(s,v)])

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \_ _ c _ -> Right ((), updateList c s (s,v)) 


getClients :: SId -> SvcodeP [SId]
getClients sid = SvcodeP $ \ d _ c _ -> Right (d !! sid, c)


getChannels :: SId -> SvcodeP [SId]
getChannels sid = SvcodeP $ \ _ ch c _ -> Right (snd $ ch !! sid, c)


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
     addCtx sid (EmptyBuf, map (\cl -> (cl,False)) cls, p)  

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


allFlagT :: [(SId,Bool)] -> Bool 
allFlagT bs = all (\(_,b) -> b) bs 

allFlagF bs = all (\(_,b) -> not b) bs


setFlagF :: [(SId, Bool)] -> [(SId, Bool)]
setFlagF bs = map (\(sid, _) -> (sid, False)) bs 

setFlagT bs = map (\(sid, _) -> (sid, True)) bs 



----- instruction interp

sInstrInterp :: [SId] -> SInstr -> SvcodeP Bool
sInstrInterp rets (SDef sid i) = 
  do (buf, bs, p) <- lookupSid sid 
     case buf of 
       Eos -> return True 
       _ -> case p of
              Done () -> (if allFlagT bs 
                            then updateCtx sid (Eos, setFlagF bs,p)                                
                            else return ()) >> return True
              Pout a p' -> (if allFlagT bs 
                              then updateCtx sid (Buf a, setFlagF bs, p')
                              else return ())  >> return False                              
              Pin i p' ->
                do chs <- getChannels sid 
                   (bufCh, flagCh,pCh) <- lookupSid (chs!!i)
                   case lookup sid flagCh of 
                     Nothing -> fail $ "undefined client " ++ show sid
                     Just True -> return False
                     Just False -> 
                       do let flagCh' = markRead flagCh sid
                          updateCtx (chs!!i) (bufCh, flagCh',pCh)
                          mbA <- readBuf bufCh
                          (if null mbA 
                             then return () 
                             else updateCtx sid (buf,bs,p' (head mbA)))
                          return False
               
 

sInstrInterp rets (WithCtrl newc code st) = 
  do (buf,_,_) <- lookupSid newc
     bs <- localCtrl newc $ mapM (sInstrInterp rets) code 
     return $ all (\x -> x) bs
     --if buf == Eos --?!
     --  then doneStream st >> return True
     --  else 
     --    do 


--doneStream :: STree -> SvcodeP ()
--doneStream (IStr s) = updateCtx s (Eos, [], Done ()) 
--doneStream (BStr s) = updateCtx s (Eos, [], Done ()) 
--doneStream (PStr st1 st2)  = doneStream st1 >> doneStream st2
--doneStream (SStr st1 st2) = doneStream (PStr st1 (BStr st2))
 

readBuf :: BufState -> SvcodeP [Maybe AVal]
readBuf buf = 
  case buf of 
    Buf a -> return [Just a]   
    Eos -> return [Nothing]
    EmptyBuf -> return []



markRead :: [(SId,Bool)] -> SId -> [(SId,Bool)]
markRead fs sid = updateWithKey fs sid True


-- update the first pair with this key `k` 
updateWithKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateWithKey [] k v = []
updateWithKey (p:ps) k v = 
  if fst p == k then (k,v): ps else p:updateWithKey ps k v



geneCTab :: [SInstr] -> SId -> CTable
geneCTab code ctrl = 
  let ctab = instrGeneCTab code ctrl 
   in addEptSids ctab 0 []


instrGeneCTab :: [SInstr] -> SId -> CTable
instrGeneCTab [] _ = []
instrGeneCTab ((SDef sid sexp):ss) c = (sid,cl0) : instrGeneCTab ss c
    where (cl0, _) = getChanExp sexp c

instrGeneCTab ((WithCtrl newc ss _):ss') c = cl ++ instrGeneCTab ss' c
    where cl = instrGeneCTab ss newc



addEptSids :: [(SId, a)] -> Int -> a -> [(SId,a)]
addEptSids [] _ _ = []
addEptSids ch@(ch0@(c0,sids):ch') i a0 = 
  if c0 == i then ch0 : addEptSids ch' (i+1) a0
    else (i,a0) : addEptSids ch (i+1) a0 



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



getChanExp :: SExp -> SId -> ([SId],String)
getChanExp Ctrl _ = ([],"Ctrl")
getChanExp EmptyCtrl _ = ([],"EmptyCtrl")
getChanExp (Const _) c = ([c],"Const")

getChanExp (MapConst s1 _) _ = ([s1],"MapConst")
getChanExp (MapOne _ s1) _ = ([s1],"MapOne")
getChanExp (MapTwo _ s1 s2) _ = ([s1,s2],"MapTwo")

getChanExp (InterMergeS ss) _ = (ss,"InterMergeS")
getChanExp (SegInterS ss) _ = (concat $ map (\(x,y) -> [x,y]) ss , "SegInterS")
getChanExp (PriSegInterS ss) _ = (concat $ map (\(x,y) -> [x,y]) ss, "PriSegInterS") 

getChanExp (Distr s1 s2) _ = ([s1,s2], "Distr")
getChanExp (SegDistr s1 s2) _ = ([s1,s2],"SegDistr")
getChanExp (SegFlagDistr s1 s2 s3) _ = ([s2,s1,s3],"SegFlagDistr")
getChanExp (PrimSegFlagDistr s1 s2 s3) _ = ([s2,s1,s3],"PrimSegFlagDistr")

getChanExp (ToFlags s1) _ = ([s1], "ToFlags")
getChanExp (Usum s1) _ = ([s1],"Usum")
getChanExp (B2u s1) _ = ([s1],"B2u")

getChanExp (SegscanPlus s1 s2) _ = ([s2,s1],"SegscanPlus")
getChanExp (ReducePlus s1 s2) _ = ([s2,s1],"ReducePlus")
getChanExp (Pack s1 s2) _ = ([s2,s1],"Pack")
getChanExp (UPack s1 s2) _ = ([s2,s1],"UPack")
getChanExp (SegConcat s1 s2) _ = ([s2,s1],"SegConcat")
getChanExp (USegCount s1 s2) _ = ([s2,s1],"USegCount")
getChanExp (SegMerge s1 s2) _ = ([s2,s1],"SegMerge")  
getChanExp (Check s1 s2) _ = ([s1,s2],"Check")



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


-------- generate a file to visualize the DAG -------------
--- use "graph-easy": graph-easy <inputfile> --png 
geneDagFile code ctrl fname = 
  do let ch0 = geneCTabExp code ctrl
         ch1 = addEmptyStreams ch0 0
         ch2 = map (\(i, str,sids) -> 
                        ("S"++ show i ++ ": " ++ str, sids)) ch1
         ch3 = map (\(str, sids) -> (str, map (\i -> fst $ ch2!!i) sids)) ch2
         lines = map (\(x,ys) -> if null ys then drawnode x else          
                  concat $ zipWith (\y c -> drawedge y x c) ys [0..]) ch3
         content = concat lines 
     writeFile fname content 
   

drawnode :: String -> String
drawnode i = "[ " ++  i ++ " ]\n"

drawedge :: String -> String -> Int -> String 
drawedge i j c = "[ " ++ i ++ " ] -- " ++ show c ++ " --> [ " ++ j ++ " ]\n"



geneCTabExp :: [SInstr] -> SId -> [(SId, String,[SId])]
geneCTabExp [] _ = []
geneCTabExp ((SDef sid sexp):ss) c = (sid,i,cl0) : geneCTabExp ss c
    where (cl0, i ) = getChanExp sexp c
geneCTabExp ((WithCtrl newc ss _):ss') c = cl ++ geneCTabExp ss' c
    where cl = geneCTabExp ss newc


addEmptyStreams :: [(SId, String, [SId])] -> Int -> [(SId, String,[SId])]
addEmptyStreams chs i = 
  let (sids, strs, sidss) = unzip3 chs  
      chs' = addEptSids (zip sids (zip strs sidss)) i ("(empty stream)",[])
      (_, strSids) = unzip chs'
      (strs', sidss') = unzip strSids
  in zip3 sids strs' sidss' 


-----------------

