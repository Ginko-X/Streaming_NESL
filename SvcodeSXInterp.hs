{- SVCODE Streaming Interpreter with arbitrary buffer size -}
-- support recursion

module SvcodeSXInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeXducer
import SneslCompiler (tree2Sids)

import Control.Monad
import Data.Set (fromList, toList)
import qualified Data.Map.Strict as M

type RSId = (Int,SId,SId)


type Svctx = M.Map RSId Process

type Process = (BufState, Suppliers, Clients, Xducer ())

data BufState = Filling [AVal] 
              | Draining [AVal] Bool 
              deriving (Show, Eq) 


type Suppliers = [RSId]  -- incoming edges
type Clients  = [((RSId,Int), Int)] --outgoing edges


type Sup = [(SId,[SId])]  -- supplier list
type Dag = [(SId,[(SId,Int)])] -- client list


type REnv = M.Map RSId RSId   -- 


newtype SvcodeP a = SvcodeP {rSvcodeP :: Svctx -> RSId -> FEnv -> Int-> REnv ->
                                Either String (a,(Int,Int),Svctx, REnv)}
                                  
instance  Monad SvcodeP where
    return a = SvcodeP $ \ctx ctrl fe sf re -> Right (a,(0,0),ctx, re) 

    m >>= f = SvcodeP $ \ctx ctrl fe sf re -> 
        case rSvcodeP m ctx ctrl fe sf re of 
            Right (a,(w,s),ctx',re') -> 
                case rSvcodeP (f a) ctx' ctrl fe sf re' of
                    Right (b,(w',s'),ctx'',re'') -> Right (b,(w+w',s+s'),ctx'',re'')
                    Left err' -> Left err'
            Left err -> Left err 

    fail err = SvcodeP $ \ _ _ _ _ _ -> Left $ "SVCODE runtime error: " ++ err


instance Functor SvcodeP where
  fmap f t = t >>= return . f 

instance Applicative SvcodeP where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



runSvcodePExp :: SFun -> Int -> Bool -> FEnv -> Either String ([(SId,[AVal])], (Int,Int))
runSvcodePExp (SFun [] retSids code fresh) bs _ fe = 
  do let (sup,d) = geneSupDag code 0
         retRSids = [((0,head retSids,r),i) | (r,i) <- zip retSids [0..]]
         ctrl = (0,head retSids,0) 
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d 
                      $ zip retSids [0..]
     (_,_, ctx,re) <- rSvcodeP (sInit code (head retSids) d' sup) M.empty ctrl fe 0 M.empty
     (as,(w1,s1), _,_) <- rrobin (mapM_ (sInstrInterp bs (head retSids)) code) 
                           ctx retRSids (map (\_ -> []) retSids) (0,0) fe re
     return (zip retSids as,(w1,s1))


addClient :: Dag -> SId -> (SId,Int) -> Dag
addClient d i c = 
  case lookup i d of 
    Nothing -> addByKey d i c
    Just cl -> updateByKey d i $ cl ++ [c] 




-- scheduling  
rrobin :: SvcodeP () -> Svctx -> [(RSId,Int)] -> [[AVal]] -> (Int,Int)
           -> FEnv -> REnv -> Either String ([[AVal]],(Int,Int),Svctx,REnv)           
rrobin m ctx retRSids as0 (w0,s0) fe re = 
  do let ctrl = replaceSid (fst $ head retRSids) 0
     (_,(w1,s1), ctx',re') <- rSvcodeP m ctx ctrl fe 0　re
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                             do (a,c) <- lookupAval c0 s re'
                                return (a:a0,c)) 
                         ([],ctx') retRSids 
     let as' = zipWith (++) as0 (reverse as)
         work = w0+w1
     if M.foldl (\b (_,_,_,p) -> b && (p == Done ())) True ctx''   -- !!
       then return (as',(work, s0+s1) , ctx'',re') 
       else if compCtx ctx ctx'' 
            then do unlockCtx <- stealing ctx''  -- try stealing
                    rrobin m unlockCtx retRSids as' (work,s0+s1+1) fe re'
            else rrobin m ctx'' retRSids as' (work,s0+s1) fe re'


replaceSid :: RSId -> SId -> RSId
replaceSid (sf,r,_) sid = (sf,r,sid)

-- pick out the first non-empty stream in Filling mode to drain
stealing :: Svctx -> Either String Svctx
stealing ctx = 
  do let ctxl = M.toAscList ctx 
     ctx' <- switchOne ctxl
     return $ M.fromList ctx'

switchOne :: [(RSId,Process)] -> Either String [(RSId,Process)]
switchOne [] = Left "Deadlock!" 
switchOne ((sid, (Filling as@(a0:_), sup, bs, p)):ss) = 
  Right $ (sid, (Draining as False, sup,bs,p)) : ss 
switchOne (s:ss) = switchOne ss >>= (\ss' -> return (s:ss'))


-- not 100% correct because Xducers are not comparable
compCtx :: Svctx -> Svctx -> Bool
compCtx ctx1 ctx2 = 
  let c1 = M.toList ctx1
      c2 = M.toList ctx2
   in compListCtx c1 c2 

compListCtx [] [] = True
compListCtx ((s1,(buf1,c1,bs1,p1)):ctx1) ((s2,(buf2,c2,bs2,p2)):ctx2) =         
  if (s1 == s2) && (buf1 == buf2) && (c1 == c2) && (bs1 == bs2) 
       && (compXducer p1 p2)
    then compListCtx ctx1 ctx2 
    else False 

compXducer :: Xducer () -> Xducer () -> Bool
compXducer (Pin i1 _) (Pin i2 _) = i1 == i2 
compXducer (Pout a p1) (Pout b p2) = (a == b) && (compXducer p1 p2)
compXducer (Done a) (Done b) = a == b 
compXducer _ _ = False


lookupAval :: Svctx -> (RSId, Int) -> REnv -> Either String ([AVal],Svctx)
lookupAval ctx (s@(sf,r,sid),i) re = 
   let s' = rsidMap s re
   in case M.lookup s' ctx of 
        Nothing -> Left  $ "lookupAval: undefined stream " ++ show s'  
        Just (Draining a b, c, bs, p) -> 
          if checkKeyVal bs ((sf,r,-1),i) 0
          then let bs' = updateByKey bs ((sf,r,-1),i) (length a)  
                   ctx' = M.adjust (\_ -> (Draining a b, c, bs', p)) s' ctx 
               in Right (a, ctx')
          else Right ([],ctx)
        
        Just (Filling _, _,_, _) -> Right ([], ctx)



addCtx :: RSId -> Process -> SvcodeP ()
addCtx s v = SvcodeP $ \ c _ _ _ re -> 
  let s' = rsidMap s re in Right ((), (0,0), M.insert s' v c, re)


addCtxChk :: RSId -> Process -> SvcodeP ()
addCtxChk s v = 
  do ctx <- getCtx
     s' <- rsidMapM s 
     case M.lookup s' ctx of 
       Nothing -> addCtx s' v 
       Just _ -> return ()



-- add RSId mapping; 
-- Note: if the key is already present in the map, the old value will be replaced
addREnv :: RSId -> RSId -> SvcodeP ()
addREnv s1 s2 = SvcodeP $ \ c _ _ _ re -> Right ((),(0,0),c, M.insert s1 s2 re)


updateCtx :: RSId -> Process -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ _ _ re -> 
  let s' = rsidMap s re in Right ((),(0,0),M.adjust (\_ -> v) s' c,re) 

getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ c _ _ _ re -> Right (c,(0,0),c,re)


getSF = SvcodeP $ \ c _ _ sf re -> Right (sf, (0,0),c,re)

localSF :: Int -> SvcodeP a -> SvcodeP a 
localSF sf m = SvcodeP $ \ ctx ctrl fe _ re -> rSvcodeP m ctx ctrl fe sf re


getCtrl :: SvcodeP RSId
getCtrl = SvcodeP $ \ ctx ctrl _ _ re -> Right (ctrl,(0,0),ctx,re)


getSuppiler sid = 
  do (_,sup,_,_) <- lookupRSid sid 
     return sup 


getCtxClient s = SvcodeP $ \c _ _ _ re -> 
  case M.lookup s c of 
    Nothing -> Right ([],(0,0),c,re)
    Just (_,_,cl,_) -> Right (fst $ unzip cl, (0,0),c,re)


localCtrl :: RSId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ ctx _ fe sf re -> rSvcodeP m ctx ctrl fe sf re 


lookupRSid :: RSId -> SvcodeP Process
lookupRSid r = SvcodeP $ \c _ _ _ re -> 
    case M.lookup (rsidMap r re) c of 
        Nothing -> Left $ "lookupRSid: undefined RSId " ++ show r
        Just st -> Right (st,(0,0),c,re)  


rsidMap :: RSId -> REnv -> RSId
rsidMap s re = 
  case M.lookup s re of 
    Nothing -> s
    Just s' -> rsidMap s' re


-- lookup the mapped RSId in REnv 
-- if no mapping exists, return itself
rsidMapM :: RSId -> SvcodeP RSId
rsidMapM s = SvcodeP $ \ c _ _ _ re  -> Right (rsidMap s re, (0,0),c,re)


lookupFId :: FId -> SvcodeP SFun
lookupFId fid = SvcodeP $ \c _ fe _ re -> 
    case lookup fid fe of 
        Nothing -> Left $ "lookupFId: undefined FId " ++ show fid
        Just sf -> Right (sf,(0,0),c,re)  


costInc :: (Int, Int) -> SvcodeP ()
costInc (w,s) = SvcodeP $ \ c _ _ sf re -> Right ((), (w,s), c,re)


clRSid :: [(SId,Int)] -> Int -> SId -> [(RSId,Int)]
clRSid cls sf sid = 
  let (sids,labels) = unzip cls 
      cls' = zip3 (repeat sf) (repeat sid) sids
  in zip cls' labels


s2Rs :: [SId] -> Int -> SId -> [RSId]
s2Rs sids sf r = [(sf,r, sid) | sid <- sids] 

curStart :: [(RSId,Int)] -> Clients
curStart cls = map (\cl -> (cl,0)) cls



getClientR :: Dag -> Int -> Int  -> SId -> SvcodeP [(RSId,Int)]
getClientR d sf r sid = 
  case lookup sid d of
    Nothing -> return []
    Just cls ->  
      do let (clR,is) = unzip $ clRSid cls sf r
         clR' <- mapM rsidMapM clR 
         return $ zip clR' is 


getSupR :: Sup -> Int -> Int  -> SId -> SvcodeP [RSId]
getSupR d sf r sid = 
  case lookup sid d of
    Nothing -> return [] 
    Just sups -> mapM rsidMapM $ s2Rs sups sf r 

 

------ processes (including xducer) initialization

sInit :: [SInstr] -> SId -> Dag -> Sup -> SvcodeP ()
sInit code r d sup = mapM_ (\i -> sInstrInit i r d sup) code

sInstrInit :: SInstr -> SId -> Dag -> Sup -> SvcodeP ()
sInstrInit (SDef sid e) r d sup = 
  do sf <- getSF
     clR <- getClientR d sf r sid 
     supR <- getSupR sup sf r sid     
     p <- sExpXducerInit e  
     ctx <- getCtx
     case M.lookup (sf,r,sid) ctx of 
       Nothing -> addCtx (sf,r,sid) (Filling [], supR, curStart clR, p)
       Just (buf,sup0,cl0,_) -> 
         updateCtx (sf,r,sid) (buf, sup0 ++ supR, cl0 ++ (curStart clR), p)


sInstrInit (WithCtrl ctrl ins code tRet) r d sup =
  do sf <- getSF
     let ctrlR = (sf,r,ctrl)
         retSids = snd $ unzip tRet 
     (buf,s,cl,p) <- lookupRSid ctrlR
     updateCtx ctrlR (buf,s,(((sf,r,-2),0),0):cl,p) -- add a first-chunk flag
     
     -- initial the return sids
     let retRSids = s2Rs retSids sf r
     retClsR <- mapM (getClientR d sf r) retSids 
     --retRSids' <- mapM rsidMapM retRSids    
     zipWithM_ (\s cl -> addCtxChk s (Filling [], [], curStart cl, Done ()))
       retRSids retClsR  
     
     ins' <- mapM rsidMapM (s2Rs ins sf r)
     mapM_ (addRSClient [((sf,r,-3),0)]) ins'  -- add pseudoclient


sInstrInit (SCall fid argSid retSids) r d sup = 
  do (SFun fmArgs fmRets code count) <- lookupFId fid
     let (fmSup,fmDag) = geneSupDag code 0 

     sf <- getSF
     let fmSf = sf +1 
         fmR = (r*count) + (head retSids)
         fmCtrlR = (fmSf,fmR,0) 
        
         argRSids = s2Rs argSid sf r -- actual parameters
         retRSids = s2Rs retSids sf r
         fmArgsR = s2Rs fmArgs fmSf fmR -- formal parameters
         fmRetsR = s2Rs fmRets fmSf fmR
     
     retCtxCls <- mapM getCtxClient retRSids  -- get clients before mapping
         
     ctrl <- getCtrl
     zipWithM_ addREnv (fmCtrlR:fmArgsR++retRSids) (ctrl: argRSids++fmRetsR) -- add mappings
     
     localCtrl fmCtrlR $ localSF fmSf $ sInit code fmR fmDag fmSup --unfolding
     
     -- add clients to the actual ctrl and paramters
     ctrlCl <- getClientR fmDag fmSf fmR 0 
     fmArgsClsR <- mapM (getClientR fmDag fmSf fmR) fmArgs
     zipWithM_ (\s cl -> addRSClient cl s) (ctrl:argRSids) (ctrlCl:fmArgsClsR)

     retClsR <- mapM (getClientR d sf r) retSids
     zipWithM_ (\ret cl -> addRSClient cl ret) fmRetsR $ zipWith (++) retClsR retCtxCls


---- SExpression init
sExpXducerInit :: SExp -> SvcodeP (Xducer ())
sExpXducerInit Ctrl = return $ rout (BVal False)
sExpXducerInit EmptyCtrl = return $ Done () 
sExpXducerInit (Const a _) = return (constXducerN a)
sExpXducerInit (Usum _) = return usumXducerN
sExpXducerInit (ToFlags _) = return toFlagsN 

sExpXducerInit (MapTwo op _ _ _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapTwoN fop)

sExpXducerInit (MapOne op _ _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapOneN fop)

sExpXducerInit (Pack _ _ _) = return packXducerN
sExpXducerInit (UPack _ _) = return upackXducerN
sExpXducerInit (Distr _ _ _) = return pdistXducerN
sExpXducerInit (SegDistr _ _ ) = return segDistrXducerN
sExpXducerInit (SegFlagDistr _ _ _) = return segFlagDistrXducerN
sExpXducerInit (PrimSegFlagDistr _ _ _ _) = return primSegFlagDistrXducerN
sExpXducerInit (B2u _) = return b2uXducerN
sExpXducerInit (SegscanPlus _ _) = return segScanPlusXducerN 
sExpXducerInit (ReducePlus _ _) = return segReducePlusXducerN
sExpXducerInit (SegConcat _ _) = return segConcatXducerN
sExpXducerInit (USegCount _ _) = return uSegCountXducerN
sExpXducerInit (InterMergeS ss) = return (interMergeXducerN $ length ss + 1)

sExpXducerInit (SegInterS ss) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
      chs' = [(x+1,y+1)| (x,y) <- chs]
  in return $ segInterXducerN chs' 

sExpXducerInit (PriSegInterS ss _) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
      chs' = [(x+1,y+1)| (x,y) <- chs]
  in return $ priSegInterXducerN chs' 

sExpXducerInit (Check _ _) = return checkXducerN
sExpXducerInit (IsEmpty _) = return isEmptyXducerN


lookupOpA :: OP -> OpAEnv -> SvcodeP ([AVal] -> AVal)
lookupOpA op r = 
  do case lookup op r of
       Just (v,_) -> return v 
       Nothing -> fail $ "lookupOpA: undefined operation " ++ show op 


----- run a process
sSIdInterp :: Int -> RSId -> SvcodeP ()
sSIdInterp bufSize sid0 = 
  do sid <- rsidMapM sid0
     (buf, sups, bs, p) <- lookupRSid sid 
     case p of 
       Done () -> 
         case buf of 
           Filling as -> do if null as then costInc (0,0) else costInc (0,1) 
                            updateCtx sid (Draining as True, sups, bs, p) 
           Draining as True -> return ()
           Draining as False -> updateCtx sid (Draining as True, sups, bs, p) 
       
       Pout a p' ->  
         case buf of 
           Filling as -> do if length as +1 >= bufSize  
                            then do updateCtx sid (Draining (as ++ [a]) False, sups,bs,p')
                                    costInc (1,1)
                            else do updateCtx sid (Filling (as ++ [a]), sups,bs,p')
                                    costInc (1,0)
                            sSIdInterp bufSize sid  -- loop
           Draining as _ -> 
             if allEnd bs (length as)
               then do updateCtx sid (Filling [], sups, resetCur bs, p)
                       sSIdInterp bufSize sid -- start filling
               else return () -- blocking           

       Pin i p' -> 
         case buf of 
           _ ->  
             do supRep <- rsidMapM (sups!!i)
                (bufSup, supSup, flagSup,pSup) <- lookupRSid supRep
                case lookup (sid,i) flagSup of 
                  Nothing -> fail $ show sid ++ " reads an undefined supplier " 
                                      ++ show supRep 
                  Just cursor -> 
                    case bufSup of                
                      Filling _ -> return () -- read unavailable, blocking 
                     
                      Draining asSup False ->   -- normal read
                        if cursor >= length asSup
                          then return () -- read blocking 
                          else -- read successfully
                            do let flagSup' = markRead flagSup (sid,i) cursor
                               updateCtx supRep (bufSup, supSup, flagSup',pSup)  
                               updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor))
                               costInc (1,0) 
                               sSIdInterp bufSize sid -- loop

                      Draining asSup True ->   -- read the last chunk
                        if cursor >= length asSup
                          then 
                            do updateCtx sid (buf,sups,bs,p' Nothing) -- read `Nothing`
                               sSIdInterp bufSize sid -- loop 
                          else -- read successfully
                            do let flagSup' = markRead flagSup (sid,i) cursor
                               updateCtx supRep (bufSup, supSup, flagSup',pSup)  
                               updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor))
                               costInc (1,0)  
                               sSIdInterp bufSize sid -- loop


-- interpret an instruction
sInstrInterp :: Int -> SId -> SInstr -> SvcodeP ()
sInstrInterp bufSize r def@(SDef sid _) = 
  do sf <- getSF
     sSIdInterp bufSize (sf,r,sid) 


sInstrInterp bufSize r (WithCtrl ctrl ins code tRet) = 
  do sf <- getSF
     let ctrlR = (sf,r,ctrl)
         retSids = snd $ unzip tRet
     (buf,c,curs,p) <- lookupRSid ctrlR

     case lookup ((sf,r,-2),0) curs of
       Nothing -> -- WithCtrl already unfolded
         localCtrl ctrlR $ mapM_ (sInstrInterp bufSize r) code

       Just 0 ->  
         case buf of 
           Draining [] True ->  -- `ctrl` is empty
             do bs <- mapM isDone $ s2Rs retSids sf r
                if foldl (&&) True bs 
                 then return ()
                 else do let retSids' = filter (\r -> r > ctrl) retSids
                             retRSids = s2Rs retSids' sf r                 
                         doneStreams retRSids
                         -- delete pseudoclient 
                         ins' <- mapM rsidMapM [(sf,r,i) | i<-ins]
                         mapM_ (delRSClient ((sf,r,-3),0)) ins'
           
           Filling [] ->  return () -- keep waiting

           _ ->  -- `ctrl` is nonempty: unfold `code`
             do let (sup,d) = geneSupDag code ctrl
                localCtrl ctrlR $ mapM_ (\i -> sInstrInit i r d sup) code 
                (_,_,curs,_) <- lookupRSid ctrlR 

                -- add real clients to ctrl and import sids
                let ctrlCl = case lookup ctrl d of 
                               Nothing -> delByKey curs ((sf,r,-2),0)
                               Just cl -> let rs = clRSid cl sf r 
                                              c0 = delByKey curs ((sf,r,-2),0)
                                           in c0 ++ curStart rs
                updateCtx ctrlR (buf, c, ctrlCl, p) 
                
                -- delete pseudoclient and add real clients for import SIds
                insClR <- mapM (getClientR d sf r) ins
                --ins' <- mapM rsidMap [(sf,r,i) | i<-ins]                                  
                mapM_ (delRSClient ((sf,r,-3),0)) [(sf,r,i) | i <- ins] 
                ins' <- mapM rsidMapM [(sf,r,s) | s <- ins]
                zipWithM_ (\cl i -> addRSClient cl i) insClR ins'
                
                -- run `code`
                --localCtrl ctrlR $ mapM_ (sInstrInterp bufSize r) code 


sInstrInterp bufSize r (SCall fid _ retSids) =
  do (SFun _ _ code count) <- lookupFId fid
     sf <- getSF
     let fmSF = sf+1 
         fmR = (r*count) + head retSids
     localSF fmSF $ mapM_ (sInstrInterp bufSize fmR) code  -- function body

  

allEnd :: [((RSId,Int),Int)] -> Int -> Bool 
allEnd bs len = all (\(_,c) -> c >= len) bs 

resetCur :: [((RSId,Int),Int)] -> [((RSId,Int), Int)]
resetCur bs = map (\(rsid, _) -> (rsid, 0)) bs 


doneStreams :: [RSId] -> SvcodeP ()
doneStreams rets = mapM_ doneStream rets

doneStream :: RSId -> SvcodeP ()
doneStream ret = 
  do (_,sups, cs,_) <- lookupRSid ret
     -- delete itself from its suppliers' client list
     zipWithM_ (\s i -> delRSClient (ret,i) s) sups [0..] 
     updateCtx ret (Draining [] True, sups, resetCur cs, Done ())


addRSClient :: [(RSId,Int)] -> RSId -> SvcodeP ()
addRSClient cls s1 = 
  do s1' <- rsidMapM s1 
     (buf1,sup1,cl1,p1) <- lookupRSid s1'
     updateCtx s1' (buf1,sup1, cl1 ++ (curStart cls), p1)


delRSClient :: (RSId,Int) -> RSId -> SvcodeP ()
delRSClient cl s1 = 
  do (buf1,sup1,cl1,p1) <- lookupRSid s1
     updateCtx s1 (buf1,sup1, delByKey cl1 cl , p1)


isDone :: RSId -> SvcodeP Bool
isDone sid = 
  do (buf, _, _, p) <- lookupRSid sid
     case buf of 
       Draining _ True -> if p == Done () then return True else return False
       _ -> return False


markRead :: Clients -> (RSId,Int) -> Int -> Clients
markRead cs sid oldCursor = updateByKey cs sid (oldCursor+1)


-- update the first pair with the key `k` 
updateByKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateByKey [] _ _ = []
updateByKey (p:ps) k v = 
  if fst p == k then (k,v): ps else p:updateByKey ps k v

-- if key `a` doesnt exist, then add such a key and value `b`
addByKey :: (Eq a) => [(a,[b])] -> a -> b -> [(a,[b])]
addByKey [] a b = [(a,[b])]  
addByKey (p0@(k0,v0):ps) k v = 
  if k0 == k then ((k0,v0++[v]): ps) else p0:addByKey ps k v

delByKey :: (Eq a) => [(a,b)] -> a -> [(a,b)]
delByKey [] _ = []
delByKey (p0@(k0,v0):ps) k = 
  if k0 == k then ps else p0:delByKey ps k


checkKeyVal :: (Eq a) => (Eq b) => [(a,b)] -> a -> b -> Bool
checkKeyVal [] _ _ = False
checkKeyVal (p0@(k0,v0):ps) k v = 
  if (k0 == k) && (v == v0) then True else checkKeyVal ps k v 


------- run the program and print out the Svctx of each round  -------------
-- 
runSvcodePExp' :: SFun -> Int -> Int -> FEnv -> Either String String 
runSvcodePExp' (SFun [] retSids code fresh) count bs fe = 
  do let (sup,d) = geneSupDag code 0
         retRSids = zip (s2Rs retSids 0 (head retSids)) [0..] 
         ctrl = (0,head retSids,0)
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d $ zip retSids [0..]
     (_,_,ctx,re) <- rSvcodeP (sInit code (head retSids) d' sup) M.empty ctrl fe 0 M.empty

     (as, ctxs) <- roundN count (round1 (mapM_ (sInstrInterp bs (head retSids)) code) ctrl retRSids fe) 
                    re [ctx] ([map (\_ -> []) retSids]) 
     let str = map (\(a,c,i) -> "Round "++ show i ++"\nOutput:" ++ show a ++ "\n" 
                      ++ showCtx (M.toList c) ++ "\n") 
               $ zip3 (reverse as) (reverse ctxs) [0..]
     return $ concat str


showCtx :: [(RSId,Process)] -> String
showCtx [] = ""
showCtx (((sf,r,sid),(buf,c,bs,p)):ss) = 
  "(" ++ show sf ++ "," ++ show r ++ ", S" ++ show sid  ++ "), (" 
    ++ show buf ++ "," ++ show c ++ "," ++ show bs 
    ++ "," ++ show p ++ ")\n" ++ showCtx ss 
    


roundN :: Int -> (REnv -> Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx,REnv))
            -> REnv -> [Svctx] -> [[[AVal]]]  -> Either String ([[[AVal]]], [Svctx])
roundN 0 f _ ctxs as = Right (as,ctxs) 
roundN c f re ctxs as = 
  if M.foldl (\b (_,_, _,p) -> b && (p == Done ())) True (head ctxs)
    then return (as,ctxs)
    else
      do (as',ctx',re') <- f re (head ctxs) (head as)
         if (length ctxs > 0) && (compCtx ctx' $ ctxs!!0)
         then do unlockCtx <- stealing ctx'
                 roundN (c-1) f re' (unlockCtx:ctxs) (as':as)
         else roundN (c-1) f re' (ctx':ctxs) (as':as)
     

round1 :: SvcodeP () -> RSId -> [(RSId,Int)] -> FEnv -> REnv -> Svctx -> [[AVal]] 
           -> Either String ([[AVal]], Svctx,REnv) 
round1 m ctrl st fe re ctx as0 = 
  do (_,_, ctx',re') <- rSvcodeP m ctx ctrl fe 0 re
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                            do (a,c) <- lookupAval c0 s re'
                               return (a:a0,c))
                        ([],ctx') st 
     let as' = zipWith (++) as0 (reverse as) 
     return (as', ctx'',re') 

-----------------------------------}


------ helper functions for generating the supplier table and DAG  ------

geneSupDag :: [SInstr] -> SId -> (Sup,Dag)
geneSupDag code c = 
  let sup = geneSup code c
      dag = geneDagFromSup sup
   in (sup, dag)


geneSup :: [SInstr] -> SId -> Sup
geneSup code ctrl = 
  let (sids,_,chs) = unzip3 $ instrGeneSup code ctrl
   in zip sids chs   
 

instrGeneSup :: [SInstr] -> SId -> [(SId, String,[SId])]
instrGeneSup [] _ = []
instrGeneSup ((SDef sid sexp):ss') c = (sid,i,cl0) : instrGeneSup ss' c
    where (cl0, i, _) = getSupExp sexp c

instrGeneSup ((WithCtrl newc _ ss _):ss') c = instrGeneSup ss' c

instrGeneSup ((SCall f args rets):ss') c = instrGeneSup ss' c 
 

geneDagFromSup :: Sup -> Dag
geneDagFromSup sup = 
  let (ss1,ss2) = unzip sup 
      allSids = toList $ fromList $ ss1 ++ (concat ss2) 
      d0 = map (\sid -> (sid,[])) allSids
   in foldl (\d (sid, sups) -> 
              foldl (\d' (s,i) -> addByKey d' s (sid,i) ) d $ zip sups [0..]
            ) d0 sup
  

-------- generate a file to visualize the DAG -------------
--- use "graph-easy": graph-easy <inputfile> --png 
geneDagFile (SFun _ ret code fresh) fname = 
  do let ch1 = instrGeneSup code 0
         ch2 = map (\(i, str,sids) -> 
                        ("S"++ show i ++ ": " ++ str, sids)) ch1
         ch3 = map (\(str, sids) -> (str, map (\i -> fst $ ch2!!i) sids)) ch2
         lines = map (\(x,ys) -> if null ys then drawnode x else          
                  concat $ zipWith (\y c -> drawedge y x c) ys [0..]) ch3         

         retSids = map (\sid -> fst $ ch2!!sid) ret 
         retLines = map (\(c,s) -> drawedge s "S-1: Output" c) 
                          $ zip [0..] retSids
         content = concat $ lines ++ retLines  
     writeFile fname content
   

drawnode :: String -> String
drawnode i = "[ " ++  i ++ " ]\n"

drawedge :: String -> String -> Int -> String 
drawedge i j c = "[ " ++ i ++ " ] -- " ++ show c ++ " --> [ " ++ j ++ " ]\n"

-----------------

