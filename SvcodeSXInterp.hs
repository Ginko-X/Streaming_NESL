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



newtype SvcodeP a = SvcodeP {rSvcodeP :: Svctx -> RSId -> FEnv -> Int-> 
                                Either String (a,(Int,Int),Svctx)}
                                  
instance  Monad SvcodeP where
    return a = SvcodeP $ \ctx ctrl fe sf -> Right (a,(0,0),ctx) 

    m >>= f = SvcodeP $ \ctx ctrl fe sf -> 
        case rSvcodeP m ctx ctrl fe sf of 
            Right (a,(w,s),ctx') -> 
                case rSvcodeP (f a) ctx' ctrl fe sf of
                    Right (b,(w',s'),ctx'') -> Right (b,(w+w',s+s'),ctx'')
                    Left err' -> Left err'
            Left err -> Left err 

    fail err = SvcodeP $ \ _ _ _ _ -> Left $ "SVCODE runtime error: " ++ err


instance Functor SvcodeP where
  fmap f t = t >>= return . f 

instance Applicative SvcodeP where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



runSvcodePExp :: SFun -> Int -> Bool -> FEnv -> Either String (SvVal, (Int,Int))
runSvcodePExp (SFun [] st code fresh) bs _ fe = 
  do let (sup,d) = geneSupDag code 0
         retSids = tree2Sids st 
         retRSids = [((0,head retSids,r),i) | (r,i) <- zip retSids [0..]]
         ctrl = (0,head retSids,0) 
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d 
                      $ zip retSids [0..]
     (_,_, ctx) <- rSvcodeP (sInit code (head retSids) d' sup) M.empty ctrl fe 0
     (as,(w1,s1), _) <- rrobin (mapM_ (sInstrInterp bs (head retSids)) code) 
                           ctx retRSids (map (\_ -> []) retSids) (0,0) fe
     return (fst $ constrSv st as,(w1,s1))


addClient :: Dag -> SId -> (SId,Int) -> Dag
addClient d i c = 
  case lookup i d of 
    Nothing -> addByKey d i c
    Just cl -> updateByKey d i $ cl ++ [c] 


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


-- scheduling  
rrobin :: SvcodeP () -> Svctx -> [(RSId,Int)] -> [[AVal]] -> (Int,Int)
           -> FEnv -> Either String ([[AVal]],(Int,Int),Svctx)           
rrobin m ctx retRSids as0 (w0,s0) fe = 
  do let ctrl = replaceSid (fst $ head retRSids) 0
     (_,(w1,s1), ctx') <- rSvcodeP m ctx ctrl fe 0
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                             do (a,c) <- lookupAval c0 s
                                return (a:a0,c)) 
                         ([],ctx') retRSids 
     let as' = zipWith (++) as0 (reverse as)
         work = w0+w1
     if M.foldl (\b (_,_,_,p) -> b && (p == Done ())) True ctx'' 
       then return (as',(work, s0+s1) , ctx'') 
       else if compCtx ctx ctx'' 
            then do unlockCtx <- stealing ctx''  -- try stealing
                    rrobin m unlockCtx retRSids as' (work,s0+s1+1) fe
            else rrobin m ctx'' retRSids as' (work,s0+s1) fe


replaceSid :: RSId -> SId -> RSId
replaceSid (sf,r,_) sid = (sf,r,sid)

-- ??
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


lookupAval :: Svctx -> (RSId, Int) -> Either String ([AVal],Svctx)
lookupAval ctx (s@(sf,r,sid),i) = 
  case M.lookup s ctx of 
      Nothing -> Left  $ "lookupAval: undefined stream " ++ show s  
      Just (Draining a b, c, bs, p) -> 
        if checkKeyVal bs ((sf,r,-1),i) 0
        then let bs' = updateByKey bs ((sf,r,-1),i) (length a)  
                 ctx' = M.adjust (\_ -> (Draining a b, c, bs', p)) s ctx 
             in Right (a, ctx')
        else Right ([],ctx)
      
      Just (Filling _, _,_, _) -> Right ([], ctx)



addCtx :: RSId -> Process -> SvcodeP ()
addCtx s v = SvcodeP $ \ c _ _ _ -> Right ((), (0,0), M.insert s v c)

addCtxChk :: RSId -> Process -> SvcodeP ()
addCtxChk s v = 
  do ctx <- getCtx
     case M.lookup s ctx of 
       Nothing -> addCtx s v 
       Just _ -> return ()


updateCtx :: RSId -> Process -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ _ _ -> Right ((),(0,0),M.adjust (\_ -> v) s c) 

getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ c _ _ _ -> Right (c,(0,0),c)


getSF = SvcodeP $ \ c _ _ sf -> Right (sf, (0,0),c)

localSF :: Int -> SvcodeP a -> SvcodeP a 
localSF sf m = SvcodeP $ \ ctx ctrl fe _ -> rSvcodeP m ctx ctrl fe sf 


getCtrl :: SvcodeP RSId
getCtrl = SvcodeP $ \ ctx ctrl _ _ -> Right (ctrl,(0,0),ctx)


getSuppiler sid = 
  do (_,sup,_,_) <- lookupRSid sid 
     return sup 


localCtrl :: RSId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ ctx _ fe sf -> rSvcodeP m ctx ctrl fe sf 


lookupRSid :: RSId -> SvcodeP Process
lookupRSid r = SvcodeP $ \c _ _ _ -> 
    case M.lookup r c of 
        Nothing -> Left $ "lookupRSid: undefined SId " ++ show r
        Just st -> Right (st,(0,0),c)  


lookupFId :: FId -> SvcodeP SFun
lookupFId fid = SvcodeP $ \c _ fe _ -> 
    case lookup fid fe of 
        Nothing -> Left $ "lookupFId: undefined FId " ++ show fid
        Just sf -> Right (sf,(0,0),c)  


costInc :: (Int, Int) -> SvcodeP ()
costInc (w,s) = SvcodeP $ \ c _ _ sf -> Right ((), (w,s), c)


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
    Just cls -> return $ clRSid cls sf r 


getSupR :: Sup -> Int -> Int  -> SId -> SvcodeP [RSId]
getSupR d sf r sid = 
  case lookup sid d of
    Nothing -> return [] 
    Just sups -> return $ s2Rs sups sf r 


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


sInstrInit (WithCtrl ctrl ins code st) r d sup =
  do sf <- getSF
     let ctrlR = (sf,r,ctrl)
     (buf,s,cl,p) <- lookupRSid ctrlR
     updateCtx ctrlR (buf,s,(((sf,r,-2),0),0):cl,p) -- add a first-chunk flag
     
     -- initial the return sids
     let retSids = tree2Sids st 
         retRSids = s2Rs retSids sf r
     retClsR <- mapM (getClientR d sf r) retSids     
     zipWithM_ (\s cl -> addCtxChk s (Filling [], [], curStart cl, Done ()))
       retRSids retClsR  -- `Done ()` will be replaced when unfolding WithCtrl

     mapM_ (addRSClient [((sf,r,-3),0)]) (s2Rs ins sf r) -- add pseudoclient


sInstrInit (SCall fid argSid retSids) r d _ = 
  do (SFun fmArgs st code count) <- lookupFId fid
     let fmRets = tree2Sids st      
         (fmSup,fmDag) = geneSupDag code 0 

     sf <- getSF
     let fmSf = sf +1 
         fmR = (r*count) + (head retSids)
         fmCtrlR = (fmSf,fmR,0) 
     
     let argSidR = s2Rs argSid sf r -- actual parameters
         fmArgsR = s2Rs fmArgs fmSf fmR -- formal parameters
         fmRetsR = s2Rs fmRets fmSf fmR
     fmArgsClsR <- mapM (getClientR fmDag fmSf fmR) (0:fmArgs)
     
     zipWithM_ (\a cl -> addCtxChk a (Filling [], [], curStart cl, rinOut)) 
       (fmCtrlR:fmArgsR) fmArgsClsR
     
     localCtrl fmCtrlR $ localSF fmSf $ sInit code fmR fmDag fmSup --unfolding

     let retRSids = s2Rs retSids sf r
     retClsR <- mapM (getClientR d sf r) retSids
     zipWithM_ (\s cl -> addCtxChk s (Filling [], [], curStart cl, rinOut))
       retRSids retClsR  
     
     ctrl <- getCtrl
     connectRSIds (ctrl:argSidR) (fmCtrlR:fmArgsR)
     connectRSIds fmRetsR retRSids

     
-- pipe [ s1 ] --- 0 --> [ s2 ]
-- s1 is s2's only supplier
connectRSIds :: [RSId] -> [RSId] -> SvcodeP ()
connectRSIds [] [] = return ()
connectRSIds (s1:s1s) (s2:s2s) = 
  do (buf1,sup1,cl1,p1) <- lookupRSid s1
     (buf2,sup2,cl2,p2) <- lookupRSid s2  
     updateCtx s1 (buf1,sup1,cl1 ++ [((s2,0),0)],p1)
     updateCtx s2 (buf2,[s1],cl2,rinOut) 
     connectRSIds s1s s2s 
connectRSIds _ _ = fail $ "connectRSIds: RSId lengths mismatch."


---- SExpression init
sExpXducerInit :: SExp -> SvcodeP (Xducer ())
sExpXducerInit Ctrl = return $ rout (BVal False)
sExpXducerInit EmptyCtrl = return $ Done () 
sExpXducerInit (Const a) = return (mapConst a)
sExpXducerInit (MapConst _ a) = return (mapConst a)
sExpXducerInit (Usum _) = return usumXducer
sExpXducerInit (ToFlags _) = return toFlags 

sExpXducerInit (MapTwo op _ _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapTwo fop)

sExpXducerInit (MapOne op _) = 
  do fop <- lookupOpA op opAEnv0
     return (mapOne fop)

sExpXducerInit (Pack _ _) = return packXducer
sExpXducerInit (UPack _ _) = return upackXducer
sExpXducerInit (Distr _ _) = return pdistXducer
sExpXducerInit (SegDistr _ _ ) = return segDistrXducer
sExpXducerInit (SegFlagDistr _ _ _) = return segFlagDistrXducer
sExpXducerInit (PrimSegFlagDistr _ _ _) = return primSegFlagDistrXducer
sExpXducerInit (B2u _) = return b2uXducer
sExpXducerInit (SegscanPlus _ _) = return segScanPlusXducer 
sExpXducerInit (ReducePlus _ _) = return segReducePlusXducer
sExpXducerInit (SegConcat _ _) = return segConcatXducer
sExpXducerInit (USegCount _ _) = return uSegCountXducer
sExpXducerInit (InterMergeS ss) = return (interMergeXducer $ length ss)

sExpXducerInit (SegInterS ss) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
  in return $ segInterXducer chs 

sExpXducerInit (PriSegInterS ss) = 
  let chs = zipWith (\_ x -> (x*2,x*2+1)) ss [0..] 
  in return $ priSegInterXducer chs 

sExpXducerInit (SegMerge _ _) = return segMergeXducer 
sExpXducerInit (Check _ _) = return checkXducer
sExpXducerInit (IsEmpty _) = return isEmptyXducer


lookupOpA :: OP -> OpAEnv -> SvcodeP ([AVal] -> AVal)
lookupOpA op r = 
  do case lookup op r of
       Just (v,_) -> return v 
       Nothing -> fail $ "lookupOpA: undefined operation " ++ show op 


----- run a process
sSIdInterp :: Int -> RSId -> SvcodeP ()
sSIdInterp bufSize sid = 
  do (buf, sups, bs, p) <- lookupRSid sid 
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
             do (bufSup, supSup, flagSup,pSup) <- lookupRSid (sups!!i)
                case lookup (sid,i) flagSup of 
                  Nothing -> fail $ show sid ++ " reads an undefined supplier " 
                                      ++ show (sups!!i)
                  Just cursor -> 
                    case bufSup of                
                      Filling _ -> return () -- read unavailable, blocking 
                     
                      Draining asSup False ->   -- normal read
                        if cursor >= length asSup
                          then return () -- read blocking 
                          else -- read successfully
                            do let flagSup' = markRead flagSup (sid,i) cursor
                               updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
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
                               updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
                               updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor))
                               costInc (1,0)  
                               sSIdInterp bufSize sid -- loop


-- interpret an instruction
sInstrInterp :: Int -> SId -> SInstr -> SvcodeP ()
sInstrInterp bufSize r def@(SDef sid _) = 
  do sf <- getSF
     sSIdInterp bufSize (sf,r,sid) 


sInstrInterp bufSize r (WithCtrl ctrl ins code st) = 
  do sf <- getSF
     let ctrlR = (sf,r,ctrl)
         retSids = tree2Sids st 
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
                         mapM_ (delRSClient ((sf,r,-3),0)) [(sf,r,i) | i<-ins]
           
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
                mapM_ (delRSClient ((sf,r,-3),0)) [(sf,r,i) | i <- ins]  
                zipWithM_ (\cl i -> addRSClient cl i) insClR [(sf,r,s) | s <- ins] 
                
                -- run `code`
                localCtrl ctrlR $ mapM_ (sInstrInterp bufSize r) code 


sInstrInterp bufSize r (SCall fid _ retSids) =
  do (SFun fmArgs st code count) <- lookupFId fid
     sf <- getSF
     let fmSF = sf+1 
         fmR = (r*count) + head retSids
         fmArgsR = s2Rs (0:fmArgs) fmSF fmR 
         fmCtrl = (fmSF,fmR,0)
     localCtrl fmCtrl $ localSF fmSF $ mapM (sSIdInterp bufSize) fmArgsR
     localCtrl fmCtrl $ localSF fmSF $ mapM (sInstrInterp bufSize fmR) code    
     let retRSids = s2Rs retSids sf r 
     mapM_ (sSIdInterp bufSize) retRSids



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
  do (buf1,sup1,cl1,p1) <- lookupRSid s1
     updateCtx s1 (buf1,sup1, cl1 ++ (curStart cls), p1)


delRSClient :: (RSId,Int) -> RSId -> SvcodeP ()
delRSClient cl s1 = 
  do (buf1,sup1,cl1,p1) <- lookupRSid s1
     updateCtx s1 (buf1,sup1, delByKey cl1 cl , p1)


isDone :: RSId -> SvcodeP Bool
isDone sid = 
  do (_, _, _, p) <- lookupRSid sid
     case p of 
       Done () -> return True
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
runSvcodePExp' (SFun [] st code fresh) count bs fe = 
  do let (sup,d) = geneSupDag code 0
         retSids = tree2Sids st
         retRSids = zip (s2Rs retSids 0 (head retSids)) [0..] 
         ctrl = (0,head retSids,0)
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d $ zip retSids [0..]
     (_,_,ctx) <- rSvcodeP (sInit code (head retSids) d' sup) M.empty ctrl fe 0

     (as, ctxs) <- roundN count (round1 (mapM_ (sInstrInterp bs (head retSids)) code) ctrl retRSids fe) [ctx]
                     ([map (\_ -> []) retSids]) 
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
    


roundN :: Int -> (Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx)) -> 
            [Svctx] -> [[[AVal]]]  -> Either String ([[[AVal]]], [Svctx])
roundN 0 f ctxs as = Right (as,ctxs) 
roundN c f ctxs as = 
  if M.foldl (\b (_,_, _,p) -> b && (p == Done ())) True (head ctxs)
    then return (as,ctxs)
    else
      do (as',ctx') <- f (head ctxs) (head as)
         if (length ctxs > 0) && (compCtx ctx' $ ctxs!!0)
         then do unlockCtx <- stealing ctx'
                 roundN (c-1) f (unlockCtx:ctxs) (as':as)
         else roundN (c-1) f (ctx':ctxs) (as':as)
     

round1 :: SvcodeP () -> RSId -> [(RSId,Int)] -> FEnv -> Svctx -> [[AVal]] 
           -> Either String ([[AVal]], Svctx) 
round1 m ctrl st fe ctx as0 = 
  do (_,_, ctx') <- rSvcodeP m ctx ctrl fe 0
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                            do (a,c) <- lookupAval c0 s
                               return (a:a0,c))
                        ([],ctx') st 
     let as' = zipWith (++) as0 (reverse as) 
     return (as', ctx'') 

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
    where (cl0, i) = getSupExp sexp c

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

         retSids = map (\sid -> fst $ ch2!!sid) $ tree2Sids ret 
         retLines = map (\(c,s) -> drawedge s "S-1: Output" c) 
                          $ zip [0..] retSids
         content = concat $ lines ++ retLines  
     writeFile fname content
   

drawnode :: String -> String
drawnode i = "[ " ++  i ++ " ]\n"

drawedge :: String -> String -> Int -> String 
drawedge i j c = "[ " ++ i ++ " ] -- " ++ show c ++ " --> [ " ++ j ++ " ]\n"

-----------------

