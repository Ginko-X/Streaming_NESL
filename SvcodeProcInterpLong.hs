{- SVCODE Streaming Interpreter with arbitrary buffer size -}

module SvcodeProcInterpLong where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc
import SneslCompiler (tree2Sids)

import Control.Monad
import Data.Set (fromList, toList)


type RSId = (Int,SId,SId)

type Svctx = [(RSId, SState)]

type SState = (BufState, Suppliers, Clients, Proc ())

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
         retRSids = zip (zip3 (repeat 0) (repeat $ head retSids) retSids) [0..]         
         ctrl = (0,head retSids,0) 
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d 
                      $ zip retSids [0..]
     (_,(w0,s0), ctx) <- rSvcodeP (sInit code (head retSids) d' sup) [] ctrl fe 0
     (as,(w1,s1), _) <- rrobin (mapM_ (sInstrInterp bs (head retSids)) code) 
                           ctx retRSids (map (\_ -> []) retSids) (w0,s0) fe
     return (fst $ constrSv st as,(w0+w1,s0+s1))


addClient :: Dag -> SId -> (SId,Int) -> Dag
addClient d i c = 
  case lookup i d of 
    Nothing -> addWithKey d i c
    Just cl -> updateWithKey d i $ cl ++ [c] 


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
     (res,_,_) <- rSvcodeP (mapM (\(s,_) -> isDone s) retRSids) ctx'' ctrl fe 0
     if all (\x -> x) res 
       then return (as', (w0+w1,s0+s1), ctx'') 
       else if equalCtx ctx ctx'' 
            then do unlockCtx <- stealing ctx''  -- try stealing
                    rrobin m unlockCtx retRSids as' (w0+w1,s0+s1) fe
            else rrobin m ctx'' retRSids as' (w0+w1,s0+s1) fe


replaceSid :: RSId -> SId -> RSId
replaceSid (sf,r,_) sid = (sf,r,sid)


-- pick out the first non-empty stream in Filling mode to drain
stealing :: Svctx -> Either String Svctx
stealing [] = Left "Deadlock!"

stealing ((sid, (Filling as@(a0:_), sup, bs, p)):ss) = 
  Right $ (sid, (Draining as False, sup,bs,p)) : ss 

stealing (s:ss) = stealing ss >>= (\ss' -> return (s:ss'))


-- not 100% correct because Procs are not comparable
equalCtx :: Svctx -> Svctx -> Bool
equalCtx [] [] = True
equalCtx ((s1,(buf1,c1,bs1,p1)):ctx1) ((s2,(buf2,c2,bs2,p2)):ctx2) =         
  if (s1 == s2) && (buf1 == buf2) && (c1 == c2) && (bs1 == bs2) 
       && (compProc p1 p2)
    then equalCtx ctx1 ctx2 
    else False 

compProc :: Proc () -> Proc () -> Bool
compProc (Pin i1 _) (Pin i2 _) = i1 == i2 
compProc (Pout a p1) (Pout b p2) = (a == b) && (compProc p1 p2)
compProc (Done a) (Done b) = a == b 
compProc _ _ = False


lookupAval :: Svctx -> (RSId, Int) -> Either String ([AVal],Svctx)
lookupAval ctx (s@(sf,r,sid),i) = 
  case lookup s ctx of 
      Nothing -> Left  $ "lookupAval: undefined stream " ++ show s  
      Just (Draining a b, c, bs, p) -> 
        if checkWithKey bs ((sf,r,-1),i) 0
        then let bs' = updateWithKey bs ((sf,r,-1),i) (length a)  
                 ctx' = updateWithKey ctx s (Draining a b, c, bs', p)
             in Right (a, ctx')
        else Right ([],ctx)
      
      Just (Filling _, _,_, _) -> Right ([], ctx)



addCtx :: RSId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ c _ _ _ -> Right ((), (0,0),c++[(s,v)])

addCtxChk :: RSId -> SState -> SvcodeP ()
addCtxChk s v = 
  do ctx <- getCtx
     case lookup s ctx of 
       Nothing -> addCtx s v 
       Just _ -> return ()


updateCtx :: RSId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ _ _ -> Right ((),(0,0),updateWithKey c s v) 

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


lookupRSid :: RSId -> SvcodeP SState
lookupRSid r = SvcodeP $ \c _ _ _ -> 
    case lookup r c of 
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
s2Rs sids sf sid = zip3 (repeat sf) (repeat sid) sids

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
sInit code r d sup = 
  mapM_ (\i -> sInstrInit i r d sup) code


sInstrInit :: SInstr -> SId -> Dag -> Sup -> SvcodeP ()
sInstrInit (SDef sid e) r d sup = 
  do sf <- getSF
     clR <- getClientR d sf r sid 
     supR <- getSupR sup sf r sid     
     p <- sExpProcInit e  
     ctx <- getCtx
     case lookup (sf,r,sid) ctx of 
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
sExpProcInit :: SExp -> SvcodeP (Proc ())
sExpProcInit Ctrl = return $ rout (BVal False)
sExpProcInit EmptyCtrl = return $ Done () 
sExpProcInit (Const a) = return (mapConst a)
sExpProcInit (MapConst _ a) = return (mapConst a)
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
sExpProcInit (Check _ _) = return checkProc
sExpProcInit (IsEmpty _) = return isEmptyProc


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
       Nothing -> -- the 1st output of ctrl has already been read
         do bs <- mapM isDone [(sf,r,s) | s <- retSids]
            if foldl (&&) True bs
              then return ()
              else localCtrl ctrlR $ mapM_ (sInstrInterp bufSize r) code

       Just 0 ->  
         case buf of 
           Draining [] True ->  -- `ctrl` is empty
             do let retRSids = s2Rs retSids sf r 
                doneStreams retRSids
                updateCtx ctrlR (buf, c, delWithKey curs ((sf,r,-2),0), p)
                -- delete pseudoclient 
                mapM_ (delRSClient ((sf,r,-3),0)) [(sf,r,i) | i <- ins]  
           
           Filling [] ->  return () -- keep waiting

           _ ->  -- `ctrl` is nonempty:  unfold `code`
             do let (sup,d) = geneSupDag code ctrl
                localCtrl ctrlR $ mapM_ (\i -> sInstrInit i r d sup) code 
                (_,_,curs,_) <- lookupRSid ctrlR 

                -- add real clients to ctrl and import sids
                let ctrlCl = case lookup ctrl d of 
                               Nothing -> delWithKey curs ((sf,r,-2),0)
                               Just cl -> let rs = clRSid cl sf r 
                                              c0 = delWithKey curs ((sf,r,-2),0)
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
     updateCtx s1 (buf1,sup1, delWithKey cl1 cl , p1)


isDone :: RSId -> SvcodeP Bool
isDone sid = 
  do (_, _, _, p) <- lookupRSid sid
     case p of 
       Done () -> return True
       _ -> return False


markRead :: Clients -> (RSId,Int) -> Int -> Clients
markRead cs sid oldCursor = updateWithKey cs sid (oldCursor+1)


-- update the first pair with the key `k` 
updateWithKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateWithKey [] _ _ = []
updateWithKey (p:ps) k v = 
  if fst p == k then (k,v): ps else p:updateWithKey ps k v

-- if key `a` doesnt exist, then and such a key and value `b`
addWithKey :: (Eq a) => [(a,[b])] -> a -> b -> [(a,[b])]
addWithKey [] a b = [(a,[b])]  
addWithKey (p0@(k0,v0):ps) k v = 
  if k0 == k then ((k0,v0++[v]): ps) else p0:addWithKey ps k v

delWithKey :: (Eq a) => [(a,b)] -> a -> [(a,b)]
delWithKey [] _ = []
delWithKey (p0@(k0,v0):ps) k = 
  if k0 == k then ps else p0:delWithKey ps k


checkWithKey :: (Eq a) => (Eq b) => [(a,b)] -> a -> b -> Bool
checkWithKey [] _ _ = False
checkWithKey (p0@(k0,v0):ps) k v = 
  if (k0 == k) && (v == v0) then True else checkWithKey ps k v 


------- run the program and print out the Svctx of each round  -------------
-- 
runSvcodePExp' :: SFun -> Int -> Int -> FEnv -> Either String String
runSvcodePExp' (SFun [] st code fresh) count bs fe = 
  do let (sup,d) = geneSupDag code 0
         retSids = tree2Sids st
         retRSids = zip (s2Rs retSids 0 (head retSids)) [0..] 
         ctrl = (0,head retSids,0)
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d $ zip retSids [0..]
     (_,_,ctx) <- rSvcodeP (sInit code (head retSids) d' sup) [] ctrl fe 0

     (as, ctxs) <- roundN count (round1 (mapM_ (sInstrInterp bs (head retSids)) code) ctrl retRSids fe) [ctx]
                     ([map (\_ -> []) retSids]) 
     let str = map (\(a,c,i) -> "Round "++ show i ++"\nOutput:" ++ show a ++ "\n" 
                      ++ showCtx c ++ "\n") 
               $ zip3 (reverse as) (reverse ctxs) [0..]
     return $ concat str


showCtx :: Svctx -> String
showCtx [] = ""
showCtx (((sf,r,sid),(buf,c,bs,p)):ss) = 
  "(" ++ show sf ++ "," ++ show r ++ ", S" ++ show sid  ++ "), (" 
    ++ show buf ++ "," ++ show c ++ "," ++ show bs 
    ++ "," ++ show p ++ ")\n" ++ showCtx ss 
    


roundN :: Int -> (Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx)) -> 
            [Svctx] -> [[[AVal]]]  -> Either String ([[[AVal]]], [Svctx])
roundN 0 f ctxs as = Right (as,ctxs) 
roundN c f ctxs as = 
  if all (\(_,(_,_, _,p)) -> p == Done ()) (head ctxs)  
    then return (as,ctxs)
    else
      do (as',ctx') <- f (head ctxs) (head as)
         if (length ctxs > 0) && (equalCtx ctx' $ ctxs!!0)
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
              foldl (\d' (s,i) -> addWithKey d' s (sid,i) ) d $ zip sups [0..]
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

