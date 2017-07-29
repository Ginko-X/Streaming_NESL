{- SVCODE Streaming Interpreter with arbitrary buffer size -}

module SvcodeProcInterpLong where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc
import SneslCompiler (tree2Sids)
import SneslInterp (wrapWork)

import Control.Monad
import Data.Set (fromList, toList)
import Data.List (sort)



type Svctx = [(SId, SState)]

type SState = (BufState, Suppliers, Clients, Proc ())

data BufState = Filling [AVal] 
              | Draining [AVal]
              | Eos deriving (Show, Eq) 

type Suppliers = [SId]  -- incoming edges
type Clients  = [((SId,Int), Int)] --outgoing edges


type Sup = [[SId]]  -- supplier list
type Dag = [[(SId,Int)]] -- client list


newtype SvcodeP a = SvcodeP {rSvcodeP :: Svctx -> SId -> FEnv -> SId -> 
                                Either String (a,(Int,Int),Svctx,SId)}
                                  
instance  Monad SvcodeP where
    return a = SvcodeP $ \ctx ctrl fe fr -> Right (a,(0,0),ctx,fr) 

    m >>= f = SvcodeP $ \ctx ctrl fe fr -> 
        case rSvcodeP m ctx ctrl fe fr of 
            Right (a,(w,s),ctx',fr') -> 
                case rSvcodeP (f a) ctx' ctrl fe fr' of
                    Right (b,(w',s'),ctx'',fr'') -> Right (b,(w+w',s+s'),ctx'',fr'')
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
  do let (sup,d) = geneSupDag code 0 fresh
         retSids = zip (tree2Sids st) [0..]
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids 
     (_,(w0,s0), ctx,_) <- rSvcodeP (sInit code d' sup) [] 0 fe fresh
     (as,(w1,s1), _) <- rrobin (mapM (sInstrInterp bs) code) ctx 0 retSids 
                    (map (\_ -> []) retSids) (w0,s0) fe fresh
     return (fst $ constrSv st as,(w0+w1,s0+s1))



addClient :: Dag -> SId -> (SId,Int) -> Dag
addClient d i c = 
  let cs0 = d !! i 
      cs = cs0 ++ [c]
  in updateList d i cs 


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


     
rrobin :: SvcodeP [Bool] -> Svctx -> SId -> [(SId,Int)] -> [[AVal]] -> (Int,Int)
           -> FEnv -> SId -> Either String ([[AVal]],(Int,Int),Svctx)           
rrobin m ctx ctrl retSids as0 (w0,s0) fe fr = 
  do (bs,(w1,s1), ctx',fr') <- rSvcodeP m ctx ctrl fe fr
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                             do (a,c) <- lookupAval c0 s
                                return (a:a0,c)) 
                         ([],ctx') retSids 
     let as' = zipWith (++) as0 (reverse as) 
     if all (\x -> x) bs 
       then return (as', (w0+w1,s0+s1), ctx'') 
       else if equalCtx ctx ctx'' 
            then do unlockCtx <- stealing ctx''  -- try stealing
                    rrobin m unlockCtx ctrl retSids as' (w0+w1,s0+s1) fe fr'
            else rrobin m ctx'' ctrl retSids as' (w0+w1,s0+s1) fe fr'


---- 
--rewriteSId :: SFun -> SId -> SFun
--rewriteSId (SFun argSid st code fresh) sid0 = 
--  let argSid' = map (+sid0) argSid
--      st' = rewriteSTree st sid0
--      code' = rewriteCode sid0 
--      fresh' = sid0 + fresh
--  in SFun argSid' st' code' fresh'

--rewriteSTree :: STree -> SId -> STree
--rewriteSTree (IStr s) s0 = IStr (s + s0)
--rewriteSTree (BStr s) s0 = BStr (s + s0)
--rewriteSTree (PStr s1 s2) s0 = PStr (rewriteSTree s1 s0) (rewriteSTree s2 s0)
--rewriteSTree (SStr s1 s2) s0 = SStr (rewriteSTree s1 s0) (s2 + s0)


--rewriteCode :: [SInstr] -> SId -> [SInstr]
--rewriteCode is s0 = map (rewriteSInstr s0) is 

--rewriteSInstr :: SId -> SInstr -> SInstr
--rewriteSInstr s0 (SDef sid e) = SDef (sid + s0) e 
--rewriteSInstr s0 (WithCtrl ctrl code st) = WithCtrl (ctrl + s0) code' st'
--  where code' = rewriteCode code s0 
--        st' = rewriteSTree st s0

--rewriteSInstr s0 (SCall fid argSid retSids) = 
--  SCall fid (map (+s0) argSid) (map (+s0) retSids)


-- pick the first non-empty stream in Filling mode to drain
stealing :: Svctx -> Either String Svctx
stealing [] = Left "Deadlock!"

stealing ((sid, (Filling as@(a0:_), sup, bs, p)):ss) = 
  Right $ (sid, (Draining as, sup,bs,p)) : ss 

stealing (s:ss) = stealing ss >>= (\ss' -> return (s:ss'))


-- not 100% correct because Procs are not comparable
equalCtx :: Svctx -> Svctx -> Bool
equalCtx [] [] = True
equalCtx ((s1,(buf1,c1,bs1,p1)):ctx1) ((s2,(buf2,c2,bs2,p2)):ctx2) =         
  if (s1 == s2) && (buf1 == buf2) && (c1 == c2) && (bs1 == bs2) && (compProc p1 p2)
    then equalCtx ctx1 ctx2 
    else False 

compProc :: Proc () -> Proc () -> Bool
compProc (Pin i1 _) (Pin i2 _) = i1 == i2 
compProc (Pout a p1) (Pout b p2) = (a == b) && (compProc p1 p2)
compProc (Done a) (Done b) = a == b 
compProc _ _ = False



lookupAval :: Svctx -> (SId, Int) -> Either String ([AVal],Svctx)
lookupAval ctx (s,i) = 
  case lookup s ctx of 
      Nothing -> Left  $ "lookupAval: undefined stream " ++ show s  
      Just (Draining a, c, bs, p) -> 
        if checkWithKey bs (-1,i) 0
        then let bs' = updateWithKey bs (-1,i) (length a)  
                 ctx' = updateWithKey ctx s (Draining a, c, bs', p)
             in Right (a, ctx')
        else Right ([],ctx)
      
      Just (Filling _, _,_, _) -> Right ([], ctx)

      Just (Eos, _, _, _) -> Right ([], ctx)



addCtx :: SId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ c _ _ fr -> Right ((), (0,0),c++[(s,v)],fr)

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ _ fr -> Right ((),(0,0),updateWithKey c s v,fr) 

getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ c _ _ fr -> Right (c,(0,0),c,fr)

setCtx :: Svctx -> SvcodeP () 
setCtx c = SvcodeP $ \ _ _ _ fr -> Right ((),(0,0),c,fr)


getFresh = SvcodeP $ \ c _ _ fr -> Right (fr, (0,0),c,fr)

setFresh fr = SvcodeP $ \ c _ _ _ -> Right ((),(0,0),c,fr)


getCtrl :: SvcodeP SId
getCtrl = SvcodeP $ \ ctx ctrl _ fr -> Right (ctrl,(0,0),ctx, fr)


getSuppiler sid = 
  do (_,sup,_,_) <- lookupSid sid 
     return sup 


localCtrl :: SId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ ctx _  -> rSvcodeP m ctx ctrl


lookupSid :: SId -> SvcodeP SState
lookupSid s = SvcodeP $ \c ctrl _ fr -> 
    case lookup s c of 
        Nothing -> Left $ "lookupSid: undefined SId " ++ show s  
        Just st -> Right (st,(0,0),c,fr)  


lookupFId :: FId -> SvcodeP SFun
lookupFId fid = SvcodeP $ \c _ fe fr -> 
    case lookup fid fe of 
        Nothing -> Left $ "lookupFId: undefined FId " ++ show fid
        Just sf -> Right (sf,(0,0),c,fr)  


costInc :: (Int, Int) -> SvcodeP ()
costInc (w,s) = SvcodeP $ \ c _ _ fr -> Right ((), (w,s), c, fr)


returnC :: (Int, Int) -> a -> SvcodeP a 
returnC (w,s) a = SvcodeP $ \ ctx _ _ fr -> Right (a,(w,s),ctx,fr)


------ instruction init

sInit :: [SInstr] -> Dag -> Sup -> SvcodeP ()
sInit code d sup = 
  do mapM_ (\i -> sInstrInit i d sup) code
     mapM_ (\sid -> do ctx <- getCtx   -- add SState for empty streams/SIds        
                       case lookup sid ctx of  
                         Nothing -> 
                           let cls =  map (\ cl -> (cl,0)) (d !! sid)
                            in addCtx sid (Eos, sup !! sid, cls, Done ())
                         Just _ -> return ())
           [0..length d-1] -- !!
   

sInstrInit :: SInstr -> Dag -> Sup -> SvcodeP ()
sInstrInit (SDef sid e) d sup = 
  do let cls = d !! sid 
         suppliers = sup !! sid 
     p <- sExpProcInit e 
     addCtx sid (Filling [], suppliers, map (\ cl -> (cl,0)) cls, p)  

sInstrInit (WithCtrl ctrl code _) d sup = 
  do -- (buf, consu, bs, p) <- lookupSid ctrl
     -- updateCtx ctrl (buf,consu,((-2,0),0):bs,p) 
     mapM_ (\i -> sInstrInit i d sup) code 


sInstrInit (SCall fid argSid retSids) d _ = 
  do let cls = map (\s -> if length d > s then d !! s else []) retSids  --- zz
     zipWithM_ (\s cl -> addCtx s (Filling [], argSid, map (\c -> (c,0)) cl, Done ()))
       retSids cls 
     return ()
 


---- SExpression init
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

sExpProcInit (Check _ _) = return checkProc



lookupOpA :: OP -> OpAEnv -> SvcodeP ([AVal] -> AVal)
lookupOpA op r = 
  do case lookup op r of
       Just (v,_) -> return v 
       Nothing -> fail $ "lookupOpA: undefined operation " ++ show op 


allEnd :: [((SId,Int),Int)] -> Int -> Bool 
allEnd bs len = all (\(_,c) -> c >= len) bs 

allStart :: [((SId,Int),Int)] -> Bool 
allStart bs = all (\(_,b) -> b == 0) bs 

resetCur :: [((SId,Int),Int)] -> [((SId,Int), Int)]
resetCur bs = map (\(sid, _) -> (sid, 0)) bs 


----- run a process
sSIdInterp :: Int -> SId -> SvcodeP Bool
sSIdInterp bufSize sid = 
  do (buf, sups, bs, p) <- lookupSid sid 
     case p of 
       -- 1. Done 
       Done () -> 
         case buf of 
           -- 1.1 the last chunk, so must use stealing 
           Filling as -> let buf' = if null as then Eos else Draining as 
                         in do updateCtx sid (buf', sups, bs, Done()) 
                               return True
           
           -- 1.2 just wait all clients finish reading its buffer
           Draining as -> if allEnd bs (length as)
                          then do updateCtx sid (Eos, sups, resetCur bs, Done ()) 
                                  returnC (length as,1) True  -- add cost 
                          else return True

           -- 1.3 do nothing
           Eos -> return True

       -- 2. Pout
       Pout a p' ->  
         case buf of 
           -- 2.1 fill the buffer until full 
           -- and then switch to draining 
           Filling as -> do let buf' = if length as +1 >= bufSize  
                                         then Draining $ as ++ [a] 
                                         else Filling $ as ++ [a]
                            updateCtx sid (buf', sups, bs, p') 
                            sSIdInterp bufSize sid  

           -- 2.2 only check if all clients have finished reading the buffer
           -- if so, switch to filling
           Draining as -> 
             if allEnd bs (length as)
               then do updateCtx sid (Filling [], sups, resetCur bs, Pout a p')
                       costInc (bufSize, 1)  -- cost work `bufSize`, step 1
                       sSIdInterp bufSize sid -- start filling
               else return False -- blocking 

           -- 2.3 can not happen
           Eos -> fail $ "Premature EOS "
       
-- 3. Pin i p'
-- for simplicity, only try to read from the supplier and modify the cursor if
-- read successfully (so not necessary to distinguish between 3.2 & 3.3)
       Pin i p' -> 
         case buf of 
           Eos -> fail $ "Premature EOS"  -- 3.1

           _ ->   -- 3.2 & 3.3
             do (bufSup, supSup, flagSup,pSup) <- lookupSid (sups!!i)
                case lookup (sid,i) flagSup of -- find own cursor from supplier's clients
                  Nothing -> fail $ "undefined client " ++ show sid
                  Just cursor -> 
                    case bufSup of -- check the bufState of the supplier
                      -- read a `Nothing`
                      Eos -> do updateCtx sid (buf,sups,bs, p' Nothing) 
                                sSIdInterp bufSize sid  -- loop 

                      -- read unavailable, blocking
                      Filling _ -> return False  
                      
                      Draining asSup -> 
                        if cursor >= length asSup
                          then return False -- read blocking 
                          else -- read successfully
                            do let flagSup' = markRead flagSup (sid,i) cursor
                               updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
                               updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor))
                               costInc (1,0)  -- cost work 1, step 0
                               sSIdInterp bufSize sid -- loop


-- interpret an instruction

sInstrInterp :: Int -> SInstr -> SvcodeP Bool
sInstrInterp bufSize def@(SDef sid _) = sSIdInterp bufSize sid 


-- check the first output of the stream `ctrl`,
-- if it's some AVal, then execute `code`
-- if it's Eos, then skip `code` and set the sids of `st` empty               
sInstrInterp bufSize (WithCtrl ctrl code st) = 
  do  bs <- mapM (sInstrInterp bufSize) code 
      return $ all (\x -> x) bs
  --do (buf,c, curs,p) <- lookupSid ctrl
  --   case lookup (-2,0) curs of
  --     Nothing -> -- the 1st value has already been read
  --        do bs <- mapM isEos (tree2Sids st)
  --           if foldl (&&) True bs  
  --           then return True
  --           else do bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
  --                   return $ all (\x -> x) bs              
  --     Just 0 ->  
  --       case buf of 
  --         Eos ->  -- `ctrl` is empty
  --           do doneStream st
  --              updateCtx ctrl (buf, c, delWithKey curs (-2,0), p) 
  --              costInc (1,1)  -- or (1,0) ?
  --              return True
  --         Filling [] ->  -- can't decide whether `ctrl` is emptbs y or not, keep waiting
  --           do bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
  --              return $ all (\x -> x) bs
  --         _ ->  -- `ctrl` is nonempty
  --           do updateCtx ctrl (buf, c, delWithKey curs (-2,0), p) 
  --              bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
  --              return $ all (\x -> x) bs


sInstrInterp bs (SCall fid argSid retSids) =
  do (SFun argSid' st code fresh') <- lookupFId fid
     (_,sups,_,_ ) <- lookupSid $ head retSids
     fresh <- getFresh
     if sups == argSid -- first time of unfolding
     then do let (sup,d) = geneSupDag code 0 fresh'
             oldCtx <- getCtx             
             ctrl <- getCtrl     
             setCtx []
             
             sInit code d sup -- dynamic 
             
             newCtx <- getCtx
             newCtx' <- rewriteCtx newCtx fresh
             setCtx $ oldCtx ++ newCtx'
             
             setFresh (fresh+fresh')
             connectSIds (ctrl:argSid) $ map (+fresh) (0:argSid')
             connectSIds (map (+fresh) (tree2Sids st)) retSids
 
             bs <- mapM (sInstrInterp bs) code 
             return $ all (\x -> x) bs
     else do 
             sups <- mapM getSuppiler retSids
             let lastSid = maximum $ concat $ sups 
                 sids = [lastSid-fresh'+1..lastSid] ++ retSids 
             bs <- mapM (sInstrInterp bs) $ map (\s -> SDef s EmptyCtrl) sids
             return $ all (\x -> x) bs
     


rewriteCtx :: Svctx -> SId -> SvcodeP Svctx
rewriteCtx [] s0 = return []
rewriteCtx ((sid, (b1,s1,c1,p1)):ss) s0 = 
  do ss' <- rewriteCtx ss s0
     let c1' = map (\((s,l),i) -> ((s+s0,l),i) ) c1 
     return $ (sid+s0, (b1, map (+s0) s1, c1',p1)) : ss'


connectSIds :: [SId] -> [SId] -> SvcodeP ()
connectSIds [] [] = return ()
connectSIds (s1:s1s) (s2:s2s) = 
  do (buf1,sup1,cl1,p1) <- lookupSid s1
     (buf2,sup2,cl2,p2) <- lookupSid s2  
     updateCtx s1 (buf1,sup1,cl1 ++ [((s2,0),0)],p1)
     updateCtx s2 (Filling [],[s1],cl2,rinOut) 
     connectSIds s1s s2s 
connectSIds _ _ = fail $ "connectSIds: SIds do not match."


doneStream :: STree -> SvcodeP ()
doneStream (IStr s) = 
  do (_,sup, flags,_) <- lookupSid s
     updateCtx s (Eos,sup, resetCur flags, Done ()) 

doneStream (BStr s) = doneStream (IStr s)
doneStream (PStr st1 st2)  = doneStream st1 >> doneStream st2
doneStream (SStr st1 st2) = doneStream (PStr st1 (BStr st2))
 

isEos :: SId -> SvcodeP Bool
isEos sid = 
  do (buf, _, _,_) <- lookupSid sid
     case buf of 
       Eos -> return True
       _ -> return False



markRead :: Clients -> (SId,Int) -> Int -> Clients
markRead cs sid oldCursor = updateWithKey cs sid (oldCursor+1)


-- update the first pair with the key `k` 
updateWithKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateWithKey [] _ _ = []
updateWithKey (p:ps) k v = 
  if fst p == k then (k,v): ps else p:updateWithKey ps k v


addWithKey :: (Eq a) => [(a,[b])] -> a -> b -> [(a,[b])]
addWithKey [] _ _ = []
addWithKey (p0@(k0,v0):ps) k v = 
  if k0 == k then (k0,v0++[v]): ps else p0:addWithKey ps k v


delWithKey :: (Eq a) => [(a,b)] -> a -> [(a,b)]
delWithKey [] _ = []
delWithKey (p0@(k0,v0):ps) k = 
  if k0 == k then ps else p0:delWithKey ps k


checkWithKey :: (Eq a) => (Eq b) => [(a,b)] -> a -> b -> Bool
checkWithKey [] _ _ = False
checkWithKey (p0@(k0,v0):ps) k v = 
  if (k0 == k) && (v == v0) then True else checkWithKey ps k v 




{------- run the program and print out the Svctx of each round  -------------
-- 
runSvcodePExp' :: SFun -> Int -> Int -> FEnv -> Either String String
runSvcodePExp' (SFun [] st code fresh) count bs fe = 
  do let (sup,d) = geneSupDag code 0 fresh
         retSids = zip (tree2Sids st) [0..]
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids  
     (_,_,ctx) <- rSvcodeP (sInit code d' sup) [] 0 fe 

     (as, ctxs) <- roundN count (round1 (mapM (sInstrInterp bs fresh) code) 0 retSids fe) [ctx]
                     $ [map (\_ -> []) retSids]
     let str = map (\(a,c,i) -> "Round "++ show i ++"\nOutput:" ++ show a ++ "\n" 
                      ++ showCtx c ++ "\n") 
               $ zip3 (reverse as) (reverse ctxs) [0..]
     return $ concat str


showCtx :: Svctx -> String
showCtx [] = ""
showCtx ((sid,(buf,c,bs,p)):ss) = 
  "S" ++ show sid ++ " := (" ++ show buf ++ "," ++ show c ++ "," ++ show bs 
    ++ "," ++ show p ++ ")\n" ++ showCtx ss 
    


roundN :: Int -> (Svctx -> [[AVal]] -> Either String ([[AVal]], Svctx) ) -> 
            [Svctx] -> [[[AVal]]] -> Either String ([[[AVal]]], [Svctx])
roundN 0 f ctxs as = Right (as,ctxs) 
roundN c f ctxs as = 
  if all (\(_,(buf,_, _,p)) -> buf == Eos) (head ctxs)
    then return (as,ctxs)
    else
      do (as',ctx') <- f (head ctxs) (head as)
         if (length ctxs > 0) && (equalCtx ctx' $ ctxs!!0)
         then do unlockCtx <- stealing ctx'
                 roundN (c-1) f (unlockCtx:ctxs) (as':as) 
         else roundN (c-1) f (ctx':ctxs) (as':as)
     

round1 :: SvcodeP [Bool] -> SId -> [(SId,Int)] -> FEnv -> Svctx -> [[AVal]] 
              -> Either String ([[AVal]], Svctx) 
round1 m ctrl st fe ctx as0 = 
  do (_,_, ctx') <- rSvcodeP m ctx ctrl fe 
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                            do (a,c) <- lookupAval c0 s
                               return (a:a0,c))
                        ([],ctx') st 
     let as' = zipWith (++) as0 (reverse as) 
     return (as', ctx'') 

-----------------------------------}


------ helper functions for generating the supplier table and DAG  ------

geneSupDag :: [SInstr] -> SId -> Int -> (Sup,Dag)
geneSupDag code ctrl count = 
  let sup = geneSup code ctrl count
      dag = geneDagFromSup sup
   in (sup, dag)


geneSup :: [SInstr] -> SId -> Int -> Sup
geneSup code ctrl count = 
  let (sids,_,chs) = unzip3 $ instrGeneSup code ctrl  
      c = addEptSids (zip sids chs) 0 [] count
   in snd $ unzip c 


instrGeneSup :: [SInstr] -> SId -> [(SId, String,[SId])]
instrGeneSup [] _ = []
instrGeneSup ((SDef sid sexp):ss') c = (sid,i,cl0) : instrGeneSup ss' c
    where (cl0, i) = getChanExp sexp c
instrGeneSup ((WithCtrl newc ss _):ss') c = cl ++ instrGeneSup ss' c
    where cl = instrGeneSup ss newc

instrGeneSup ((SCall f args rets):ss') c = instrGeneSup ss' c 
    --where cl = map (\r -> (r,"SCall", args)) rets'
          --rets' = sort rets  


addEptSids :: [(SId, a)] -> Int -> a -> Int -> [(SId,a)]
addEptSids [] i a count = zip [i..count-1] (repeat a)
addEptSids ch@(ch0@(c0,sids):ch') i a0 count  
  | c0 == i = ch0 : addEptSids ch' (i+1) a0 count 
  | c0 > i = (i,a0) : addEptSids ch (i+1) a0 count
   -- | c0 < i   -- must be an error case


geneDagFromSup :: Sup -> Dag
geneDagFromSup sup = 
  let sup' = zip [0..] sup
      d0 = map (\(sid, _) -> (sid,[])) sup'
      d = foldl (\d (sid, chs) -> 
              foldl (\d' (ch,i) -> addWithKey d' ch (sid,i)) d $ zip chs [0..]
            ) d0 sup'
  in snd $ unzip d 



getChanExp :: SExp -> SId -> ([SId],String)
getChanExp Ctrl _ = ([],"Ctrl")
getChanExp EmptyCtrl _ = ([],"EmptyCtrl")
getChanExp (Const a) c = ([c],"Const " ++ show a)

getChanExp (MapConst s1 a) _ = ([s1],"MapConst " ++ show a)
getChanExp (MapOne op s1) _ = ([s1],"MapOne " ++ show op)
getChanExp (MapTwo op s1 s2) _ = ([s1,s2],"MapTwo " ++ show op)

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



-------- generate a file to visualize the DAG -------------
--- use "graph-easy": graph-easy <inputfile> --png 
geneDagFile (SFun _ ret code fresh) fname = 
  do let ch0 = instrGeneSup code 0
         ch1 = addEmptyStreams ch0 0 fresh
         ch2 = map (\(i, str,sids) -> 
                        ("S"++ show i ++ ": " ++ str, sids)) ch1
         ch3 = map (\(str, sids) -> (str, map (\i -> fst $ ch2!!i) sids)) ch2
         lines = map (\(x,ys) -> if null ys then drawnode x else          
                  concat $ zipWith (\y c -> drawedge y x c) ys [0..]) ch3         

         retSids = map (\sid -> fst $ ch2!!sid) $ tree2Sids ret 
         retLines = map (\(c,s) -> drawedge s "S-1: Output" c) $ zip [0..] retSids
         content = concat $ lines ++ retLines  
     writeFile fname content
   

drawnode :: String -> String
drawnode i = "[ " ++  i ++ " ]\n"

drawedge :: String -> String -> Int -> String 
drawedge i j c = "[ " ++ i ++ " ] -- " ++ show c ++ " --> [ " ++ j ++ " ]\n"


addEmptyStreams :: [(SId, String, [SId])] -> Int -> Int -> [(SId, String,[SId])]
addEmptyStreams chs i count = 
  let (sids, strs, sidss) = unzip3 chs  
      chs' = addEptSids (zip sids (zip strs sidss)) i ("(empty stream)",[]) count 
      (sids', strSids) = unzip chs'
      (strs', sidss') = unzip strSids
  in zip3 sids' strs' sidss' 

-----------------

