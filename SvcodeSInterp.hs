{- SVCODE Streaming Interpreter using two-phase scheduling -}

module SvcodeSInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc
import SneslCompiler (tree2Sids)
import SneslInterp (wrapWork)

import Control.Monad
import Data.Set (fromList, toList)



type Svctx = [(SId, SState)]

type SState = (BufState, Suppliers, Clients, Proc ())

data BufState = Filling [AVal] 
              | Draining [AVal]
              | Eos deriving (Show, Eq) 

type Suppliers = [SId]  -- incoming edges
type Clients  = [((SId,Int), Int)] --outgoing edges


type Sup = [[SId]]  -- supplier list
type Dag = [[(SId,Int)]] -- client list


-- classify the SIds into levels by breadth-first traversal of the Dag
-- used in the 1st phase of two-phase scheduling
type Levels = [[SId]] 


 -- blocking list
 -- (s1, Just s2): s1 is blocking at reading from s2
 -- (s1, Nothing): s1 is runnable
type BList = [(SId, Maybe SId)] 



newtype SvcodeP a = SvcodeP {rSvcodeP :: Svctx -> SId -> (Int,Int) -> BList ->
                                Either String (a,(Int,Int),Svctx, BList)}
                                  
instance  Monad SvcodeP where
    return a = SvcodeP $ \ctx _ cost bl -> Right (a,cost,ctx,bl) 

    m >>= f = SvcodeP $ \ctx ctrl cost bl -> 
        case rSvcodeP m ctx ctrl cost bl of 
            Right (a,cost',ctx',bl') -> 
                case rSvcodeP (f a) ctx' ctrl cost' bl' of
                    Right (b,cost'',ctx'',bl'') -> Right (b,cost'',ctx'',bl'')
                    Left err' -> Left err'
            Left err -> Left err 

    fail err = SvcodeP $ \ _ _ _ _ -> Left $ "SVCODE runtime error: " ++ err


instance Functor SvcodeP where
  fmap f t = t >>= return . f 

instance Applicative SvcodeP where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



runSvcodePExp :: SFun -> Int -> Bool -> Either String (SvVal, (Int,Int))
runSvcodePExp (SFun [] st code _) bufSize mflag = 
  do let (sup,d) = geneSupDag code 0
         lv = geneLevels d sup 
         retSids = zip (tree2Sids st) [0..]
         bl0 = blInit sup 
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids 
     (_,_, ctx, _) <- rSvcodeP (sInit code d' sup) [] 0 (0,0) bl0
     (as,(w,s), _,_) <- rrobin (twoPhaSche code bufSize lv mflag) ctx 0 retSids 
                         (map (\_ -> []) retSids) (0,0) bl0
     return (fst $ constrSv st as,(w,s))


    
rrobin :: SvcodeP () -> Svctx -> SId -> [(SId,Int)] -> [[AVal]] -> (Int,Int)
       -> BList -> Either String ([[AVal]],(Int,Int),Svctx, BList)           
rrobin m ctx ctrl retSids as0 cost bl = 
  do (_,cost', ctx', bl') <- rSvcodeP m ctx ctrl cost bl
     (as,ctx'') <- foldM (\(a0,c0) s ->    -- collect the result
                             do (a,c) <- lookupAval c0 s
                                return (a:a0,c)) 
                         ([],ctx') retSids 
     let as' = zipWith (++) as0 (reverse as) 
     (res,_,_,_) <- rSvcodeP (mapM (\(s,_) -> isEos s) retSids) ctx'' ctrl (0,0) bl' 
     if all (\x -> x) res 
       then return (as', cost', ctx'',bl') 
       else if equalCtx ctx ctx'' 
            then do unlockCtx <- stealing ctx''  -- try stealing
                    rrobin m unlockCtx ctrl retSids as' cost' bl'
            else rrobin m ctx'' ctrl retSids as' cost' bl'

blInit :: Sup -> BList
blInit sups = map (\(s,sup) -> if null sup then (s,Nothing) else (s, Just $ head sup)) 
              $ zip [0..] sups 


-- pick the first non-empty stream in Filling mode to drain
stealing :: Svctx -> Either String Svctx
stealing [] = Left "Deadlock!"

stealing ((sid, (Filling as@(a0:_), sup, bs, p)):ss) = 
  Right $ (sid, (Draining as, sup,bs,p)) : ss 

stealing (s:ss) = stealing ss >>= (\ss' -> return (s:ss'))


-- two-phase scheduling
twoPhaSche :: [SInstr] -> Int -> Levels -> Bool -> SvcodeP ()
twoPhaSche ss _ [] _ = return ()
twoPhaSche ss bufSize (level:ls) mflag = 
  do runnable <- filterM (\n -> isRunnable n) level    
     defs <- mapM (getInstr ss) runnable  -- pick out all the runnable instrs
     
     (_,s0) <- getCost
     bls <- mapM (sInstrInterp bufSize) defs  -- and run them     
     (w1,s1) <- getCost

     mapM_ setBlock $ zip runnable bls 

     if mflag -- MIMD flag: fix the step cost
      then (if s1 > s0 then setCost (w1,s0+1) else setCost (w1,s1)) 
      else return ()

     -- check if it's necessary to run the rest levels
     if all (\b -> case b of  
                     Just _ -> True
                     _ -> False) 
             bls  
       then return ()    
       else twoPhaSche ss bufSize ls mflag 



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


getInstr :: [SInstr] -> SId -> SvcodeP SInstr
getInstr ss sid = 
  case lookupSInstr ss sid of 
    Nothing -> fail "getInstr: undefined sid"
    Just def -> return def  


lookupSInstr :: [SInstr] -> SId -> Maybe SInstr
lookupSInstr [] _ = Nothing
lookupSInstr (def@(SDef sid1 _):ss) sid2 = 
  if sid1 == sid2 then (Just def) else lookupSInstr ss sid2 
lookupSInstr (WithCtrl _ code _ :ss) sid2 = 
  case lookupSInstr code sid2 of 
    Nothing -> lookupSInstr ss sid2
    v -> v 


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
      Nothing -> Left  $ "SVCODE runtime error: undefined streams " ++ show s  
      Just (Draining a, c, bs, p) -> 
        if checkWithKey bs (-1,i) 0
        then let bs' = updateWithKey bs (-1,i) (length a)  
                 ctx' = updateWithKey ctx s (Draining a, c, bs', p)
             in Right (a, ctx')
        else Right ([],ctx)
      
      Just (Filling _, _,_, _) -> Right ([], ctx)

      Just (Eos, _, _, _) -> Right ([], ctx)



addCtx :: SId -> SState -> SvcodeP ()
addCtx s v = SvcodeP $ \ c _ cost bl -> Right ((), cost ,c++[(s,v)],bl)

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ cost bl -> Right ((),cost,updateWithKey c s v,bl) 

getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ c _ cost bl -> Right (c,cost,c,bl)

localCtrl :: SId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ ctx _  cost bl -> rSvcodeP m ctx ctrl cost bl 

getCost :: SvcodeP (Int,Int)
getCost = SvcodeP $ \ c _ cost bl -> Right (cost, cost, c,bl)

setCost :: (Int,Int) -> SvcodeP ()
setCost cost = SvcodeP $ \ c _ _ bl -> Right ((), cost, c,bl)


isRunnable :: SId -> SvcodeP Bool
isRunnable s = 
  do b <- getBlock s
     case b of 
       Nothing -> return True
       Just s' -> 
         do b' <- getBlock s' 
            case b' of 
              Nothing -> return True
              _ -> return False


getBlock :: SId -> SvcodeP (Maybe SId)
getBlock sid = SvcodeP $ \ c _ cost bl -> 
    case lookup sid bl of 
      Nothing -> Left $ "getBlock: undefined SId " ++ show sid
      Just v -> Right (v, cost,c, bl)


setBlock :: (SId, Maybe SId) -> SvcodeP ()
setBlock (s,b) = SvcodeP $ \ c _ cost bl ->
   Right ((), cost, c, updateWithKey bl s b)



lookupSid :: SId -> SvcodeP SState
lookupSid s = SvcodeP $ \c ctrl cost bl -> 
    case lookup s c of 
        Nothing -> Left $ "lookupSid: undefined SId " ++ show s  
        Just st -> Right (st,cost,c,bl)  


costInc :: (Int, Int) -> SvcodeP ()
costInc (w,s) = SvcodeP $ \ c _ (w0,s0) bl -> Right ((), (w0+w,s0+s), c,bl)


returnC :: (Int, Int) -> a -> SvcodeP a 
returnC (w,s) a = SvcodeP $ \ ctx _ (w0,s0) bl -> Right (a,(w0+w,s0+s),ctx,bl)


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
           [0..length d-1]
   

sInstrInit :: SInstr -> Dag -> Sup -> SvcodeP ()
sInstrInit (SDef sid e) d sup = 
  do let cls = d !! sid 
         suppliers = sup !! sid 
     p <- sExpProcInit e
     case e of 
       EmptyCtrl -> addCtx sid (Eos, suppliers, map (\ cl -> (cl,0)) cls, p)
       _ -> addCtx sid (Filling [], suppliers, map (\ cl -> (cl,0)) cls, p) 


sInstrInit (WithCtrl ctrl code _) d sup = 
     mapM_ (\i -> sInstrInit i d sup) code 


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
       Nothing -> fail $ "SVCODE: can't find " ++ show op 


allEnd :: [((SId,Int),Int)] -> Int -> Bool 
allEnd bs len = all (\(_,c) -> c >= len) bs 

allStart :: [((SId,Int),Int)] -> Bool 
allStart bs = all (\(_,b) -> b == 0) bs 

resetCur :: [((SId,Int),Int)] -> [((SId,Int), Int)]
resetCur bs = map (\(sid, _) -> (sid, 0)) bs 


----- instruction interp

-- return True: runnable
-- return False: not runnable due to read blocking 
sInstrInterp :: Int -> SInstr -> SvcodeP (Maybe SId)
sInstrInterp bufSize def@(SDef sid _) =
  do (buf, sups, bs, p) <- lookupSid sid
     case p of 
       -- 1. Done 
       Done () -> 
         case buf of 
           -- 1.1 the last chunk, so must use stealing 
           Filling as -> let buf' = if null as then Eos else Draining as 
                         in do updateCtx sid (buf', sups, bs, Done()) 
                               return Nothing
           
           -- 1.2 just wait all clients finish reading its buffer
           Draining as -> if allEnd bs (length as)
                          then do updateCtx sid (Eos, sups, resetCur bs, Done ()) 
                                  returnC (length as,1) Nothing -- add cost 
                          else return Nothing

           -- 1.3 do nothing
           Eos -> return Nothing

       -- 2. Pout
       Pout a p' ->  
         case buf of 
           -- 2.1 fill the buffer until full 
           -- and then switch to draining 
           Filling as -> do let buf' = if length as +1 >= bufSize  
                                         then Draining $ as ++ [a] 
                                         else Filling $ as ++ [a]
                            updateCtx sid (buf', sups, bs, p') 
                            sInstrInterp bufSize def  

           -- 2.2 only check if all clients have finished reading the buffer
           -- if so, switch to filling
           Draining as -> 
             if allEnd bs (length as)
               then do updateCtx sid (Filling [], sups, resetCur bs, Pout a p')
                       costInc (bufSize, 1)  -- cost work `bufSize`, step 1
                       sInstrInterp bufSize def -- start filling
               else return Nothing -- write blocking

           -- 2.3 can not happen
           Eos -> fail $ "Premature EOS "
       
-- 3. Pin i p'
-- for simplicity, only try to read from the supplier and modify the cursor if
-- read successfully (so not necessary to distinguish between 3.2 & 3.3)
       Pin i p' -> 
         case buf of 
           Eos -> fail $ "Premature EOS"  -- 3.1

           other ->   -- 3.2 & 3.3
             do (bufSup, supSup, flagSup,pSup) <- lookupSid (sups!!i)
                case lookup (sid,i) flagSup of -- find own cursor from supplier's clients
                  Nothing -> fail $ "undefined client " ++ show sid
                  Just cursor -> 
                    case bufSup of -- check the bufState of the supplier
                      -- read a `Nothing`
                      Eos -> do updateCtx sid (buf,sups,bs, p' Nothing) 
                                sInstrInterp bufSize def  -- loop 

                      -- read unavailable, blocking
                      Filling _ -> case other of 
                                     Filling _ -> return $ Just (sups!!i)
                                     Draining _ -> return Nothing  
                      Draining asSup -> 
                        if cursor >= length asSup
                          then case other of 
                                 Filling _ -> return $ Just (sups!!i)
                                 Draining _ -> return Nothing 
                          else -- read successfully
                            do let flagSup' = markRead flagSup (sid,i) cursor
                               updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
                               updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor))
                               costInc (1,0)  -- cost work 1, step 0
                               sInstrInterp bufSize def -- loop


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


checkWithKey :: (Eq a) => (Eq b) => [(a,b)] -> a -> b -> Bool
checkWithKey [] _ _ = False
checkWithKey (p0@(k0,v0):ps) k v = 
  if (k0 == k) && (v == v0) then True else checkWithKey ps k v 




{------- run the program and print out the Svctx of each round  -------------
-- 
runSvcodePExp' :: SFun -> Int -> Int -> Either String String
runSvcodePExp' (SFun [] st code _) count bs = 
  do let (sup,d) = geneSupDag code 0
         lv = geneLevels d sup 
         bl0 = blInit sup 
         retSids = zip (tree2Sids st) [0..]
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids  
     (_,_,ctx,_) <- rSvcodeP (sInit code d' sup) [] 0 (0,0) bl0

     (as, ctxs) <- roundN count (round1 (twoPhaSche code bs lv False) 0 retSids) [ctx]
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
  if all (\(_,(buf,_, _,p)) -> buf == Eos) (head ctxs) -- only need to check retSids
    then return (as,ctxs)
    else
      do (as',ctx') <- f (head ctxs) (head as)
         if (length ctxs > 0) && (equalCtx ctx' $ ctxs!!0)
         then do unlockCtx <- stealing ctx'
                 roundN (c-1) f (unlockCtx:ctxs) (as':as) 
         else roundN (c-1) f (ctx':ctxs) (as':as) 
     

round1 :: SvcodeP () -> SId -> [(SId,Int)] -> Svctx -> [[AVal]] 
              -> Either String ([[AVal]], Svctx) 
round1 m ctrl st ctx as0 = 
  do (_,_, ctx') <- rSvcodeP m ctx ctrl (0,0)
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                            do (a,c) <- lookupAval c0 s
                               return (a:a0,c))
                        ([],ctx') st 
     let as' = zipWith (++) as0 (reverse as) 
     return (as', ctx'') 

-----------------------------------}


------ helper functions for generating the supplier table and DAG  ------

geneSupDag :: [SInstr] -> SId -> (Sup,Dag)
geneSupDag code ctrl = 
  let sup = geneSup code ctrl
      dag = geneDagFromSup sup
   in (sup, dag)


geneSup :: [SInstr] -> SId -> Sup
geneSup code ctrl = 
  let (sids,_,chs) = unzip3 $ instrGeneSup code ctrl 
      c = addEptSids (zip sids chs) 0 []
   in snd $ unzip c 


instrGeneSup :: [SInstr] -> SId -> [(SId, String,[SId])]
instrGeneSup [] _ = []
instrGeneSup ((SDef sid sexp):ss) c = (sid,i,cl0) : instrGeneSup ss c
    where (cl0, i) = getChanExp sexp c
instrGeneSup ((WithCtrl newc ss _):ss') c = cl ++ instrGeneSup ss' c
    where cl = instrGeneSup ss newc



addEptSids :: [(SId, a)] -> Int -> a -> [(SId,a)]
addEptSids [] _ _ = []
addEptSids ch@(ch0@(c0,sids):ch') i a0 
  | c0 == i = ch0 : addEptSids ch' (i+1) a0
  | c0 > i = (i,a0) : addEptSids ch (i+1) a0 
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
geneDagFile (SFun _ ret code _) fname = 
  do let ch0 = instrGeneSup code 0
         ch1 = addEmptyStreams ch0 0
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


addEmptyStreams :: [(SId, String, [SId])] -> Int -> [(SId, String,[SId])]
addEmptyStreams chs i = 
  let (sids, strs, sidss) = unzip3 chs  
      chs' = addEptSids (zip sids (zip strs sidss)) i ("(empty stream)",[])
      (sids', strSids) = unzip chs'
      (strs', sidss') = unzip strSids
  in zip3 sids' strs' sidss' 



------- functions for generating Levels  -----------

geneLevels :: Dag -> Sup -> Levels
geneLevels dag sup = 
  let maxLev = length dag + 1 
      -- find the roots, not just Ctrl, but also empty streams
      node0 = fst $ unzip $ filter (\(n,s) -> null s) $ zip [0..] sup 
      
      levelInit = map (\_ -> maxLev) dag
      levelInit0 = foldl (\l n -> updateList l n 0) levelInit node0  
      dag' = map (\clients -> fst $ unzip clients) dag -- remove the labels of client
    in genelevRecur dag' sup levelInit0 maxLev [[0]] [0]


genelevRecur :: [[SId]] -> Sup -> [Int] -> Int -> Levels ->[SId] -> Levels
genelevRecur dag sup levelTab m levels roots = 
  let nodes = toList $ fromList $ 
                concat $ map (\r -> geneLev dag sup levelTab m r) roots
  in if null nodes 
      then levels 
      else let newLT = foldl (\l n -> updateList l n (length levels)) 
                          levelTab nodes
               newLevels = levels ++ [nodes]
           in genelevRecur dag sup newLT m newLevels nodes


geneLev :: [[SId]] -> Sup -> [Int] -> Int -> SId -> [SId]
geneLev dag sup levelTable m root =
  let clients = dag !! root  
      clSup = map (\cl -> sup !! cl) clients
      clLev = map (\sups -> maximum $ map (\sup -> levelTable !! sup) sups) 
                clSup
   in fst $ unzip $ filter (\(_,l) -> not (l == m)) $ zip clients clLev 


-----------------

