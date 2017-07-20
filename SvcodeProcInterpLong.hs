{- SVCODE Streaming Interpreter with arbitrary buffer size -}

module SvcodeProcInterpLong where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc
import SneslCompiler (tree2Sids)
import SneslInterp (wrapWork)

import Control.Monad


type Svctx = [(SId, SState)]

type SState = (BufState, Suppliers, Clients, Proc ())

data BufState = Filling [AVal] 
              | Draining [AVal]
              | Eos deriving (Show, Eq) 

type Suppliers = [SId]  -- incoming edges
type Clients  = [((SId,Int), Int)] --outgoing edges


type Sup = [[SId]]  -- supplier list
type Dag = [[(SId,Int)]] -- client list


newtype SvcodeP a = SvcodeP {rSvcodeP :: Svctx -> SId -> 
                                Either String (a,(Int,Int),Svctx)}
                                  
instance  Monad SvcodeP where
    return a = SvcodeP $ \ctx ctrl -> Right (a,(0,0),ctx) 

    m >>= f = SvcodeP $ \ctx ctrl -> 
        case rSvcodeP m ctx ctrl of 
            Right (a,(w,s),ctx') -> 
                case rSvcodeP (f a) ctx' ctrl of
                    Right (b,(w',s'),ctx'') -> Right (b,(w+w',s+s'),ctx'')
                    Left err' -> Left err'
            Left err -> Left err 

    fail err = SvcodeP $ \ _ _ -> Left $ "SVCODE runtime error: " ++ err


instance Functor SvcodeP where
  fmap f t = t >>= return . f 

instance Applicative SvcodeP where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



runSvcodePExp :: SFun -> Int -> Either String (SvVal, (Int,Int))
runSvcodePExp (SFun [] st code _) bs = 
  do let (sup,d) = geneSupDag code 0
         retSids = zip (tree2Sids st) [0..]
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids 
     (_,(w0,s0), ctx) <- rSvcodeP (sInit code d' sup) [] 0 
     (as,(w1,s1), _) <- rrobin (mapM (sInstrInterp bs) code) ctx 0 retSids 
                    (map (\_ -> []) retSids) (w0,s0)
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


------- run the program and print out the Svctx of each round  -------------
runSvcodePExp' :: SFun -> Int -> Int -> Either String String
runSvcodePExp' (SFun [] st code _) count bs = 
  do let (sup,d) = geneSupDag code 0
         retSids = zip (tree2Sids st) [0..]
         d' = foldl (\dag (sid,i) -> addClient dag sid (-1,i)) d retSids  
     (_,_,ctx) <- rSvcodeP (sInit code d' sup) [] 0 

     (as, ctxs) <- roundN count (round1 (mapM (sInstrInterp bs) code) 0 retSids) [ctx]
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
         then do unlockCtx <- fill2Drain ctx'
                 roundN (c-1) f (unlockCtx:ctxs) (as':as) 
         else roundN (c-1) f (ctx':ctxs) (as':as) 
     

round1 :: SvcodeP [Bool] -> SId -> [(SId,Int)] -> Svctx -> [[AVal]] 
              -> Either String ([[AVal]], Svctx) 
round1 m ctrl st ctx as0 = 
  do (_,_, ctx') <- rSvcodeP m ctx ctrl
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                            do (a,c) <- lookupAval c0 s
                               return (a:a0,c))
                        ([],ctx') st 
     let as' = zipWith (++) as0 (reverse as) 
     return (as', ctx'') 

-----------------------------------}

     
rrobin :: SvcodeP [Bool] -> Svctx -> SId -> [(SId,Int)] -> [[AVal]] -> (Int,Int)
            -> Either String ([[AVal]],(Int,Int),Svctx)           
rrobin m ctx ctrl retSids as0 (w0,s0)= 
  do (bs,(w1,s1), ctx') <- rSvcodeP m ctx ctrl
     (as,ctx'') <- foldM (\(a0,c0) s -> 
                             do (a,c) <- lookupAval c0 s
                                return (a:a0,c)) 
                         ([],ctx') retSids 
     let as' = zipWith (++) as0 (reverse as) 
     if all (\x -> x) bs 
       then return (as', (w0+w1,s0+s1), ctx'') 
       else if equalCtx ctx ctx'' 
            then do unlockCtx <- fill2Drain ctx''
                    rrobin m unlockCtx ctrl retSids as' (w0+w1,s0+s1)
            else rrobin m ctx'' ctrl retSids as' (w0+w1,s0+s1)


fill2Drain :: Svctx -> Either String Svctx
fill2Drain [] = Left "Deadlock!"
fill2Drain ((sid, (Filling as, sup, bs, p)):ss) = Right $ (sid, (Draining as, sup,bs,p)) : ss 
fill2Drain (s:ss) = fill2Drain ss >>= (\ss' -> return (s:ss'))


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
addCtx s v = SvcodeP $ \ c _ -> Right ((), (0,0),c++[(s,v)])

updateCtx :: SId -> SState -> SvcodeP ()
updateCtx s v = SvcodeP $ \ c _ -> Right ((),(0,0),updateWithKey c s v) 

getCtx :: SvcodeP Svctx
getCtx = SvcodeP $ \ c _ -> Right (c,(0,0),c)

localCtrl :: SId -> SvcodeP a -> SvcodeP a 
localCtrl ctrl m = SvcodeP $ \ ctx _  -> rSvcodeP m ctx ctrl


lookupSid :: SId -> SvcodeP SState
lookupSid s = SvcodeP $ \c ctrl -> 
    case lookup s c of 
        Nothing -> Left $ "lookupSid: undefined SId " ++ show s  
        Just st -> Right (st,(0,0),c)  


costInc :: (Int, Int) -> SvcodeP ()
costInc (w,s) = SvcodeP $ \ c _ -> Right ((), (w,s), c)


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
     addCtx sid (Filling [], suppliers, map (\ cl -> (cl,0)) cls, p)  

sInstrInit (WithCtrl ctrl code _) d sup = 
  do (buf, consu, bs, p) <- lookupSid ctrl
     updateCtx ctrl (buf,consu,((-2,0),0):bs,p) 
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

--sExpProcInit (Check )


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

sInstrInterp :: Int -> SInstr -> SvcodeP Bool
sInstrInterp bufSize def@(SDef sid i) = 
  do (buf, sups, bs, p) <- lookupSid sid 
     case buf of 
       Eos -> return True 
       Filling as -> 
         case p of 
           Done () -> do if null as 
                           then updateCtx sid (Eos, sups, bs, p) 
                           else updateCtx sid (Draining as, sups, bs, p) 
                         return True
           Pout a p' -> let buf' = if length as +1 >= bufSize
                                   then Draining $ as ++ [a]
                                   else Filling $ as ++ [a]
                          in do updateCtx sid (buf', sups, bs, p')
                                sInstrInterp bufSize def                            
           Pin i p' -> 
              do (bufSup, supSup, flagSup,pSup) <- lookupSid (sups!!i)
                 case lookup (sid,i) flagSup of 
                   Nothing -> fail $ "undefined client " ++ show sid
                   Just cursor -> 
                     case bufSup of 
                       Eos -> updateCtx sid (buf,sups,bs,p' Nothing) >> sInstrInterp bufSize def 
                       Filling _ -> return False
                       Draining asSup -> 
                         if cursor >= length asSup 
                         then costInc (0,1) >> return False   -- read blocking
                         else 
                           do let flagSup' = markRead flagSup (sid,i) cursor
                              updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
                              updateCtx sid (buf,sups,bs,p' (Just $ asSup !! cursor)) 
                              costInc (1,0)
                              sInstrInterp bufSize def 


       Draining as -> 
          case p of
            Done () -> (if allEnd bs (length as) 
                          then updateCtx sid (Eos,sups,resetCur bs,p) >> costInc (1,1)
                          else return ()) >> return True
            _ -> (if allEnd bs bufSize 
                    then updateCtx sid (Filling [], sups, resetCur bs, p) 
                    else 
                      if allEnd bs (length as) -- the last chunk
                        then updateCtx sid (Eos, sups, resetCur bs, p) 
                        else return ()) >> return False

            --Pout a p' -> (if allEnd bs (length a0)  -- start filling
            --                then do updateCtx sid (Buf [a],sups,resetCur bs, p')
            --                        costInc (1,0)
            --                        sInstrInterp bufSize def 
            --                else if (allStart bs) && (length a0 < bufSize) -- keep filling
            --                      then do updateCtx sid (Buf (a0 ++[a]),sups,bs,p')
            --                              costInc (1,0)
            --                              sInstrInterp bufSize def 
            --                      else costInc (0,1) >> return False)  -- write blocking
            --Pin i p' ->
            --  do (bufSup, supSup, flagSup,pSup) <- lookupSid (sups!!i)
            --     case lookup (sid,i) flagSup of 
            --       Nothing -> fail $ "undefined client " ++ show sid
            --       Just cursor -> 
            --         case bufSup of 
            --           Eos -> updateCtx sid (buf,sups,bs,p' Nothing) >> sInstrInterp bufSize def 
            --           Buf as -> 
            --             if cursor >= length as 
            --             then costInc (0,1) >> return False   -- read blocking
            --             else 
            --               do let flagSup' = markRead flagSup (sid,i) cursor
            --                  updateCtx (sups!!i) (bufSup, supSup, flagSup',pSup)  
            --                  updateCtx sid (buf,sups,bs,p' (Just $ as !! cursor)) 
            --                  costInc (1,0)
            --                  sInstrInterp bufSize def 


-- check the first output of the stream `ctrl`,
-- if it's some AVal, then execute `code`
-- if it's Eos, then skip `code` and set the sids of `st` empty               
sInstrInterp bufSize (WithCtrl ctrl code st) = 
  do (buf,c, curs,p) <- lookupSid ctrl
     case lookup (-2,0) curs of
       Nothing -> -- the 1st value has already been read
          do bs <- mapM isEos (tree2Sids st)
             if foldl (&&) True bs  
             then return True
             else do bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
                     return $ all (\x -> x) bs              
       Just 0 ->  
         case buf of 
           Eos ->  -- `ctrl` is empty
             do doneStream st
                updateCtx ctrl (buf, c, delWithKey curs (-2,0), p) 
                costInc (1,1) 
                return True
           Filling [] ->  -- can't decide whether `ctrl` is emptbs y or not, keep waiting
             do bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
                return $ all (\x -> x) bs
           _ ->  -- `ctrl` is nonempty
             do updateCtx ctrl (buf, c, delWithKey curs (-2,0), p) 
                bs <- localCtrl ctrl $ mapM (sInstrInterp bufSize) code 
                return $ all (\x -> x) bs



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
    where (cl0, i ) = getChanExp sexp c
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


-----------------

