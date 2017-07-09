module SvcodePreInterp where

import SvcodeSyntax
import SneslSyntax


import Control.Monad 
import Data.Set (fromList, toList)
import Data.List (delete)


type PreCtx = [(SId, SExp)]

newtype PreInterp a = PreInterp {rPreInterp ::  SId -> PreCtx -> 
                                     Either String (a, PreCtx, [SInstr], SId)}

instance Monad PreInterp where
    return a = PreInterp $ \ sid ctx -> Right (a, ctx, [], sid)

    m >>= f = PreInterp $ \ sid ctx -> 
      case rPreInterp m sid ctx of 
        Right (a, ctx', code1, sid') -> 
          case rPreInterp (f a) sid' ctx' of 
            Right (b,ctx'',code2, sid'') -> Right (b,ctx'',code1++code2, sid'')
            Left err' -> Left err' 
        Left err -> Left err

    fail e = PreInterp $ \ _ _ -> Left $ "SVCODE pre-interpret error: " ++ e 

instance Functor PreInterp where
  fmap f t = t >>= return . f 

instance Applicative PreInterp where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta 



--runSvcodePreInterp :: SFun -> Either String ([SInstr],[SId],Svctx)
runSvcodePreInterp (SFun [] stree code freshSid) = 
  do let ctx = constrCtx code 
         m = mapM_ instrChk code >> mapM recoverCode code  
     (newIs, ctx', copyIs, freshSid') <- rPreInterp m freshSid ctx 
     --newCode <- rPreInterp (mapM recoverCode code) 
     return $ SFun [] stree (newIs ++ copyIs) freshSid'
     ----let (ch,d) = geneCTabDag code' 0
     --retSids = tree2Sids st
     ----d' = foldl (\dag sid -> addClient dag sid (-1)) d retSids  -- output flag (client number: -1)

     --return (code', retSids, [])


constrCtx :: [SInstr] -> PreCtx
constrCtx [] = []
constrCtx ((SDef sid exp):ss) = (sid, exp) : constrCtx ss
constrCtx ((WithCtrl _ code _):ss) = ctx ++ (constrCtx ss) 
  where ctx = constrCtx code 

recoverCode :: SInstr -> PreInterp SInstr
recoverCode (SDef sid _) = 
  do e <- lookupExp sid 
     return (SDef sid e) 

recoverCode (WithCtrl ctrl code st) = 
  do newCode <- mapM recoverCode code
     return (WithCtrl ctrl newCode st)


lookupExp :: SId -> PreInterp SExp 
lookupExp s = PreInterp $ \ sid ctx -> 
  case lookup s ctx of 
    Nothing -> Left $ "lookupExp: undefined SId " ++ show s  
    Just e -> Right (e, ctx, [], sid)


updateCtx :: SId -> SExp -> PreInterp ()
updateCtx s e = PreInterp $ \ sid ctx -> 
  Right ((), updateWithKey ctx s e, [], sid)


emit :: SExp -> PreInterp SId
emit e = PreInterp $ \ sid ctx -> Right (sid, ctx, [SDef sid e] ,sid+1)


instrChk :: SInstr -> PreInterp ()
instrChk (SDef sid (MapTwo op s1 s2)) = 
  if s1 == s2 
    then do e <- lookupExp s1 
            s1Copy <- emit e 
            updateCtx sid (MapTwo op s1 s1Copy)
    else return ()

instrChk (SDef sid (InterMergeS ss)) = 
  do let ssUnique = toList $ fromList ss 
     ss' <- copyInstr ss ssUnique
     updateCtx sid (InterMergeS ss')


instrChk (SDef sid (SegInterS ss)) =
  do let (s1s,s2s) = unzip ss 
         sUnique = map (toList.fromList) [s1s,s2s]
     [s1s',s2s'] <- zipWithM copyInstr [s1s,s2s] sUnique
     updateCtx sid (SegInterS $ zip s1s' s2s')


instrChk (SDef sid (PriSegInterS ss)) =
  do let (s1s,s2s) = unzip ss 
         sUnique = map (toList.fromList) [s1s,s2s]
     [s1s',s2s'] <- zipWithM copyInstr [s1s,s2s] sUnique
     updateCtx sid (PriSegInterS $ zip s1s' s2s')
     


instrChk (WithCtrl ctrl code _) = mapM_ instrChk code

instrChk (SDef _ e) = return () 




copyInstr :: [SId] -> [SId] -> PreInterp [SId]
copyInstr [] _ = return []
copyInstr (s0:sl1) sl2 = 
  if s0 `elem` sl2 
    then do ss <- copyInstr sl1 (delete s0 sl2); return $ s0:ss 
    else do e <- lookupExp s0
            s0Cp <- emit e
            ss <- copyInstr sl1 sl2  
            return $ s0Cp :ss  
           


-- update the first pair with this key `k` 
updateWithKey :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
updateWithKey [] _ _ = []
updateWithKey (p:ps) k v = 
  if fst p == k then (k,v): ps else p:updateWithKey ps k v





