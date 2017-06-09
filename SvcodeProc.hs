module SvcodeProc where

import SvcodeSyntax
import SneslSyntax
import DataTrans (i2flags)


data Proc a = Pin Int (Maybe AVal -> Proc a)  
            | Pout AVal (Proc a)  
            | Done a 


instance Monad Proc where
  return a = Done a

  (Pin i g) >>= f = Pin i (\x -> g x >>= f) 
  (Pout o p) >>= f = Pout o (p >>= f)
  Done a >>= f = f a 


instance Functor Proc where
  fmap f t = t >>= return . f

instance Applicative Proc where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



rin :: Int -> (AVal -> Proc ()) -> Proc ()
rin i p = Pin i (\x ->
  case x of 
    Nothing -> Done ()
    Just v -> p v)


rout :: AVal -> Proc ()
rout a = Pout a (Done ())




mapConst :: AVal -> Proc ()
mapConst a = p
  where p = rin 0 (\x -> rout a >> p) 
 
--mapConst a =       
--  let p = Pin 0 (\x -> 
--              case x of  
--                Nothing -> Done ()
--                Just _ -> Pout a p)
--   in p



usumProc :: Proc ()
usumProc = p
  where p = rin 0 p'
        p' (BVal False) = rout (BVal False) >> p 
        p' (BVal True) = p 


mapTwo :: ([AVal] -> AVal) -> Proc ()
mapTwo op = 
  let p = Pin 0 (\x -> 
            case x of 
              Nothing -> Done ()
              Just a -> Pin 1 (\y ->
                case y of 
                  Nothing -> Done ()
                  Just b -> Pout (op [a,b]) p))
   in p  

--mapOne :: 




toFlags :: Proc ()
--toFlags = p 
--  where p = rin 0 p'
--        p' (IVal x) = mapM_ rout [BVal b | b <- i2flags x] >> p
toFlags = 
  let p = Pin 0 (\x ->
              case x of 
                Nothing -> Done ()
                Just (IVal a) -> mapM_ rout [BVal b | b <- i2flags a] >> p  ) 
   in p



segScanPlus :: Proc ()
--segScanPlus = p 0 
--  where p acc = rin 0 (p' acc)
--        p' acc (BVal False) = rin 1 (pf acc) 
--        p' _ (BVal True) = p 0 
--        pf acc (IVal a) = rout (IVal acc) >> p (a + acc)
segScanPlus =
  let p acc = Pin 0 (\x -> 
                  case x of 
                    Nothing -> Done ()
                    Just (BVal False) -> 
                      Pin 1 (\ (Just (IVal a)) -> Pout (IVal acc) (p (a+acc)))                    
                    Just (BVal True) -> p 0)
  in (p 0)




-- 
evalProc :: Proc () -> [SvVal] -> SvVal -> SvVal
evalProc (Pin i p) ss s = 
  let (a, tailS) = (pread $ ss !! i )
      ss' =  update ss i tailS
   in evalProc (p a) ss' s

evalProc (Pout a p) ss s = 
  evalProc p ss (pwrite a s) 

evalProc (Done ()) _ s = reverseSv s 



update :: [SvVal] -> Int -> SvVal -> [SvVal]
update ss i s = take i ss ++ [s] ++ drop (i+1) ss 




pread :: SvVal -> (Maybe AVal, SvVal)
pread s@(SIVal []) = (Nothing,s)
pread (SIVal (i:is)) = (Just $ IVal i, SIVal is)
pread s@(SBVal []) = (Nothing,s)
pread (SBVal (b:bs)) = (Just $ BVal b, SBVal bs)



pwrite :: AVal -> SvVal -> SvVal
pwrite (IVal i) (SIVal is) = SIVal (i:is) 
pwrite (BVal b) (SBVal bs) = SBVal (b:bs)


reverseSv :: SvVal -> SvVal 
reverseSv (SIVal is) = SIVal $ reverse is
reverseSv (SBVal bs) = SBVal $ reverse bs  











