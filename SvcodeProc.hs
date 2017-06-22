{- SVCODE transducers -}

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



toFlags :: Proc ()
toFlags = p 
  where p = rin 0 (\(IVal a) -> mapM_ rout [BVal b | b <- i2flags a] >> p)



usumProc :: Proc ()
usumProc = p
  where p = rin 0 (\x -> 
              case x of 
                BVal False -> rout (BVal False) >> p 
                BVal True -> p)



mapTwo :: ([AVal] -> AVal) -> Proc ()
mapTwo op = p 
  where p = rin 0 (\x -> rin 1 (\y -> rout (op [x,y]) >> p))



mapOne :: ([AVal] -> AVal) -> Proc ()
mapOne op = p 
  where p = rin 0 (\x -> rout (op [x]) >> p)


packProc :: Proc ()
packProc = p 
  where p = rin 0 (\x -> 
              case x of 
                BVal False -> rin 1 (\_ -> p)
                BVal True -> rin 1 (\y -> rout y >> p))


upackProc :: Proc ()
upackProc = p 
  where p = rin 0 (\x -> 
              case x of 
                BVal False -> uIn 1 >> p
                BVal True -> uInOut 1 >> rout (BVal True) >> p )


uIn :: Int -> Proc ()
uIn i = rin i (\x -> 
          case x of 
            BVal False -> uIn i
            BVal True -> Done ())



uInOut :: Int -> Proc ()
uInOut i = rin i (\x -> 
             case x of 
               BVal False -> rout (BVal False) >> (uInOut i)
               BVal True -> Done ())


pdistProc :: Proc ()
pdistProc = p
  where p = rin 0 (\x -> rin 1 (\y -> 
              case y of 
                BVal False -> rout x >> (uOut 1 x) >> p 
                BVal True -> p))
 

uOut :: Int -> AVal -> Proc ()
uOut i a = rin i (\x -> 
             case x of 
               BVal False -> rout a >> (uOut i a)
               BVal True -> Done ())



segDistrProc :: Proc ()
segDistrProc = p 
  where p = do vs <- uRecord 0
               rin 1 (\y -> 
                 case y of
                   BVal False -> mapM_ rout vs >> uOuts 1 vs >> p 
                   BVal True -> p)


-- not feasible in a streaming setting?
segFlagDistrProc :: Proc ()
segFlagDistrProc = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> do vs' <- uRecord 1; p (vs++vs')
                   BVal True -> rin 2 (\y -> 
                     case y of 
                       BVal False -> mapM_ rout vs >> uOuts 2 vs >> (p [])
                       BVal True -> p []))


-- ?
primSegFlagDistrProc :: Proc ()
primSegFlagDistrProc = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> rin 1 (\y -> p (vs ++ [y]))
                   BVal True -> rin 2 (\y -> 
                     case y of 
                       BVal False -> mapM_ rout vs >> uOuts 2 vs >> (p [])
                       BVal True -> p []))



-- read in and out a unary (without the True flag)
uRecord :: Int -> Proc [AVal]
uRecord i =
  let p vs = Pin i (\x -> 
               case x of 
                 Nothing -> Done vs 
                 Just (BVal False) -> p ((BVal False):vs)  
                 Just (BVal True) -> Done (reverse (BVal True : vs)))
  in (p [])


uOuts :: Int -> [AVal] -> Proc ()
uOuts i as = rin i (\x -> 
               case x of 
                 BVal False -> mapM_ rout as >> (uOuts i as)
                 BVal True -> Done ())



b2uProc :: Proc ()
b2uProc = p 
  where p = rin 0 (\x -> 
              case x of 
                BVal True -> rout (BVal False) >> rout (BVal True) >> p 
                BVal False -> rout (BVal True) >> p )



segScanPlusProc :: Proc ()
segScanPlusProc = p 0 
  where p acc = rin 0 (\x -> 
              case x of 
                BVal False -> rin 1 (\(IVal a) -> rout (IVal acc) >> p (a+acc))
                BVal True -> p 0)



segReducePlusProc :: Proc ()
segReducePlusProc = 
  let p acc = Pin 0 (\x -> 
                case x of 
                  Nothing -> Done ()
                  Just (BVal False) -> 
                    Pin 1 (\(Just (IVal a)) -> p (acc + a))
                  Just (BVal True) -> Pout (IVal acc) (p 0))
  in (p 0)


segConcatProc :: Proc ()
segConcatProc = 
  let p = Pin 0 (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> (uInOut 1) >> p 
              Just (BVal True) -> rout (BVal True) >> p)
  in p  
                

uSegCountProc :: Proc ()
uSegCountProc = 
  let p = Pin 0 (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> Pin 1 (\y -> 
                case y of 
                  Just (BVal False) -> p 
                  Just (BVal True) -> Pout (BVal False) p) 
              Just (BVal True) -> Pout (BVal True) p )
  in p 




segMergeProc :: Proc ()
segMergeProc = 
  let p = Pin 0 (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> uInOut 1 >> p
              Just (BVal True) -> Pout (BVal True) p)
  in p 


interMergeProc :: Int -> Proc ()
interMergeProc c = 
  let p = Pin 0 (\x -> 
            case x of 
              Nothing -> Done ()
              Just v -> rout v >> mapM_ uInOut [0..c-1] >> rout (BVal True) >> p)
  in p  


segInterProc :: [(Int,Int)] -> Proc ()
segInterProc cs = 
  let (j,i) = head cs 
      p = Pin i (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> uInOut j >> Pout (BVal True) p 
              Just (BVal True) -> mapM_ segInterP (tail cs) >> p )
   in p      


segInterP :: (Int, Int) -> Proc ()
segInterP (j,i) = 
  let p = Pin i (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> uInOut j >> Pout (BVal True) p 
              Just (BVal True) -> Done ())
  in p 


priSegInterProc :: [(Int, Int)] -> Proc ()
priSegInterProc cs = 
  let (j,i) = head cs 
      p = Pin i (\x -> 
            case x of 
              Nothing -> Done ()
              Just (BVal False) -> Pin j (\(Just v) -> Pout v p) 
              Just (BVal True) -> mapM_ priSegInterP (tail cs) >> p )
      in p 


priSegInterP :: (Int, Int) -> Proc ()
priSegInterP (j,i) = 
  let p = Pin i (\x -> 
            case x of 
              Nothing -> Done () 
              Just (BVal False) -> Pin j (\(Just v) -> Pout v p) 
              Just (BVal True) -> Done ())
  in p   



----
evalProcA :: Proc () -> [AVal] -> (AVal, Proc ()) 
evalProcA (Pin c p) as = evalProcA (p (Just $ as !! c)) as 
 
evalProcA (Pout a p) _ = (a, p) 

-- evalProcA (Done ()) as = 




------- 

evalProc :: Proc () -> [SvVal] -> SvVal -> SvVal
evalProc (Pin i p) ss s = 
  let (a, tailS) = (pread $ ss !! i )
      ss' =  updateList ss i tailS
   in evalProc (p a) ss' s

evalProc (Pout a p) ss s = 
  evalProc p ss (pwrite a s) 

evalProc (Done ()) _ s = reverseSv s 




updateList :: [a] -> Int -> a -> [a]
updateList ss i s = take i ss ++ [s] ++ drop (i+1) ss 


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











