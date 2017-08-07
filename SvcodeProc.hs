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


instance (Show a) => Show (Proc a) where
  show (Pin i p) = "<Pin " ++ show i ++ " <Proc>>"
  show (Pout a p) = "<Pout " ++ show a ++ " " ++ show p ++ "> "
  show (Done a) = "<Done " ++ show a ++ "> "


instance Functor Proc where
  fmap f t = t >>= return . f

instance Applicative Proc where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


instance (Eq a) => Eq (Proc a) where
  Done a == Done b = a == b
  Pout a p1 == Pout b p2 = (a == b) && (p1 == p2)
  _ == _ = False  -- ??




rin :: Int -> (AVal -> Proc ()) -> Proc ()
rin i p = Pin i (\x ->
  case x of 
    Nothing -> Done ()
    Just v -> p v)

-- Eos is not allowed to read 
rinx :: String -> Int -> Proc AVal
rinx proc i = Pin i (\ x -> 
  case x of 
    Nothing -> error $ proc ++ " premature end of stream" 
    Just a -> Done a)


rout :: AVal -> Proc ()
rout a = Pout a (Done ())


rinOut :: Proc ()
rinOut = rin 0 (\x -> rout x >> rinOut)
 

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


-- the 2nd supplier stream can not be shorter (but can be longer ??!) than 1st
mapTwo :: ([AVal] -> AVal) -> Proc ()
mapTwo op = p 
  where p = rin 0 (\x -> 
              do y <- rinx "mapTwo" 1 
                 rout (op [x,y])
                 p)



mapOne :: ([AVal] -> AVal) -> Proc ()
mapOne op = p 
  where p = rin 0 (\x -> rout (op [x]) >> p)



checkProc :: Proc ()
checkProc = rin 0 (\x -> 
              do y <- rinx "checkProc" 1 
                 if x == y 
                   then checkProc 
                   else error "checkProc: runtime error")



packProc :: Proc ()
packProc =  rin 0 (\x -> 
              case x of 
                BVal False -> rinx "packProc" 1 >> packProc
                BVal True -> rinx "packProc" 1 >>= rout >> packProc)


upackProc :: Proc ()
upackProc = rin 0 (\x -> 
              case x of 
                BVal False -> uInx 1 >> upackProc
                BVal True -> uInOutx 1 >> rout (BVal True) >> upackProc)


-- read an unary and throw it away (can be Eos)
uIn :: Int -> Proc ()
uIn i = rin i (\x ->  
          case x of 
            BVal False -> uIn i
            BVal True -> Done ())


-- must read an unary and throw it away (no Eos)
uInx :: Int -> Proc ()
uInx i = do x <- rinx "uInx" i 
            case x of 
              BVal False -> uInx i
              BVal True -> Done ()

-- must read and output an unary (no Eos)
uInOutx :: Int -> Proc ()
uInOutx i = do x <- rinx "uInOutx" i
               case x of 
                 BVal False -> rout x >> (uInOutx i)
                 BVal True -> Done ()


pdistProc :: Proc ()
pdistProc = p
  where p = rin 0 (\x -> 
              do y <- rinx "pdistProc" 1 
                 case y of 
                   BVal False -> rout x >> (uOutx 1 x) >> p 
                   BVal True -> p)
 

uOutx :: Int -> AVal -> Proc ()
uOutx i a = do x <- rinx "uOutx" i
               case x of 
                 BVal False -> rout a >> (uOutx i a)
                 BVal True -> Done ()



segDistrProc :: Proc ()
segDistrProc = p 
  where p = do vs <- uRecord 0
               if null vs then Done ()
               else  
                 do y <- rinx "segDistrProc" 1
                    case y of
                      BVal False -> mapM_ rout vs >> uOutsx 1 vs >> p 
                      BVal True -> p



segFlagDistrProc :: Proc ()
segFlagDistrProc = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> do vs' <- uRecordx 1; p (vs++vs')
                   BVal True -> 
                     do y <- rinx "segFlagDistrProc" 2
                        case y of 
                          BVal False -> mapM_ rout vs >> uOutsx 2 vs >> (p [])
                          BVal True -> p [])



primSegFlagDistrProc :: Proc ()
primSegFlagDistrProc = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> do y <- rinx "primSegFlagDistrProc" 1; p (vs ++ [y])
                   BVal True -> 
                     do y <- rinx "primSegFlagDistrProc" 2
                        case y of 
                          BVal False -> mapM_ rout vs >> uOutsx 2 vs >> (p [])
                          BVal True -> p [])



-- read in and output a unary (without the True flag)
uRecord :: Int -> Proc [AVal]
uRecord i =
  let p vs = Pin i (\x -> 
               case x of 
                 Nothing -> Done vs 
                 Just (BVal False) -> p ((BVal False):vs)  
                 Just (BVal True) -> Done (reverse (BVal True : vs)))
  in (p [])


uRecordx :: Int -> Proc [AVal]
uRecordx i =
  let p vs = do x <- rinx "uRecordx" i 
                case x of 
                  (BVal False) -> p ((BVal False):vs)  
                  (BVal True) -> Done (reverse (BVal True : vs))
  in (p [])


-- repeat `as` until read a `True` from channel `i` 
-- and don't allow Eos when reading channel `i` 
uOutsx :: Int -> [AVal] -> Proc ()
uOutsx i as = do x <- rinx "uOutsx" i        
                 case x of 
                   BVal False -> mapM_ rout as >> (uOutsx i as)
                   BVal True -> Done ()


b2uProc :: Proc ()
b2uProc = p 
  where p = rin 0 (\x -> 
              case x of 
                BVal True -> rout (BVal False) >> rout (BVal True) >> p 
                BVal False -> rout (BVal True) >> p)



segScanPlusProc :: Proc ()
segScanPlusProc = p 0 
  where p acc = rin 0 (\x -> 
                  case x of 
                    BVal True -> p 0
                    BVal False -> 
                      do y <- rinx "segScanPlusProc" 1 
                         case y of 
                           IVal a -> rout (IVal acc) >> p (a+acc)
                           _ -> error "segScanPlusProc: read some non-integer")



segReducePlusProc :: Proc ()
segReducePlusProc = 
  let p acc = rin 0 (\x -> 
                case x of 
                  BVal True -> rout (IVal acc) >> (p 0)
                  BVal False -> 
                    do y <- rinx "segReducePlusProc" 1 
                       case y of 
                         IVal a -> p (acc + a)
                         _ -> error "segReducePlusProc: read some non-integer")
  in (p 0)


segConcatProc :: Proc ()
segConcatProc = 
  let p = rin 0 (\x -> 
            case x of 
              BVal False -> uInOutx 1 >> p 
              BVal True -> rout (BVal True) >> p)
  in p  
                

uSegCountProc :: Proc ()
uSegCountProc = 
  let p = rin 0 (\x -> 
            case x of 
              BVal True -> rout (BVal True) >> p
              BVal False -> 
                do y <- rinx "uSegCountProc" 1
                   case y of 
                     BVal False -> p 
                     BVal True -> rout (BVal False) >> p)

  in p 




segMergeProc :: Proc ()
segMergeProc = 
  let p = rin 0 (\x -> 
            case x of 
              BVal False -> uInOutx 1 >> p
              BVal True -> rout (BVal True) >> p)
  in p 

--1
interMergeProc :: Int -> Proc ()
interMergeProc c = 
  let p = rin 0 (\x -> 
            case x of 
              BVal True -> mapM_ uInOutx [1..c-1] >> rout (BVal True) >> p       
              BVal False ->  do rout (BVal False) 
                                mapM_ uInOutx [0..c-1] 
                                rout (BVal True) 
                                p )
  in p

--2
segInterProc :: [(Int,Int)] -> Proc ()
segInterProc cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> mapM_ segInterP (tail cs) >> p )
   in p      


segInterP :: (Int, Int) -> Proc ()
segInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> Done ())
  in p 

--3
priSegInterProc :: [(Int, Int)] -> Proc ()
priSegInterProc cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterProc" j >>= rout >> p 
              BVal True -> mapM_ priSegInterP (tail cs) >> p )
      in p 


priSegInterP :: (Int, Int) -> Proc ()
priSegInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterP" j >>= rout >> p
              BVal True -> Done ())
  in p   


isEmptyProc :: Proc ()
isEmptyProc = 
  rin 0 (\x -> 
    case x of 
      BVal False -> rout x >> uInx 0 >> isEmptyProc 
      BVal True -> rout x >> isEmptyProc)



-------------

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

