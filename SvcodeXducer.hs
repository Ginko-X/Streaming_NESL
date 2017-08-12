{- SVCODE transducers -}

module SvcodeXducer where

import SvcodeSyntax
import SneslSyntax
import DataTrans (i2flags)


data Xducer a = Pin Int (Maybe AVal -> Xducer a)  
            | Pout AVal (Xducer a)  
            | Done a 


instance Monad Xducer where
  return a = Done a

  (Pin i g) >>= f = Pin i (\x -> g x >>= f) 
  (Pout o p) >>= f = Pout o (p >>= f)
  Done a >>= f = f a 


instance (Show a) => Show (Xducer a) where
  show (Pin i p) = "<Pin " ++ show i ++ " <Xducer>>"
  show (Pout a p) = "<Pout " ++ show a ++ " " ++ show p ++ "> "
  show (Done a) = "<Done " ++ show a ++ "> "


instance Functor Xducer where
  fmap f t = t >>= return . f

instance Applicative Xducer where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


instance (Eq a) => Eq (Xducer a) where
  Done a == Done b = a == b
  Pout a p1 == Pout b p2 = (a == b) && (p1 == p2)
  _ == _ = False  -- ??




rin :: Int -> (AVal -> Xducer ()) -> Xducer ()
rin i p = Pin i (\x ->
  case x of 
    Nothing -> Done ()
    Just v -> p v)

-- Eos is not allowed to read 
rinx :: String -> Int -> Xducer AVal
rinx proc i = Pin i (\ x -> 
  case x of 
    Nothing -> error $ proc ++ " premature end of stream" 
    Just a -> Done a)


rout :: AVal -> Xducer ()
rout a = Pout a (Done ())


rinOut :: Xducer ()
rinOut = rin 0 (\x -> rout x >> rinOut)
 

mapConst :: AVal -> Xducer ()
mapConst a = p
  where p = rin 0 (\x -> rout a >> p) 



toFlags :: Xducer ()
toFlags = p 
  where p = rin 0 (\(IVal a) -> mapM_ rout [BVal b | b <- i2flags a] >> p)



usumXducer :: Xducer ()
usumXducer = p
  where p = rin 0 (\x -> 
              case x of 
                BVal False -> rout (BVal False) >> p 
                BVal True -> p)


-- the 2nd supplier stream can not be shorter (but can be longer ??!) than 1st
mapTwo :: ([AVal] -> AVal) -> Xducer ()
mapTwo op = p 
  where p = rin 0 (\x -> 
              do y <- rinx "mapTwo" 1 
                 rout (op [x,y])
                 p)



mapOne :: ([AVal] -> AVal) -> Xducer ()
mapOne op = p 
  where p = rin 0 (\x -> rout (op [x]) >> p)



checkXducer :: Xducer ()
checkXducer = rin 0 (\x -> 
              do y <- rinx "checkXducer" 1 
                 if x == y 
                   then checkXducer 
                   else error "checkXducer: runtime error")



packXducer :: Xducer ()
packXducer =  rin 0 (\x -> 
              case x of 
                BVal False -> rinx "packXducer" 1 >> packXducer
                BVal True -> rinx "packXducer" 1 >>= rout >> packXducer)


upackXducer :: Xducer ()
upackXducer = rin 0 (\x -> 
              case x of 
                BVal False -> uInx 1 >> upackXducer
                BVal True -> uInOutx 1 >> rout (BVal True) >> upackXducer)


-- read an unary and throw it away (can be Eos)
uIn :: Int -> Xducer ()
uIn i = rin i (\x ->  
          case x of 
            BVal False -> uIn i
            BVal True -> Done ())


-- must read an unary and throw it away (no Eos)
uInx :: Int -> Xducer ()
uInx i = do x <- rinx "uInx" i 
            case x of 
              BVal False -> uInx i
              BVal True -> Done ()

-- must read and output an unary (no Eos)
uInOutx :: Int -> Xducer ()
uInOutx i = do x <- rinx "uInOutx" i
               case x of 
                 BVal False -> rout x >> (uInOutx i)
                 BVal True -> Done ()


pdistXducer :: Xducer ()
pdistXducer = p
  where p = rin 0 (\x -> 
              do y <- rinx "pdistXducer" 1 
                 case y of 
                   BVal False -> rout x >> (uOutx 1 x) >> p 
                   BVal True -> p)
 

uOutx :: Int -> AVal -> Xducer ()
uOutx i a = do x <- rinx "uOutx" i
               case x of 
                 BVal False -> rout a >> (uOutx i a)
                 BVal True -> Done ()



segDistrXducer :: Xducer ()
segDistrXducer = p 
  where p = do vs <- uRecord 0
               if null vs then Done ()
               else  
                 do y <- rinx "segDistrXducer" 1
                    case y of
                      BVal False -> mapM_ rout vs >> uOutsx 1 vs >> p 
                      BVal True -> p



segFlagDistrXducer :: Xducer ()
segFlagDistrXducer = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> do vs' <- uRecordx 1; p (vs++vs')
                   BVal True -> 
                     do y <- rinx "segFlagDistrXducer" 2
                        case y of 
                          BVal False -> mapM_ rout vs >> uOutsx 2 vs >> (p [])
                          BVal True -> p [])



primSegFlagDistrXducer :: Xducer ()
primSegFlagDistrXducer = p []
  where p vs = rin 0 (\x -> 
                 case x of 
                   BVal False -> do y <- rinx "primSegFlagDistrXducer" 1; p (vs ++ [y])
                   BVal True -> 
                     do y <- rinx "primSegFlagDistrXducer" 2
                        case y of 
                          BVal False -> mapM_ rout vs >> uOutsx 2 vs >> (p [])
                          BVal True -> p [])



-- read and output an unary (without the last True flag)
uRecord :: Int -> Xducer [AVal]
uRecord i =
  let p vs = Pin i (\x -> 
               case x of 
                 Nothing -> Done vs 
                 Just (BVal False) -> p ((BVal False):vs)  
                 Just (BVal True) -> Done (reverse (BVal True : vs)))
  in (p [])


uRecordx :: Int -> Xducer [AVal]
uRecordx i =
  let p vs = do x <- rinx "uRecordx" i 
                case x of 
                  (BVal False) -> p ((BVal False):vs)  
                  (BVal True) -> Done (reverse (BVal True : vs))
  in (p [])


-- repeat `as` until read a `True` from channel `i` 
-- and don't allow Eos when reading channel `i` 
uOutsx :: Int -> [AVal] -> Xducer ()
uOutsx i as = do x <- rinx "uOutsx" i        
                 case x of 
                   BVal False -> mapM_ rout as >> (uOutsx i as)
                   BVal True -> Done ()


b2uXducer :: Xducer ()
b2uXducer = p 
  where p = rin 0 (\x -> 
              case x of 
                BVal True -> rout (BVal False) >> rout (BVal True) >> p 
                BVal False -> rout (BVal True) >> p)



segScanPlusXducer :: Xducer ()
segScanPlusXducer = p 0 
  where p acc = rin 0 (\x -> 
                  case x of 
                    BVal True -> p 0
                    BVal False -> 
                      do y <- rinx "segScanPlusXducer" 1 
                         case y of 
                           IVal a -> rout (IVal acc) >> p (a+acc)
                           _ -> error "segScanPlusXducer: read some non-integer")



segReducePlusXducer :: Xducer ()
segReducePlusXducer = 
  let p acc = rin 0 (\x -> 
                case x of 
                  BVal True -> rout (IVal acc) >> (p 0)
                  BVal False -> 
                    do y <- rinx "segReducePlusXducer" 1 
                       case y of 
                         IVal a -> p (acc + a)
                         _ -> error "segReducePlusXducer: read some non-integer")
  in (p 0)


segConcatXducer :: Xducer ()
segConcatXducer = 
  let p = rin 0 (\x -> 
            case x of 
              BVal False -> uInOutx 1 >> p 
              BVal True -> rout (BVal True) >> p)
  in p  
                

uSegCountXducer :: Xducer ()
uSegCountXducer = 
  let p = rin 0 (\x -> 
            case x of 
              BVal True -> rout x >> p 
              BVal False -> 
                do y <- rinx "uSegCountXducer" 1
                   case y of 
                     BVal False -> rout y >> uInInter 1 0 >> p
                     BVal True -> rout (BVal False) >> p)

  in p 


-- read an unary (and throw it away) from `i`
-- at the same time interlacedly read an equal number of elements from `j`
uInInter :: Int -> Int -> Xducer ()
uInInter i j = 
    do x <- rinx "uInInter" i 
       case x of
         BVal False -> Pin j (\_ -> uInInter i j)
         BVal True -> Pin j (\_ -> Done ())



segMergeXducer :: Xducer ()
segMergeXducer = 
  let p = rin 0 (\x -> 
            case x of 
              BVal False -> uInOutx 1 >> p
              BVal True -> rout (BVal True) >> p)
  in p 

--1
interMergeXducer :: Int -> Xducer ()
interMergeXducer c = 
  let p = rin 0 (\x -> 
            case x of 
              BVal True -> mapM_ uInOutx [1..c-1] >> rout (BVal True) >> p       
              BVal False ->  do rout (BVal False) 
                                mapM_ uInOutx [0..c-1] 
                                rout (BVal True) 
                                p )
  in p

--2
segInterXducer :: [(Int,Int)] -> Xducer ()
segInterXducer cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> mapM_ segInterP (tail cs) >> p )
   in p      


segInterP :: (Int, Int) -> Xducer ()
segInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> Done ())
  in p 

--3
priSegInterXducer :: [(Int, Int)] -> Xducer ()
priSegInterXducer cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterXducer" j >>= rout >> p 
              BVal True -> mapM_ priSegInterP (tail cs) >> p )
      in p 


priSegInterP :: (Int, Int) -> Xducer ()
priSegInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterP" j >>= rout >> p
              BVal True -> Done ())
  in p   


isEmptyXducer :: Xducer ()
isEmptyXducer = 
  rin 0 (\x -> 
    case x of 
      BVal False -> rout x >> uInx 0 >> isEmptyXducer 
      BVal True -> rout x >> isEmptyXducer)



-------------

evalXducer :: Xducer () -> [SvVal] -> SvVal -> SvVal
evalXducer (Pin i p) ss s = 
  let (a, tailS) = (pread $ ss !! i )
      ss' =  updateList ss i tailS
   in evalXducer (p a) ss' s

evalXducer (Pout a p) ss s = 
  evalXducer p ss (pwrite a s) 

evalXducer (Done ()) _ s = reverseSv s 



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

