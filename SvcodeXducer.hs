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


loop0 :: Xducer () -> Xducer ()
loop0 xd = rin 0 (\ _ -> xd >> loop0 xd)


mapConst :: AVal -> Xducer ()
mapConst a = p
  where p = rin 0 (\x -> rout a >> p) 

constXducerN :: AVal -> Xducer ()
constXducerN a = loop0 p 
  where p = rout a 


-- toFlags.
toFlags :: Xducer ()
toFlags = p 
  where p = rin 0 (\(IVal a) -> mapM_ rout [BVal b | b <- i2flags a] >> p)

toFlagsN = loop0 p  
  where p = do (IVal a) <- rinx "toFlagsXducer(x)" 1 
               mapM_ rout [BVal b | b <- i2flags a]

-- usumXducer.
usumXducer :: Xducer ()
usumXducer = 
    rin 0 (\x -> 
      case x of 
        BVal False -> rout x >> usumXducer
        BVal True -> usumXducer)

-- ?!
usumXducerN = loop0 p 
  where p = uInOutx 1 



-- mapTwo.
mapTwo :: ([AVal] -> AVal) -> Xducer ()
mapTwo op = 
    rin 0 (\x -> 
      do y <- rinx "mapTwo" 1 
         rout (op [x,y])
         mapTwo op)

mapTwoN op = loop0 p
  where p = do x <- rinx "mapTwo(x)" 1
               y <- rinx "mapTwo(y)" 2
               rout (op [x,y])


-- mapOne.
mapOne :: ([AVal] -> AVal) -> Xducer ()
mapOne op = rin 0 (\x -> rout (op [x]) >> mapOne op)

mapOneN op = loop0 p 
  where p = do x <- rinx "mapOne(x)" 1 
               rout (op [x])

-- checkXducer.
checkXducer :: Xducer ()
checkXducer = rin 0 (\x -> 
              do y <- rinx "checkXducer" 1 
                 if x == y 
                   then checkXducer 
                   else error "checkXducer: runtime error")

checkXducerN = loop0 p 
  where p = do x <- rinx "checkXducer(x)" 1 
               y <- rinx "checkXducer(y)" 2 
               if x == y then return ()
               else error  "checkXducer: runtime error"

-- packXducer.
packXducer :: Xducer ()
packXducer = rin 0 (\x -> 
              case x of 
                BVal False -> rinx "packXducer" 1 >> packXducer
                BVal True -> rinx "packXducer" 1 >>= rout >> packXducer)

-- with special ctrl
packXducerN = loop0 p 
  where p = do BVal x <- rinx "packXducer(flag)" 1
               y <- rinx "packXducer(data)" 2
               if x then rout y 
               else return ()


-- upackXducer.
upackXducer :: Xducer ()
upackXducer = rin 0 (\x -> 
              case x of 
                BVal False -> uInx 1 >> upackXducer
                BVal True -> uInOutx 1 >> rout x >> upackXducer)

upackXducerN = loop0 p 
  where p = do (BVal x) <- rinx "upackXducer(flag)" 1 
               if x then uInOutx 2 >> rout (BVal True) 
               else uInx 2


-- must read an unary and throw it away (no Eos)
uInx :: Int -> Xducer ()
uInx i = do x <- rinx "uInx" i 
            case x of 
              BVal False -> uInx i
              BVal True -> Done ()

-- must read and output an unary (excluding the last T flag)
uInOutx :: Int -> Xducer ()
uInOutx i = do x <- rinx "uInOutx" i
               case x of 
                 BVal False -> rout x >> (uInOutx i)
                 BVal True -> Done ()
      

-- pdistXducer.
pdistXducer :: Xducer ()
pdistXducer = 
  rin 0 (\x -> 
    do y <- rinx "pdistXducer" 1 
       case y of 
         BVal False -> rout x >> (uOutx 1 x) >> pdistXducer
         BVal True -> pdistXducer)

pdistXducerN = loop0 p 
  where p = do x <- rinx "pdistXducer(x)" 1
               uOutx 2 x 
 

uOutx :: Int -> AVal -> Xducer ()
uOutx i a = do x <- rinx "uOutx" i
               case x of 
                 BVal False -> rout a >> (uOutx i a)
                 BVal True -> Done ()


-- for sequence distribution, may be not necessary to support
-- 1.
segDistrXducer :: Xducer ()
segDistrXducer = p 
  where p = do vs <- uRecord 0
               if null vs then Done ()
               else  
                 do y <- rinx "segDistrXducer" 1
                    case y of
                      BVal False -> mapM_ rout vs >> uOutsx 1 vs >> p 
                      BVal True -> p

-- ??!
segDistrXducerN = loop0 p 
  where p = do vs <- uRecordx 1 
               uOutsx 2 vs 

-- 2.
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

segFlagDistrXducerN = loop0 (p []) 
  where p vs = do BVal b1 <- rinx "segFlagDistrXducer(flag1)" 1 
                  if not b1 then do vs' <- uRecordx 2; p (vs++vs')
                  else uOutsx 3 vs 
             


-- 3.
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

primSegFlagDistrXducerN = loop0 (p [])
  where p vs = do BVal b1 <- rinx "primSegFlagDistrXducer(flag1)" 1 
                  if not b1 then do y <- rinx "primSegFlagDistrXducer(data)" 2; p (vs++[y])
                  else uOutsx 3 vs 


-- read and return an unary (excluding the last T flag)
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


-- repeat outputing `as` until read a `True` from channel `i` 
uOutsx :: Int -> [AVal] -> Xducer ()
uOutsx i as = do x <- rinx "uOutsx" i        
                 case x of 
                   BVal False -> mapM_ rout as >> (uOutsx i as)
                   BVal True -> Done ()

-- b2uXducer.
b2uXducer :: Xducer ()
b2uXducer =
  rin 0 (\x -> 
    case x of 
      BVal True -> rout (BVal False) >> rout (BVal True) >> b2uXducer 
      BVal False -> rout (BVal True) >> b2uXducer)

b2uXducerN = loop0 p 
  where p = do (BVal x) <- rinx "b2uXducer(x)" 1
               if x then rout (BVal False) >> rout (BVal True)
               else rout (BVal True)

-- segScanPlusXducer.
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


segScanPlusXducerN = loop0 (p 0)
  where p acc = do BVal b <- rinx "segScanPlusXducer(flag)" 1
                   if b then return ()
                   else do IVal a <- rinx "segScanPlusXducer(data)" 2
                           rout (IVal acc)
                           p (acc + a)


-- segReducePlusXducer.
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

segReducePlusXducerN = loop0 (p 0)
  where p acc = do BVal b <- rinx "segReducePlusXducer(flag)" 1
                   if b then rout (IVal acc)
                   else do IVal a <- rinx "segReducePlusXducer(data)" 2 
                           p (acc + a)

-- segConcatXducer.
segConcatXducer :: Xducer ()
segConcatXducer = 
    rin 0 (\x -> 
      case x of 
        BVal False -> uInOutx 1 >> segConcatXducer
        BVal True -> rout x >> segConcatXducer)

segConcatXducerN  = loop0 p 
  where p = do BVal b <- rinx "segConcatXducer(flag)" 1 
               if b then rout (BVal True)
               else uInOutx 2 >> p
                

-- uSegCountXducer.
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

uSegCountXducerN = loop0 p
  where p = do BVal b1 <- rinx "uSegCountXducerN(flag)" 1 
               if b1 then rout (BVal True)
               else do BVal b2 <- rinx "uSegCountXducerN(data)" 2
                       rout (BVal False)
                       if b2 then p
                       else uInInter 2 1 >> p


-- read an unary (and throw it away) from `i`
-- at the same time interlacedly read an equal number of elements from `j`
uInInter :: Int -> Int -> Xducer ()
uInInter i j = 
    do x <- rinx "uInInter" i 
       case x of
         BVal False -> rinx "uInInter" j >> uInInter i j
         BVal True -> rinx "uInInter" j >> Done ()


-- segMergeXducer.
segMergeXducer :: Xducer ()
segMergeXducer = 
  let p = rin 0 (\x -> 
            case x of 
              BVal False -> uInOutx 1 >> p
              BVal True -> rout x >> p)
  in p 

segMergeXducerN = loop0 p 
  where p = do BVal b <- rinx "segMergeXducer(flag)" 1
               if b then rout (BVal True)
               else uInOutx 2 >> p


-- interMergeXducer.
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

-- this code will loop forever:
-- let p = do mapM_ uInOut [0..c-1]
--            rout (BVal True)
--            p 
-- in p

interMergeXducerN c = loop0 p 
  where p = do mapM_ uInOutx [1..c-1] 
               rout (BVal True)


-- segInterXducer ?
segInterXducer :: [(Int,Int)] -> Xducer ()
segInterXducer cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> mapM_ segInterP (tail cs) >> p )
   in p      

segInterXducerN cs = loop0 p 
  where p = mapM_ segInterP cs 



segInterP :: (Int, Int) -> Xducer ()
segInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> uInOutx j >> rout (BVal True) >> p 
              BVal True -> Done ())
  in p 


-- . ?
priSegInterXducer :: [(Int, Int)] -> Xducer ()
priSegInterXducer cs = 
  let (j,i) = head cs 
      p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterXducer" j >>= rout >> p 
              BVal True -> mapM_ priSegInterP (tail cs) >> p )
  in p 

priSegInterXducerN cs = loop0 p 
  where p = mapM_ priSegInterP cs 
   

priSegInterP :: (Int, Int) -> Xducer ()
priSegInterP (j,i) = 
  let p = rin i (\x -> 
            case x of 
              BVal False -> rinx "priSegInterP" j >>= rout >> p
              BVal True -> Done ())
  in p   


-- .
isEmptyXducer :: Xducer ()
isEmptyXducer = 
  rin 0 (\x -> 
    case x of 
      BVal False -> rout x >> uInx 0 >> isEmptyXducer 
      BVal True -> rout x >> isEmptyXducer)

isEmptyXducerN = loop0 p 
  where p = do BVal b <- rinx "isEmptyXducer(data)" 1
               rout (BVal b)
               if b then return ()
               else uInx 1



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

