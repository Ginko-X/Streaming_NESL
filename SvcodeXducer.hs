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
  _ == _ = False 



-- Eos is not allowed to read 
rinx :: String -> Int -> Xducer AVal
rinx proc i = Pin i (\ x -> 
  case x of 
    Nothing -> error $ proc ++ " premature end of stream" 
    Just a -> Done a)


rout :: AVal -> Xducer ()
rout a = Pout a (Done ()) 

routF = rout (BVal False)
routT = rout (BVal True)

done = Done ()


loop0 :: Xducer () -> Xducer ()
loop0 xd = Pin 0 (\ x -> 
             case x of 
              Just _ -> xd >> loop0 xd
              Nothing -> done)


loopu :: Int -> Xducer () -> Xducer () -> Xducer ()
loopu i xdF xdT = p 
  where p = do BVal b <- rinx "loopu" i 
               if b then xdT 
               else xdF >> p 


-- only for scanPlus and reducePlus
loopuv :: Int -> (Int -> Xducer Int) -> (Int -> Xducer ()) -> Xducer ()
loopuv i xdF xdT = p 0
  where p acc = do BVal b <- rinx "loopuv" i 
                   if b then xdT acc
                   else xdF acc >>= p 


-- must read and output an unary (excluding the last T flag)
uInOutx :: Int -> Xducer ()
uInOutx i = loopu i routF done


-- must read an unary and throw it away (no Eos)
uInx :: Int -> Xducer ()
uInx i = loopu i done done


uOutx :: Int -> AVal -> Xducer ()
uOutx i a = loopu i (rout a) done


-- repeat outputing `as` until read a `True` from channel `i` 
uOutsx :: Int -> [AVal] -> Xducer ()
uOutsx i as = loopu i (mapM_ rout as) done

 
-- read an unary (and throw it away) from `i`
-- at the same time interlacedly read an equal number of elements from `j`
uInInter :: Int -> Int -> Xducer ()
uInInter i j = loopu i (rinx "uInInter" j >> done) (rinx "uInInter" j >> done)



-------------- Xducers ---------------------

constXducerN :: AVal -> Xducer ()
constXducerN a = loop0 $ rout a 


-- toFlags.
toFlagsN = loop0 p  
  where p = do (IVal a) <- rinx "toFlagsXducer(data)" 1 
               mapM_ rout [BVal b | b <- i2flags a]


-- usumXducer.
usumXducerN = loop0 $ loopu 1 (routF) done  


-- mapTwoN :: ([AVal] -> AVal) -> Xducer ()
mapTwoN op = loop0 p
  where p = do x <- rinx "mapTwo(x)" 1
               y <- rinx "mapTwo(y)" 2
               rout (op [x,y])


-- mapOne.
mapOneN op = loop0 p 
  where p = do x <- rinx "mapOne(x)" 1 
               rout (op [x])


-- checkXducer.
checkXducerN = loop0 p 
  where p = do x <- rinx "checkXducer(x)" 1 
               y <- rinx "checkXducer(y)" 2 
               if x == y then return ()
               else error  "checkXducer: runtime error"


-- packXducer.
-- need special ctrl
packXducerN = loop0 p 
  where p = do BVal x <- rinx "packXducer(flag)" 1
               y <- rinx "packXducer(data)" 2
               if x then rout y 
               else return ()


-- upackXducer.
upackXducerN = loop0 p 
  where p = do BVal x <- rinx "upackXducer(flag)" 1 
               if x then loopu 2 routF routT
               else loopu 2 done done


-- pdistXducerN :: Xducer ()
pdistXducerN = loop0 p 
  where p = do x <- rinx "pdistXducer(x)" 1
               loopu 2 (rout x) done


-- b2uXducer
b2uXducerN = loop0 p 
  where p = do (BVal x) <- rinx "b2uXducer(x)" 1
               if x then routF >> routT
               else routT


-- segScanPlusXducer.
segScanPlusXducerN = loop0 $ loopuv 1 xdF (\_ -> done)
  where xdF acc = do rout (IVal acc)
                     IVal a <- rinx "segScanPlusXducer(data)" 2
                     return (acc + a) 


-- segReducePlusXducer
segReducePlusXducerN = loop0 $ loopuv 1 xdF xdT
  where xdF acc = do IVal a <- rinx "segReducePlusXducer(data)" 2 
                     return (acc + a)
        xdT acc = rout $ IVal acc 


--segConcatXducer :: Xducer ()
segConcatXducerN  = loop0 $ loopu 1 (uInOutx 2) (routT)


--uSegCountXducer :: Xducer ()
uSegCountXducerN = loop0 p
  where p = loopu 1 u routT
        u = do BVal b2 <- rinx "uSegCountXducerN(data)" 2
               routF   -- eager output 
               if b2 then return ()
               else uInInter 2 1 


--segMergeXducer :: Xducer ()
segMergeXducerN = segConcatXducerN


--interMergeXducer :: Int -> Xducer ()
interMergeXducerN c = loop0 $ mapM_ uInOutx [1..c-1] >> routT


--segInterXducer :: [(Int,Int)] -> Xducer ()
segInterXducerN cs = loop0 $ mapM_ segInterP cs 
  where segInterP (j,i) = loopu i (uInOutx j >> routT) done
 

--priSegInterXducer :: [(Int, Int)] -> Xducer ()
priSegInterXducerN cs = loop0 $ mapM_ priSegInterP cs 
   where priSegInterP (j,i) = loopu i (rinx "priSegInterP" j >>= rout) done
 

--isEmptyXducer :: Xducer ()
isEmptyXducerN = loop0 p 
  where p = do BVal b <- rinx "isEmptyXducer(data)" 1
               rout (BVal b)  -- eager output
               if b then return ()
               else uInx 1



-- for sequence distribution, may be not necessary to support
-- 1.
segDistrXducerN :: Xducer ()
segDistrXducerN = loop0 p 
  where p = do vs <- uRecordx 1 
               uOutsx 2 vs 

-- 2.
segFlagDistrXducerN = loop0 (p []) 
  where p vs = do BVal b1 <- rinx "segFlagDistrXducer(flag1)" 1 
                  if not b1 then do vs' <- uRecordx 2; p (vs++vs')
                  else uOutsx 3 vs 

-- 3.
primSegFlagDistrXducerN = loop0 (p [])
  where p vs = do BVal b1 <- rinx "primSegFlagDistrXducer(flag1)" 1 
                  if not b1 then do y <- rinx "primSegFlagDistrXducer(data)" 2; p (vs++[y])
                  else uOutsx 3 vs 

-- read and return an unary (excluding the last T flag)
uRecordx :: Int -> Xducer [AVal]
uRecordx i =
  let p vs = do x <- rinx "uRecordx" i 
                case x of 
                  (BVal False) -> p ((BVal False):vs)  
                  (BVal True) -> Done (reverse (BVal True : vs))
  in (p [])



---------------------------

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

