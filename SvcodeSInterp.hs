{- SVCODE Streaming Interpreter -}

module SvcodeSInterp where

import SvcodeSyntax
import SneslSyntax
import SneslCompiler (tree2Sids)
import DataTrans (i2flags)
import SneslInterp (flags2len, seglist)

import SvcodeInterp (usum, segSum,streamLen, ppack)

import Control.Monad
import Data.List (transpose)
import Data.Bits ((.&.))


data Proc a = Pin (Int,Int) (Maybe AVal -> Proc a)
            | Pout (Maybe AVal) (Proc a)
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



mapConst :: AVal -> Proc ()
mapConst a =       
  let p i = Pin (0,i) (\x -> 
              case x of  
                Nothing -> Done ()
                Just _ -> Pout (Just a) (p (i+1)))
     in (p 0)  



toFlags :: Proc ()
toFlags = 
  let p i = Pin (0,i) (\x ->
              case x of 
                Nothing -> Done ()
                Just (IVal a) -> bSeqOut (i2flags a) (p (i+1)) ) 
   in (p 0)


bSeqOut :: [Bool] -> Proc () -> Proc ()
bSeqOut [] p = p
bSeqOut (b:bs) p = Pout (Just $ BVal b) (bSeqOut bs p) 



segScanPlus :: Proc ()
segScanPlus = 
  let p i acc = Pin (0,i) (\x -> 
                  case x of 
                    Nothing -> Done ()
                    Just (IVal a) -> 
                      Pin (1,i) (\y -> 
                        case y of 
                          Nothing -> Done ()
                          Just (BVal False) -> Pout (Just $ IVal $ acc) (p (i+1) (a+acc))
                          Just (BVal True) -> Pout (Just $ IVal 0) (p (i+1) a)))  -- bug
  in (p 0 0)




evalProc :: Proc () -> [SvVal] -> SvVal -> SvVal
evalProc (Pin (x,y) p) ss s = 
  let p' = p (pread y (ss !! x))
   in evalProc p' ss s

evalProc (Pout a p) ss s = 
  let s0 = pwrite a
  in  evalProc p ss (concatSv s s0)

evalProc (Done ()) _ s = s 



pread :: Int -> SvVal -> Maybe AVal
pread i (SIVal is) = 
  if length is < i+1 then Nothing 
    else Just $ IVal (is !! i)

pread i (SBVal bs) = 
  if length bs < i+1 then Nothing 
    else Just $ BVal (bs !! i)


pwrite :: Maybe AVal -> SvVal 
pwrite (Just (IVal i)) = SIVal [i]
pwrite (Just (BVal b)) = SBVal [b]
-- pwrite Nothing = 



concatSv :: SvVal -> SvVal -> SvVal
concatSv (SIVal a) (SIVal b) = SIVal (a++b)
concatSv (SBVal a) (SBVal b) = SBVal (a++b)
concatSv (SPVal v1 v2) (SPVal v3 v4) = SPVal (concatSv v1 v3) (concatSv v2 v4)
concatSv (SSVal v1 b1) (SSVal v2 b2) = SSVal (concatSv v1 v2) (b1++b2)




----- Streaming Interpreter -----
{-

type Svctx = [(SId, SvVal)]

newtype SvcodeS a = SvcodeS {rSvcodeS :: Svctx -> SId -> 
                                           Either String (a, Svctx)}


sExpInterp :: SExp -> SvcodeS SvVal
sExpInterp (MapConst s1 a) = 
  do v1 <- lookupSid s1
     return evalProc (mapConst a) [v1]

sExpInterp (ToFlags s1) = 
  do v1 <- lookupSid s1 
     return evalProc toFlags [v1]

-}






