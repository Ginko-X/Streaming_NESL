{- Svcode Interpreter 
 + TODO: add more instructions -}

module SvcodeInterp where

import SvcodeSyntax
import SneslSyntax
import Control.Monad

-- ?? low-level execution
--data Buffer = Buffer {start :: Int, len :: Int, is_end :: Bool}
--type BufferStore = [(SId, Buffer)]
--type AccStore = [(SId, SvVal)]
--type CursorMap = [(SId, SId, SvVal)]
--data Svctx = Svctx {buffers :: BufferStore, accs :: AccStore, cursors :: CursorMap}  


type Svctx = [(SId, SvVal)]

newtype Svcode a = Svcode {rSvcode :: Svctx -> Either (a, Svctx) String}

instance Monad Svcode where
    return a = Svcode $ \ c -> Left (a, c)

    m >>= f = Svcode $ \ c -> 
        case rSvcode m c of 
            Left (a, c') -> case rSvcode (f a) c' of 
                               Left (b, c'') -> Left (b, c'')
                               Right err' -> Right err'      
            Right err -> Right err

instance Functor Svcode where
  fmap f t = t >>= return . f

instance Applicative Svcode where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


runProg :: SSym -> Either Svctx String
runProg (SSym sdefs st) = case rSvcode (mapM_ sdefInterp sdefs) [] of 
    Left (_,sids) -> 
        case lookupTree st sids of                      
            [] -> Right "Returns are empty or some stream definition does not exist." 
            vs -> Left vs
    Right err -> Right err 


lookupTree :: STree -> Svctx -> Svctx
lookupTree STnil _ = []
lookupTree (STcons s st) ctx = case lookup s ctx of 
    Just v -> (s,v) : (lookupTree st ctx) 
    Nothing -> []



sdefInterp :: SDef -> Svcode SvVal
sdefInterp (SDef sid Ctrl) = Svcode $ \c ->  
    Left (sv, c++[(sid,sv)])
        where sv = SSVal [IVal 1]   -- Ctrl stream should be wildcards


-- MapConst: Map the const 'a' to the stream 'sid2'
sdefInterp (SDef sid1 (MapConst sid2 a)) = Svcode $ \ c ->
    case lookup sid2 c of 
        Nothing -> Right $ "Referring to a stream that does not exist: " ++ show sid2
        Just (SSVal sv2) -> Left (sv1,c ++ [(sid1, sv1)])
            where sv1 = SSVal $ replicate (length sv2) a 


-- toflags: generate flag segments for a stream of integers
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
sdefInterp (SDef sid1 (ToFlags sid2)) = Svcode $ \ c ->
    case lookup sid2 c of 
        Nothing -> Right $ "Referring to a stream that does not exist: " ++ show sid2
        Just (SSVal sv2) -> Left (sv1, c ++ [(sid1,sv1)]) 
            where sv1 = SSVal $ concat $ map si2sb sv2 
                  si2sb (IVal i) = replicate i (BVal False) ++ [BVal True]


-- iotas: segmemt iota on segment flags
-- e.g. <F,F,F,T, F,F,T> => <0,1,2,0,1>  
sdefInterp (SDef sid1 (Iotas sid2)) = Svcode $ \ c ->
    case lookup sid2 c of 
        Nothing -> Right $ "Referring to a stream that does not exist: " ++ show sid2
        Just (SSVal sv2) -> Left (sv1, c ++ [(sid1,sv1)])
            where sv1 = SSVal $ segExScanPlus sv2'
                  sv2' = map sb2si sv2
                  sb2si (BVal b) = IVal (if b then 0 else 1)




-- segment exclusive scan for plus; segment delimiter is 0
segExScanPlus :: [AVal] -> [AVal]
segExScanPlus  = undefined



exampleProg = SSym defs (STcons 2 STnil)
defs = [SDef 0 Ctrl, 
        SDef 1 (MapConst 0 (IVal 100)),
        SDef 2 (ToFlags 1)]






