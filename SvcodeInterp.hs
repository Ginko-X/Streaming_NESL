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


instance Applicative Svcode where
    pure = return 
    (<*>) = ap 

instance Functor Svcode where
    fmap = liftM


runProg :: SSym -> Either SvVal String
runProg (SSym sdefs sid) = case rSvcode (mapM_ sdefInterp sdefs) [] of 
    Left (_,sids) -> 
        case lookup sid sids of 
            Just stream -> Left stream
            Nothing -> Right "Referring to a stream that does not exist." 
    Right err -> Right err 


sdefInterp :: SDef -> Svcode SvVal
sdefInterp (SDef sid Ctrl) = Svcode $ \ c -> 
    case lookup sid c of 
        Just _ -> Right "Trying to define a stream that already exists."
        Nothing -> Left (sv, c++[(sid,sv)])
            where sv = SSVal [IVal 0]   -- Ctrl stream should be a wildcard


sdefInterp (SDef sid1 (Rep sid2 a)) = Svcode $ \ c ->
    case lookup sid1 c of 
        Just _ -> Right "Trying to define a stream that already exists." 
        Nothing -> case lookup sid2 c of 
            Nothing -> Right "Referring to a stream that does not exist."  
            Just (SSVal sv2) -> Left (sv1,c ++ [(sid1, sv1)])
                where sv1 = SSVal $ replicate (length sv2) a 


-- toflags: generate flags segment for each segment length in a given int seq
-- e.g. <1,4,0,2> => <F,T,F,F,F,F,T,T,F,F,T>
sdefInterp (SDef sid1 (ToFlags sid2)) = Svcode $ \ c ->
    case lookup sid2 c of 
        Nothing -> Right "Referring to a stream that does not exist."  
        Just (SSVal sv2) -> Left (sv1, c ++ [(sid1,sv1)])  -- need to check redefinition
            where sv1 = SSVal $ concat $ map si2sb sv2 
                  si2sb (IVal i) = replicate i (BVal False) ++ [BVal True]


-- iotas: segmemt iota on segment flags
-- e.g. <F,F,F,T, F,F,T> => <0,1,2,0,1>  
sdefInterp (SDef sid1 (Iotas sid2)) = Svcode $ \ c ->
    case lookup sid2 c of 
        Nothing -> Right "Referring to a stream that does not exist."  
        Just (SSVal sv2) -> Left (sv1, c ++ [(sid1,sv1)])
            where sv1 = segExScanPlus sv2'
                  sv2' = map sb2si sv2
                  sb2si (BVal b) = IVal (if b then 0 else 1)


-- segment exclusive scan for plus; segment delimiter is 0
segExScanPlus :: [AVal] -> [AVal]
--segExScanPlus 



exampleProg = SSym defs "s2"
defs = [SDef "s" Ctrl, 
        SDef "s1" (Rep "s" (IVal 100)),
        SDef "s2" (ToFlags "s1")]






