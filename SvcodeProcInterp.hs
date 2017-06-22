{- SVCODE Streaming Interpreter using Proc -}

module SvcodeProcInterp where

import SvcodeSyntax
import SneslSyntax
import SvcodeProc

import Control.Monad

data BufState = Maybe AVal | EmptyBuf | Eos 
type SState = (BufState, [Bool], Proc ())
type Svctx = [(SId, SState)]
type Dag = [[SId]]
type CTable = [[SId]] -- channel table


getChan :: SExp -> SId -> [SId]
getChan Ctrl _ = []
getChan EmptyCtrl _ = []
getChan (Const _) c = [c]

getChan (MapConst s1 _) _ = [s1]
getChan (MapOne _ s1) _ = [s1]
getChan (MapTwo _ s1 s2) _ = [s1,s2]

getChan (InterMergeS ss) _ = ss 
getChan (SegInterS ss) _ = concat $ map (\(x,y) -> [x,y]) ss 
getChan (PriSegInterS ss) _ = concat $ map (\(x,y) -> [x,y]) ss 

getChan (Distr s1 s2) _ = [s1,s2]
getChan (SegDistr s1 s2) _ = [s1,s2]
getChan (SegFlagDistr s1 s2 s3) _ = [s2,s1,s3]
getChan (PrimSegFlagDistr s1 s2 s3) _ = [s2,s1,s3]

getChan (ToFlags s1) _ = [s1] 
getChan (Usum s1) _ = [s1]
getChan (B2u s1) _ = [s1]

getChan (SegscanPlus s1 s2) _ = [s2,s1]
getChan (ReducePlus s1 s2) _ = [s2,s1]  
getChan (Pack s1 s2) _ = [s2,s1]  
getChan (UPack s1 s2) _ = [s2,s1]  
getChan (SegConcat s1 s2) _ = [s2,s1]  
getChan (USegCount s1 s2) _ = [s2,s1]  
getChan (SegMerge s1 s2) _ = [s2,s1]  
getChan (Check s1 s2) _ = [s1,s2]  


-- generate the channel table for an SVCODE program
geneCTab :: [SInstr] -> SId -> CTable
geneCTab [] _ = []
geneCTab ((SDef sid sexp):ss) c = cl0 : geneCTab ss c
    where cl0 = getChan sexp c

geneCTab ((WithCtrl newc ss _):ss') c = cl ++ geneCTab ss' c
    where cl = geneCTab ss newc



-- generate the DAG 
geneDag :: [SInstr] -> SId -> Int -> Dag 
geneDag ss c sc = dagUpdate ss c $ replicate sc []


dagUpdate :: [SInstr] -> SId -> Dag -> Dag 
dagUpdate [] _ d = d 
dagUpdate (def@(SDef _ _) : ss) c d = dagUpdate ss c $ addDefEdge c d def 
dagUpdate ((WithCtrl newc ss _): ss') c d = 
    dagUpdate ss' c $ dagUpdate ss newc d 


addDefEdge :: SId -> Dag -> SInstr -> Dag 
addDefEdge _ d (SDef _ Ctrl) = d 
addDefEdge _ d (SDef _ EmptyCtrl) = d
addDefEdge c d (SDef i (Const _)) = addEdges [c] i d

addDefEdge _ d (SDef i (MapConst j _)) = addEdges [j] i d 
addDefEdge _ d (SDef i (MapOne _ j)) = addEdges [j] i d 
addDefEdge _ d (SDef i (MapTwo _ j k)) = addEdges [j,k] i d

addDefEdge _ d (SDef i (InterMergeS ss)) = addEdges ss i d 
addDefEdge _ d (SDef i (SegInterS ss)) = 
  let sids = concat $ map (\(x,y) -> [x,y]) ss in addEdges sids i d
addDefEdge _ d (SDef i (PriSegInterS ss)) = 
  let sids = concat $ map (\(x,y) -> [x,y]) ss in addEdges sids i d

addDefEdge _ d (SDef i (Distr j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegDistr j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegFlagDistr j k m)) = addEdges [j,k,m] i d
addDefEdge _ d (SDef i (PrimSegFlagDistr j k m)) = addEdges [j,k,m] i d

addDefEdge _ d (SDef i (ToFlags j)) = addEdges [j] i d
addDefEdge _ d (SDef i (Usum j)) = addEdges [j] i d
addDefEdge _ d (SDef i (B2u j)) = addEdges [j] i d

addDefEdge _ d (SDef i (SegscanPlus j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (ReducePlus j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (Pack j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (UPack j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegConcat j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (USegCount j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (SegMerge j k)) = addEdges [j,k] i d
addDefEdge _ d (SDef i (Check j k)) = addEdges [j,k] i d


-- add edges from js to i 
addEdges :: [SId] -> SId -> Dag -> Dag 
addEdges [] _ d = d 
addEdges (j:js) i d = addEdges js i d'
    where d' = updateList d j (e0++[i])  
          e0 = d!!j 


------ generate a file to visualize the DAG 
--- use "graph-easy": graph-easy <inputfile> --png 
geneDagFile ss c sc fname = 
  do let d = geneDag ss c sc
         ps = [0..length d -1]
         lines = zipWith (\x ys -> 
         	if null ys then drawpoint x 
            else concat $ map (\y -> drawline x y) ys) ps d
         content = concat lines 
     writeFile fname content 
   

drawpoint :: Int -> String
drawpoint i = "[" ++ show i ++ "]\n"

drawline :: Int -> Int -> String 
drawline i j = "[" ++ show i ++ "] ---> [" ++ show j ++ "] \n"

-----------------

