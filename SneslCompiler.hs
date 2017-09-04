{- Compiler from SNESL to SVCODE  -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import Data.Set (fromList, toList)




runCompileDefs :: [Def] -> (VEnv, FEnv)  -> Either String (VEnv,FEnv)
runCompileDefs [] e = return e 
runCompileDefs (d:ds) e = runCompileDef d e >>= runCompileDefs ds 


-- compile a user-defined function  
runCompileDef :: Def -> (VEnv, FEnv) -> Either String (VEnv,FEnv)
runCompileDef (FDef fname args rtp e) (ve,fe) = 
    let (argSts, s0) = args2tree args 1 
        (st, s1) = type2tree rtp s0 
        sts = snd $ unzip argSts
        newVe = (fname, FDStr sts st):ve  
        sids = concat $ map tree2Sids sts  
    in  case rSneslTrans (translate e) s1 (argSts++newVe) of 
            Right (st',svs,count) -> Right $ (newVe,(fname,SFun sids st' svs count):fe) 
            Left err -> Left $ "Compiling error: " ++ fname ++ ":" ++ err



-- compile an expression
runCompileExp :: Exp -> VEnv -> Either String SFun 
runCompileExp e ve = 
    case rSneslTrans (translate e) 1 ve of 
        Right (st, sv, count) -> Right $ SFun [] st (SDef 0 Ctrl : sv) count 
        Left err -> Left err 

-------------------------------



-- transform the argument list to STrees
args2tree :: [(Id,Type)] -> SId -> ([(Id,STree)], SId)
args2tree [] i = ([],i)
args2tree [(v,t)] i = ([(v,st)], i')
    where  (st,i') = type2tree t i 

args2tree (v:vs) i = (st ++ sts, i'')
    where (st, i') = args2tree [v] i 
          (sts, i'') = args2tree vs i'


type2tree :: Type -> SId -> (STree, SId)
type2tree TInt i = (IStr i, i+1)
type2tree TBool i = (BStr i, i+1) 
type2tree (TSeq t) i = (SStr s i, i')
    where (s,i') = type2tree t (i+1)

type2tree (TTup t1 t2) i = (PStr s1 s2, i'') 
    where (s1,i') = type2tree t1 i
          (s2, i'') = type2tree t2 i'


tree2Sids :: STree -> [SId]
tree2Sids (IStr s1) = [s1]
tree2Sids (BStr s1) = [s1]
tree2Sids (SStr s1 s2) = (tree2Sids s1) ++ [s2]  
tree2Sids (PStr s1 s2) = (tree2Sids s1) ++ (tree2Sids s2)



askVEnv :: SneslTrans VEnv
askVEnv = SneslTrans $ \ sid e0 -> Right (e0, [], sid)


localVEnv :: VEnv -> SneslTrans a -> SneslTrans a
localVEnv newe m = SneslTrans $ \sid e0 -> rSneslTrans m sid (newe++e0)
  
lookupSTree :: Id -> SneslTrans STree
lookupSTree var = 
  do env <- askVEnv 
     case lookup var env of 
        Just stree -> return stree
        Nothing -> fail "Variable binding Error"


bindM :: Pat -> STree -> SneslTrans VEnv
bindM (PVar x) t = return [(x,t)]
bindM PWild _  = return []
bindM (PTup p1 p2) (PStr st1 st2) = 
  do b1 <- bindM p1 st1 
     b2 <- bindM p2 st2
     return $ b1 ++ b2 
bindM p@(PTup _ _) t = fail $ "Bad bindings: " ++ show p 
                                                 

-- generate a stream definition
emit :: SExp -> SneslTrans SId
emit i = SneslTrans $ \ sid _ -> Right (sid, [SDef sid i] ,sid+1)

emitIs :: SExp -> SneslTrans STree
emitIs i = do s <- emit i; return (IStr s)

emitBs i = do s <- emit i; return (BStr s)


emitInstr :: SInstr -> SneslTrans ()
emitInstr i = SneslTrans $ \ sid _ -> Right ((), [i], sid)


-- generate a stream SId without the instruction definition 
emitEmpty :: SneslTrans SId
emitEmpty = SneslTrans $ \ sid _ -> Right (sid, [] ,sid+1)


 --get the translated code and the returned stream ids(i.e. STree)
ctrlTrans :: SneslTrans STree -> SneslTrans (STree,[SInstr])
ctrlTrans m = SneslTrans $ \sid ve -> 
    case rSneslTrans m sid ve of 
      Right (st, code, s) -> Right ((st,code), [], s)
      Left err -> Left err 


scall :: FId -> [STree] -> STree -> SneslTrans STree
scall f argSts (FDStr _ st) = 
  do let sids = concat $ map tree2Sids argSts 
     rettr <- treeGene st 
     emitInstr (SCall f sids $ tree2Sids rettr)
     return rettr 


treeGene :: STree -> SneslTrans STree
treeGene (IStr _) = do s <- emitEmpty; return (IStr s)
treeGene (BStr _) = do s <- emitEmpty; return (BStr s)
treeGene (SStr s _) = 
    do s0 <- treeGene s       
       f <- emitEmpty
       return (SStr s0 f)
treeGene (PStr t1 t2) = 
    do s1 <- treeGene t1
       s2 <- treeGene t2
       return (PStr s1 s2)



--- Translation ---

translate :: Exp -> SneslTrans STree 

translate (Var x) = lookupSTree x 

translate (Lit l) =
    case l of 
      IVal i -> emitIs (Const l)
      BVal b -> emitBs (Const l)

translate (Tup e1 e2) = 
    do t1 <- translate e1   
       t2 <- translate e2 
       return (PStr t1 t2)

translate (SeqNil tp) = 
    do emptyCtrl <- emit EmptyCtrl
       (st,defs) <- ctrlTrans $ emptySeq tp       
       emitInstr (WithCtrl emptyCtrl [] defs st)
       f <- emit (Const (BVal True))
       return (SStr st f)


translate e@(Seq es) = 
    do sts <- mapM translate es
       s0 <- emit (Const (IVal 1)) 
       fs <- mapM (\ _ -> emit (ToFlags s0)) sts 
       mergeSeq $ zipWith (\st f -> SStr st f) sts fs 


translate (Let pat e1 e2) = 
    do st <- translate e1 
       newEnv <- bindM pat st
       localVEnv newEnv $ translate e2 
       

translate (Call fname es) = 
    do args <- mapM (\e -> translate e) es
       ve <- askVEnv
       case lookup fname ve of  
         Just fd@(FDStr _ _) -> scall fname args fd  -- user-defined
         Just (FStr f) -> f args -- built-in 
         Just _ -> fail $ "function and varibale names are identical: "++ fname
         Nothing -> fail $ "Compiling error: undefined function "++fname


translate (GComp e0 ps) = 
    do  trs <- mapM (\(_,e) -> translate e) ps        
        newEnvs <- mapM (\((p,_), SStr st _) -> bindM p st) (zip ps trs)

        -- new ctrl
        let (SStr _  s0) = head trs 

        -- free variables distribution
        let bindvs = concat $ map (\(p,_) -> getPatVars p) ps  
            usingVars = filter (\x -> not $ x `elem` bindvs) (getVars e0) 
        usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars
        newVarTrs  <- mapM (\x -> distr x s0) usingVarsTrs      
        usingVarbinds <- mapM (\(v,tr) -> bindM (PVar v) tr) 
                              (zip usingVars newVarTrs)

        newCtrl <- emit (Usum s0) 
        -- translate the body
        (st,defs) <- ctrlTrans $ localVEnv (concat $ newEnvs ++ usingVarbinds) 
                                  $ translate e0 
        let imps = getImportSids newCtrl defs 
        emitInstr (WithCtrl newCtrl imps defs st)
        return (SStr st s0)
        

translate (RComp e0 e1) = 
    do (BStr s1) <- translate e1       
       s2 <- emit (B2u s1)  
       let usingVars = getVars e0 
       usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars 
       newVarTrs <- mapM (\x -> pack x s1) usingVarsTrs
       binds <- mapM (\(v,tr) -> bindM (PVar v) tr) (zip usingVars newVarTrs)

       newCtrl <- emit (Usum s2)
       (s3,defs) <- ctrlTrans $ localVEnv (concat binds) $ translate e0 
       let imps = getImportSids newCtrl defs
       emitInstr (WithCtrl newCtrl imps defs s3)
       return (SStr s3 s2) 


getImportSids :: SId -> [SInstr] -> [SId]
getImportSids ctrl ins = 
  toList $ fromList $ concat $ map (getImportSid ctrl) ins

getImportSid :: SId -> SInstr -> [SId]
getImportSid ctrl (SDef sid e) =
  let (sups,_) = getSupExp e ctrl
  in filter (\sup -> sup < ctrl) sups 
getImportSid ctrl (WithCtrl _ imps _ _) = filter (\i -> i < ctrl) imps  
getImportSid ctrl (SCall _ args _) =  filter (\i -> i < ctrl) args



-- generate empty/undefined stream SIds
emptySeq :: Type  -> SneslTrans STree
emptySeq TInt =  do s <- emitEmpty; return (IStr s)
emptySeq TBool = do s <- emitEmpty; return (BStr s)

emptySeq (TSeq tp) = 
    do s0 <- emptySeq tp
       (BStr f) <- emptySeq TBool
       return (SStr s0 f)

emptySeq (TTup t1 t2) = 
    do s1 <- emptySeq t1
       s2 <- emptySeq t2
       return (PStr s1 s2)


distr :: STree -> SId -> SneslTrans STree
distr (IStr s1) s = emitIs (Distr s1 s)
distr (BStr s1) s = emitBs (Distr s1 s)
distr (PStr t1 t2) s = 
    do st1 <- distr t1 s
       st2 <- distr t2 s
       return (PStr st1 st2) 

--distr st s = fail "Sequences can't be distributed ." 
distr st@(SStr _ _) s = distrSeg st s


-- Distributing Sequence
distrSeg :: STree -> SId -> SneslTrans STree
distrSeg t@(SStr s0 s1) s = 
   do newS1 <- emit (SegDistr s1 s) 
      newS0 <- distrSegRecur t s  
      return (SStr newS0 newS1)


distrSegRecur :: STree -> SId -> SneslTrans STree       
distrSegRecur (SStr (IStr s0) s1) s = emitIs (PrimSegFlagDistr s0 s1 s)
distrSegRecur (SStr (BStr s0) s1) s = emitBs (PrimSegFlagDistr s0 s1 s)

distrSegRecur (SStr (PStr s1 s2) s3) s =     
    do st1 <- distrSegRecur (SStr s1 s3) s
       st2 <- distrSegRecur (SStr s2 s3) s
       return (PStr st1 st2) 

distrSegRecur (SStr (SStr s0 s1) s2) s = 
   do newS1 <- emit (SegFlagDistr s1 s2 s)
      s1' <- emit (SegConcat s1 s2)
      newS0 <- distrSegRecur (SStr s0 s1') s 
      return (SStr newS0 newS1)


pack :: STree -> SId -> SneslTrans STree
pack (IStr s) b = emitIs (Pack s b)
pack (BStr s) b = emitBs (Pack s b) 

pack (PStr t1 t2) b = 
  do st1 <- pack t1 b
     st2 <- pack t2 b
     return (PStr st1 st2)  

pack (SStr t s) b = 
    do st1 <- emit (Distr b s)
--       st2 <- pack t st1

       ctrl <- emit (Usum s)
       (st2, code) <- ctrlTrans (pack t st1)
       let imps = getImportSids ctrl code
       emitInstr (WithCtrl ctrl imps code st2)

       st3 <- emit (UPack s b)
       return (SStr st2 st3)


--- translation of built-in functions ---
compEnv0 = [ 
             ("_uminus", FStr (\[IStr s1] -> emitIs (MapOne Uminus s1))),
             ("not", FStr (\[BStr s1] -> emitBs (MapOne Not s1))),

             ("_plus", FStr (\[IStr s1, IStr s2] -> emitIs (MapTwo Add s1 s2))),
             ("_minus", FStr (\[IStr s1, IStr s2] -> emitIs (MapTwo Minus s1 s2))),
             ("_times", FStr (\[IStr s1, IStr s2] -> emitIs (MapTwo Times s1 s2))),
             ("_div", FStr (\[IStr s1, IStr s2] -> emitIs (MapTwo Div s1 s2))),
             ("_mod", FStr (\[IStr s1, IStr s2] -> emitIs (MapTwo Mod s1 s2))),

             ("_eq",FStr (\[IStr s1, IStr s2] -> emitBs (MapTwo Equal s1 s2))),
             ("_leq",FStr (\[IStr s1, IStr s2] -> emitBs (MapTwo Leq s1 s2))),
              
             ("index", FStr (\[IStr s] -> iotas s)),                
             ("scanExPlus", FStr (\[SStr (IStr t) s] -> scanExPlus t s)),
             ("reducePlus", FStr (\[SStr (IStr t) s] -> reducePlus t s)),             
             ("_append", FStr (\[t1'@(SStr t1 s1), t2'@(SStr t2 s2)] -> 
                        mergeSeq [t1',t2'])), 

             ("concat", FStr (\[SStr (SStr t s1) s2] -> concatSeq t s1 s2)),
             ("the", FStr (\[t@(SStr _ _)] -> the　t) ),
             ("empty", FStr (\[t@(SStr _ _)] -> empty t)),
             ("part",  FStr (\[t1'@(SStr _ _), t2'@(SStr (BStr _) _)] -> part t1' t2') )]


-- {1,2,3} {FT FFT}
-- <1,2,3>  <FFFT> ; <FTFFT> <FFFFFT>
-- USegCount <FTFFT> <FFFFFT> => <F F T>
-- <1 2 3> <FT FFT> <F F T>  => {{1},{2,3}}
part :: STree -> STree -> SneslTrans STree
part (SStr t1 _) (SStr (BStr t2) f2) = 
    do s1 <- emit (USegCount t2 f2) 
       return (SStr (SStr t1 t2) s1)


empty :: STree -> SneslTrans STree
empty (SStr _ f) = emitBs (IsEmpty f)


the :: STree -> SneslTrans STree
the (SStr t f) = -- return t 
    do s1 <- emit (Const (IVal 1))
       s2 <- emit (ToFlags s1)
       s3 <- emit (Check f s2)
       return t 


iotas :: SId -> SneslTrans STree
iotas s = 
    do s1 <- emit (ToFlags s)
       s2 <- emit (Usum s1)
       (st@(IStr s3'), code) <- ctrlTrans 
                                 (do s3 <- emit (Const (IVal 1)); 
                                     return $ IStr s3) 
       emitInstr (WithCtrl s2 [] code st)
       s4 <- emit (SegscanPlus s3' s1)
       return (SStr (IStr s4) s1) 


scanExPlus :: SId -> SId -> SneslTrans STree
scanExPlus t s = 
    do v <- emitIs (SegscanPlus t s)
       return (SStr v s)


reducePlus t s = emitIs (ReducePlus t s)


concatSeq :: STree -> SId -> SId -> SneslTrans STree
concatSeq t s1 s2 = 
    do f <- emit (SegConcat s1 s2)
       return (SStr t f)


mergeSeq :: [STree] -> SneslTrans STree
mergeSeq trs = 
    do let (_, fs) = unzip $ map (\(SStr t f) -> (t,f)) trs
       f <- emit (InterMergeS fs)
       t <- mergeRecur trs
       return (SStr t f)


mergeRecur :: [STree] -> SneslTrans STree 
mergeRecur trs@((SStr (IStr t) s):_) = 
    emitIs (PriSegInterS $ map (\(SStr (IStr t) s) -> (t,s)) trs)

mergeRecur trs@((SStr (BStr t) s):_) = 

    emitBs (PriSegInterS $ map (\(SStr (BStr t) s) -> (t,s)) trs)   

mergeRecur trs@((SStr (PStr t1 t2) s):_) = 
    do let fsts = [(SStr t1 s) | (SStr (PStr t1 _) s) <- trs]
           snds = [(SStr t2 s) | (SStr (PStr _ t2) s) <- trs]
       st1 <- mergeRecur fsts
       st2 <- mergeRecur snds
       return (PStr st1 st2)

mergeRecur trs@((SStr (SStr t1 s1) s2):_) = 
    do let (t1s,ps) = unzip 
               [(t1,(s1,s2)) | (SStr (SStr t1 s1) s2) <- trs]
       s2' <- emit (SegInterS ps)
       ps' <- mapM (\(x,y) -> emit (SegConcat x y)) ps
       trs' <- mergeRecur (zipWith SStr t1s ps')
       return (SStr trs' s2')

