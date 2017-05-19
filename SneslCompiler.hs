{- Compiler from SNESL to SVCODE  -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import DataTrans
import SneslParser
import SneslTyping
import Data.List (union)



runCompilerEnv :: [Def] -> Either String [(Id,SFun)]
runCompilerEnv defs = compileDefs defs []


compileDefs :: [Def] -> FEnv  -> Either String FEnv
compileDefs [] r = return r 
compileDefs (d:ds) r = 
    do r' <- compileDef d r
       compileDefs ds (r'++r)  


-- compile a user-defined function  
compileDef :: Def -> FEnv -> Either String FEnv
compileDef (FDef fname args rtp e) se0 = 
    let (s0,arge) = argsSTree args 1
        (_, sts) = unzip arge
    in  case rSneslTrans (translate e) s0 (arge++compEnv0) se0 of 
            Right (st,svs,_) -> Right [(fname,SFun 0 sts st svs)]
            Left err -> Left $ "Compiling error: " ++ fname ++ ":" ++ err


-- transform the argument list to STrees
argsSTree :: [(Id,Type)] -> SId -> (SId,[(Id,STree)])
argsSTree [] i = (i,[])
argsSTree [(v,t)] i = (i',[(v,st)])
    where  (st,i') = tp2st t i 

argsSTree (v:vs) i = (i'', st ++ sts)
    where (i',st) = argsSTree [v] i 
          (i'',sts) = argsSTree vs i'

tp2st :: Type -> SId -> (STree, SId)
tp2st TInt i = (IStr i, i+1)
tp2st TBool i = (BStr i, i+1) 
tp2st (TSeq t) i = (SStr s i, i')
    where (s,i') = tp2st t (i+1)

tp2st (TTup t1 t2) i = (PStr s1 s2, i'') 
    where (s1,i') = tp2st t1 i
          (s2, i'') = tp2st t2 i'


-- compile an expression
compileExp :: Exp -> FEnv -> Either String SFun 
compileExp e fe = 
    case rSneslTrans (translate e) 1 compEnv0 fe  of 
        Right (st, sv, _) -> Right $ SFun 0 [] st (SDef 0 Ctrl:sv)
        Left err -> Left err 



askEnv :: SneslTrans (VEnv,FEnv)
askEnv = SneslTrans $ \ sid ve fe -> Right ((ve,fe), [], sid)


askVEnv :: SneslTrans VEnv
askVEnv = SneslTrans $ \ sid e0 _ -> Right (e0, [], sid)


addVEnv :: VEnv -> SneslTrans a -> SneslTrans a
addVEnv newe m = SneslTrans $ \sid e0 fe -> rSneslTrans m sid (newe++e0) fe 
  
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
                                      ++ ", STree: " ++ show t
                                                 

-- generate a stream definition
emit :: SExp -> SneslTrans SId
emit i = SneslTrans $ \ sid _ _ -> Right (sid, [SDef sid i] ,sid+1)

emitIs i = do s <- emit i; return (IStr s)
emitBs i = do s <- emit i; return (BStr s)


-- generate a stream SId without the instruction definition 
emitEmpty :: SneslTrans SId
emitEmpty = SneslTrans $ \ sid _ _ -> Right (sid, [] ,sid+1)


 --get the translated code and the returned stream ids(i.e. STree)
ctrlTrans :: SneslTrans STree -> SneslTrans (STree,[SInstr])
ctrlTrans m = SneslTrans $ \sid ve fe -> 
    case rSneslTrans m sid ve fe of 
      Right (st, code, s) -> Right ((st,code), [], s)
      Left err -> Left err 


scall :: [STree] -> SFun -> SneslTrans STree
scall args (SFun c sts st sv) = 
  do argmap <- sidMaps args sts
     rettr <- streeCopy st 
     retmap <- sidMap st rettr
     emit (SCall c argmap sv retmap)
     return rettr 


-- copy the structure of a STree, a type copy indeed
streeCopy :: STree -> SneslTrans STree
streeCopy (IStr _) = do s <- emitEmpty; return (IStr s)
streeCopy (BStr _) = do s <- emitEmpty; return (BStr s)
streeCopy (SStr s f) = 
    do s0 <- streeCopy s
       (BStr f') <- streeCopy (BStr f)
       return (SStr s0 f')
streeCopy (PStr t1 t2) = 
    do s1 <- streeCopy t1
       s2 <- streeCopy t2
       return (PStr s1 s2)


-- create argument/return passing mappings 
sidMaps :: [STree] -> [STree] -> SneslTrans [(SId,SId)]
sidMaps [] [] = return []
sidMaps (a:as) (b:bs) = 
  do ab <- sidMap a b
     abs <- sidMaps as bs
     return $ ab ++ abs 
sidMaps _ _ = fail "function argumennts passing error."

sidMap :: STree -> STree -> SneslTrans [(SId,SId)]
sidMap (IStr s1) (IStr s2) = return [(s1,s2)]
sidMap (BStr s1) (BStr s2) = return [(s1,s2)]
sidMap (SStr s1 s2) (SStr s3 s4) = 
  do s13 <- sidMap s1 s3 
     return $ s13 ++ [(s2,s4)]
sidMap (PStr s1 s2) (PStr s3 s4) = 
  do s13 <- sidMap s1 s3 
     s24 <- sidMap s2 s4 
     return $ s13 ++ s24
sidMap _ _ = fail "function argumennts passing error."



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
    do (st,defs) <- ctrlTrans $ emptySeq tp
       emptyCtrl <- emit EmptyCtrl
       emit (WithCtrl emptyCtrl defs st)
       f <- emit (Const (BVal True))
       return (SStr st f)


translate e@(Seq es) = 
    do sts <- mapM translate es
       s0 <- emit (Const (IVal 1)) 
       f0 <- emit (ToFlags s0)
       mergeSeq [(SStr st f0) | st <- sts]


translate (Let pat e1 e2) = 
    do st <- translate e1 
       newEnv <- bindM pat st
       addVEnv newEnv $ translate e2 
       

translate (Call fname es) = 
    do args <- mapM (\e -> translate e) es
       (ve,fe) <- askEnv
       case lookup fname ve of 
         Just (FStr f) -> f args
         Nothing -> 
             case lookup fname fe of 
                Just ss@(SFun _ _ _ _) -> scall args ss 
                Nothing -> fail $ "Compiling error: undefined function "++fname


translate (GComp e0 ps) = 
    do  trs <- mapM (\(_,e) -> translate e) ps        
        newEnvs <- mapM (\((p,_), SStr st _) -> bindM p st) (zip ps trs)                        

        -- new ctrl
        let (SStr _  s0) = head trs 
        newCtrl <- emit (Usum s0) 

        -- free variables distribution
        let bindvs = concat $ map (\(p,_) -> getPatVars p) ps  
            usingVars = filter (\x -> not $ x `elem` bindvs) (getVars e0) 
        usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars
        newVarTrs  <- mapM (\x -> distr x s0) usingVarsTrs      
        usingVarbinds <- mapM (\(v,tr) -> bindM (PVar v) tr) 
                              (zip usingVars newVarTrs)

        -- translate the body
        (st,defs) <- ctrlTrans $ addVEnv (concat $ newEnvs ++ usingVarbinds) 
                                  $ translate e0 
        emit (WithCtrl newCtrl defs st)
        return (SStr st s0)
        

translate (RComp e0 e1) = 
    do (BStr s1) <- translate e1
       
       s2 <- emit (B2u s1)  
       newCtrl <- emit (Usum s2)

       let usingVars = getVars e0 
       usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars 
       newVarTrs <- mapM (\x -> pack x s1) usingVarsTrs
       binds <- mapM (\(v,tr) -> bindM (PVar v) tr) 
                     (zip usingVars newVarTrs)

       (s3,defs) <- ctrlTrans $ addVEnv (concat binds) $ translate e0 
       emit (WithCtrl newCtrl defs s3)
       return (SStr s3 s2) 




-- get the free varibales in the expression
getVars :: Exp -> [Id]
getVars (Var x) = [x]
getVars (Lit a) = []
getVars (Tup e1 e2) = foldl union [] $ map getVars [e1,e2]
getVars (SeqNil tp) = []
getVars (Seq es) = foldl union [] $ map getVars es
getVars (Let p e1 e2) = e1Vars ++ filter (\x -> not $ x `elem` binds) (getVars e2) 
    where binds = getPatVars p 
          e1Vars = getVars e1 

getVars (Call fname es) = foldl union [] $ map getVars es 

getVars (GComp e0 ps) = pVars ++ filter (\x -> not $ x `elem` binds) e0vs
    where e0vs = getVars e0
          binds = foldl union [] $ map (\(p,_) -> getPatVars p) ps
          pVars = foldl union [] $ map (\(_,e) -> getVars e) ps 

getVars (RComp e0 e1) = foldl union [] $ map getVars [e0,e1]


getPatVars :: Pat -> [Id]
getPatVars (PVar x) = [x]
getPatVars PWild = [] 
getPatVars (PTup p1 p2) = concat $ map getPatVars [p1,p2] 


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
      s1' <- emit (SegMerge s1 s2)
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
       st2 <- pack t st1 
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
             ("_eq",FStr (\[IStr s1, IStr s2] -> emitBs (MapTwo Equal s1 s2))),
             ("_leq",FStr (\[IStr s1, IStr s2] -> emitBs (MapTwo Leq s1 s2))),
              
             ("index", FStr (\[IStr s] -> iotas s)),                

             ("scanExPlus", FStr (\[SStr (IStr t) s] -> scanExPlus t s)),

             ("reducePlus", FStr (\[SStr (IStr t) s] -> reducePlus t s)),
             
             ("_append", FStr (\[t1'@(SStr t1 s1), t2'@(SStr t2 s2)] -> 
                        mergeSeq [t1',t2'])), 

             ("concat", FStr (\[SStr (SStr t s1) s2] -> concatSeq t s1 s2))]



iotas :: SId -> SneslTrans STree
iotas s = 
    do s1 <- emit (ToFlags s)
       s2 <- emit (Usum s1)
       s3 <- emit (MapConst s2 (IVal 1))
       s4 <- emit (SegscanPlus s3 s1)
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
       ps' <- mapM (\(x,y) -> emit (SegMerge x y)) ps
       trs' <- mergeRecur (zipWith SStr t1s ps')
       return (SStr trs' s2')

