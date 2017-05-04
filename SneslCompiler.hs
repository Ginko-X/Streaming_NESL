{- Compiler from SNESL to SVCODE  -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import DataTrans
import SneslParser
import SneslTyping
import Control.Monad (zipWithM)
import Data.List (union)


newtype SneslTrans a = SneslTrans {rSneslTrans :: SId -> CompEnv ->
                                       Either String (a,[SDef], SId)}

instance Monad SneslTrans where
    return a = SneslTrans $ \ sid e -> Right (a, [], sid)

    m >>= f = SneslTrans $ \ sid e -> 
        case rSneslTrans m sid e of
            Left err -> Left err
            Right (a, sv, sid')  -> 
                case rSneslTrans (f a) sid' e of 
                    Left err' -> Left err'
                    Right (a', sv', sid'') -> Right (a', sv++sv', sid'')



instance Functor SneslTrans where
  fmap f t = t >>= return . f

instance Applicative SneslTrans where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



compiler :: Exp ->  Either String SSym 
compiler e = case rSneslTrans (translate e) 1 env0  of 
                 Right (st, sv, _) -> Right $ SSym (SDef 0 Ctrl:sv) st
                 Left err -> Left err 

type CompEnv = [(Id, (Type, STree))]

env0 :: CompEnv
env0 = []


askEnv :: SneslTrans CompEnv
askEnv = SneslTrans $ \ sid e0 -> Right (e0, [], sid)

askTyEnv :: SneslTrans TyEnv
askTyEnv = SneslTrans $ \ sid e0 -> 
             let tye0 = [(var, tp) | (var,(tp,_)) <- e0] 
              in Right (tye0, [], sid)


addEnv :: CompEnv -> SneslTrans a -> SneslTrans a
addEnv newe m = SneslTrans $ \sid e0 -> rSneslTrans m sid (newe++e0)  
  
lookupSTree :: Id -> SneslTrans STree
lookupSTree var = 
  do env <- askEnv 
     case lookup var env of 
        Just (_,stree) -> return stree
        Nothing -> fail "Variable binding Error"


lookupType :: Id -> SneslTrans Type
lookupType var = 
  do env <- askEnv 
     case lookup var env of 
        Just (tp,_) -> return tp 
        Nothing -> fail "Variable binding Error"



bindM :: Pat -> Type -> STree -> SneslTrans CompEnv
bindM (PVar x) t a = return [(x,(t,a))]
bindM PWild _ _ = return []
bindM (PTup p1 p2) (TTup tp1 tp2) (STPair st1 st2) = 
  do b1 <- bindM p1 tp1 st1 
     b2 <- bindM p2 tp2 st2
     return $ b1 ++ b2 
bindM p@(PTup _ _) t v = fail $ "Bad bindings: " ++ show p 
                                      ++ ", Type: " ++ show t
                                  


compTypeInfer :: Exp -> SneslTrans Type
compTypeInfer e =  
  do tye <- askTyEnv 
     case typeInfer e tye of 
         Right t -> return t 
         Left err -> fail err

getVarType :: Id -> SneslTrans Type
getVarType x =
  do tye <- askTyEnv 
     case lookup x tye of
         Just t -> return t
         Nothing -> fail "Variable type error" 


--- Translation ---

translate :: Exp -> SneslTrans STree 

translate (Var x) = lookupSTree x 


translate (Lit l) =  
       emit (Const l)


translate (Tup e1 e2) = 
    do t1 <- translate e1   
       t2 <- translate e2 
       return (STPair t1 t2)

translate (SeqNil tp) = 
    do s0 <- emptySeq tp
       f <- emit (Const (BVal True))  
       return (STPair s0 f)


translate (Let pat e1 e2) = 
    do tp <- compTypeInfer e1
       st <- translate e1 
       newEnv <- bindM pat tp st
       addEnv newEnv $ translate e2 
       

translate (Call fname es) = 
    do args <- mapM (\e -> translate e) es
       tys <-  mapM (\e -> compTypeInfer e) es 
       transFunc fname fe0 args tys 


translate (GComp e0 ps) = 
     do -- variables bindings
        tps <- mapM (\(_,e) -> compTypeInfer e) ps 
        trs <- mapM (\(_,e) -> translate e) ps        
        newEnvs <- mapM (\((p,_),TSeq tp,STPair st (STId _)) -> bindM p tp st) 
                        (zip3 ps tps trs)

        -- new ctrl
        let (STPair _ (STId s0)) = head trs 
        (STId newCtrl) <- emit (Usum s0) 
        (STId oldCtrl) <- emit GetCtrl 

        -- free variables distribution
        let bindvs = concat $ map (\(p,_) -> getPatVars p) ps  
            usingVars = filter (\x -> not $ x `elem` bindvs) (getVars e0) 
        usingVarsTps <- mapM (\x -> getVarType x) usingVars         
        usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars
        newVarTrs  <- zipWithM (\xtp x -> distr xtp x s0) usingVarsTps usingVarsTrs      
        usingVarbinds <- mapM (\(v,tp,tr) -> bindM (PVar v) tp tr) 
                              (zip3 usingVars usingVarsTps newVarTrs)

        -- translate the body
        emit (SetCtrl newCtrl)
        st <- addEnv (concat $ newEnvs ++ usingVarbinds) $ translate e0
        emit (SetCtrl oldCtrl) 
        return (STPair st (STId s0))
        

translate (RComp e0 e1) = 
    do (STId s1) <- translate e1
       (STId s2) <- emit (B2u s1)  
       (STId newCtrl) <- emit (Usum s2)
       (STId oldCtrl) <- emit GetCtrl 

       let usingVars = getVars e0 
       usingVarsTps <- mapM (\x -> getVarType x) usingVars         
       usingVarsTrs <- mapM (\x -> translate (Var x)) usingVars 
       newVarTrs <- zipWithM (\x xtp -> pack xtp x s1) usingVarsTrs usingVarsTps
       binds <- mapM (\(v,tp,tr) -> bindM (PVar v) tp tr) 
                     (zip3 usingVars usingVarsTps newVarTrs)

       emit (SetCtrl newCtrl)      
       s3 <- addEnv (concat binds) $ translate e0 
       emit (SetCtrl oldCtrl) 

       return (STPair s3 (STId s2)) 


type FuncEnv = [(Id, [STree] -> [Type] -> SneslTrans STree)]

-- [STree] are the arguments
transFunc :: Id -> FuncEnv -> [STree] -> [Type] -> SneslTrans STree
transFunc fid fe0 args tys = case lookup fid fe0 of 
    Just f -> f args tys
    Nothing -> fail "Function call error."


distr :: Type -> STree -> SId -> SneslTrans STree
distr TInt (STId s1) s = emit (Distr s1 s)
distr TBool (STId s1) s = emit (Distr s1 s)
distr (TTup tp1 tp2) (STPair t1 t2) s = 
    do st1 <- distr tp1 t1 s
       st2 <- distr tp2 t2 s
       return (STPair st1 st2) 
distr tp@(TSeq _) st s = distrSeg tp st s

-- Distributing Sequence
distrSeg :: Type -> STree -> SId -> SneslTrans STree
distrSeg (TSeq tp) t@(STPair s0 (STId s1)) s = 
   do newS1 <- emit (SegDistr s1 s) 
      newS0 <- distrSegRecur tp t s  
      return (STPair newS0 newS1)


distrSegRecur :: Type -> STree -> SId -> SneslTrans STree       
distrSegRecur TInt (STPair (STId s0) (STId s1))  s 
    = emit (PrimSegFlagDistr s0 s1 s)

distrSegRecur TBool (STPair (STId s0) (STId s1))  s 
    = emit (PrimSegFlagDistr s0 s1 s)

distrSegRecur (TTup tp1 tp2) (STPair (STPair s1 s2) s3) s =     
    do st1 <- distrSegRecur tp1 (STPair s1 s3) s
       st2 <- distrSegRecur tp2 (STPair s2 s3) s
       return (STPair st1 st2) 

distrSegRecur (TSeq t) (STPair (STPair s0 (STId s1)) (STId s2)) s = 
   do newS1 <- emit (SegFlagDistr s1 s2 s)
      s1' <- emit (SegMerge s1 s2)
      newS0 <- distrSegRecur t (STPair s0 s1') s 
      return (STPair newS0 newS1)


-- for empty sequences
emptySeq :: Type  -> SneslTrans STree
emptySeq TInt =  emit (Empty TInt)

emptySeq TBool = emit (Empty TBool)

emptySeq (TSeq tp) = 
    do s0 <- emptySeq tp
       f <- emit (Const (BVal True)) 
       --f <- emit (ToFlags c)
       return (STPair s0 f)

emptySeq (TTup t1 t2) = 
    do s1 <- emptySeq t1
       s2 <- emptySeq t2
       return (STPair s1 s2)


pack :: Type -> STree -> SId -> SneslTrans STree
pack TInt (STId s) b = emit (Pack s b)
pack TBool (STId s) b = emit (Pack s b)
pack (TTup tp1 tp2) (STPair t1 t2) b = 
  do st1 <- pack tp1 t1 b
     st2 <- pack tp2 t2 b
     return (STPair st1 st2)  

pack (TSeq tp) (STPair t (STId s)) b = 
    do (STId st1) <- emit (Distr b s)
       st2 <- pack tp t st1 
       st3 <- emit (UPack s b)
       return (STPair st2 st3)



-- get the free varibales in the expression
getVars :: Exp -> [Id]
getVars (Var x) = [x]
getVars (Lit a) = []
getVars (Tup e1 e2) = foldl union [] $ map getVars [e1,e2]
getVars (SeqNil tp) = []

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




-- emit one instruction
emit :: Instr -> SneslTrans STree
emit i = SneslTrans $ \ sid _ -> Right (STId sid, [SDef sid i] ,sid+1)



fe0 :: FuncEnv
fe0 = [("_uminus", \[STId s1] _ -> emit (MapOne Uminus s1)),
       ("not", \[STId s1] _ -> emit (MapOne Not s1)),

       ("_plus", \[STId s1, STId s2] _ -> emit (MapTwo Add s1 s2)),
       ("_minus", \[STId s1, STId s2] _ -> emit (MapTwo Minus s1 s2)),
       ("_times", \[STId s1, STId s2] _ -> emit (MapTwo Times s1 s2)),
       ("_div", \[STId s1, STId s2] _ -> emit (MapTwo Div s1 s2)),
       ("_eq",\[STId s1, STId s2] _ -> emit (MapTwo Equal s1 s2)),
       ("_leq",\[STId s1, STId s2] _ -> emit (MapTwo Leq s1 s2)),
        
       ("index", \[STId s] _ -> iotas s),                     

       ("scanExPlus", \[STPair (STId t) (STId s)] _ -> scanExPlus t s),

       ("reducePlus", \[STPair (STId t) (STId s)] _ -> reducePlus t s),
       
        ("_append", \[t1'@(STPair t1 (STId s1)), t2'@(STPair t2 (STId s2))] tys -> 
                  appendSeq (head tys) t1' t2'), 

       ("concat", \[STPair (STPair t (STId s1)) (STId s2)] _ -> concatSeq t s1 s2)]



iotas :: SId -> SneslTrans STree
iotas s = 
    do (STId s1) <- emit (ToFlags s)
       (STId s2) <- emit (Usum s1)
       (STId s3) <- emit (MapConst s2 (IVal 1))
       (STId s4) <- emit (SegscanPlus s3 s1)
       return (STPair (STId s4) (STId s1)) 


scanExPlus :: SId -> SId -> SneslTrans STree
scanExPlus t s = 
   do v <- emit (SegscanPlus t s)
      return (STPair v (STId s))


reducePlus t s = emit (ReducePlus t s)

concatSeq :: STree -> SId -> SId -> SneslTrans STree
concatSeq t s1 s2 = 
    do s' <- emit (SegConcat s1 s2)
       return (STPair t s')



-- append 
appendSeq :: Type -> STree -> STree -> SneslTrans STree
appendSeq (TSeq tp) t1'@(STPair t1 (STId s1)) t2'@(STPair t2 (STId s2)) =
    do fs <- emit (InterMerge s1 s2)
       t <- appendRecur tp t1' t2'
       return (STPair t fs)


appendRecur :: Type -> STree -> STree -> SneslTrans STree 
appendRecur TInt (STPair (STId t1) (STId s1)) (STPair (STId t2) (STId s2)) =
      emit (PriSegInter t1 s1 t2 s2)

appendRecur TBool (STPair (STId t1) (STId s1)) (STPair (STId t2) (STId s2)) =
      emit (PriSegInter t1 s1 t2 s2)
     
appendRecur (TTup tp1 tp2) (STPair (STPair t1 s1) (STId s2))
                           (STPair (STPair t2 s3) (STId s4)) = 
    do st1 <- appendRecur tp1 (STPair t1 (STId s2)) (STPair t2 (STId s4))
       st2 <- appendRecur tp2 (STPair s1 (STId s2)) (STPair s3 (STId s4)) 
       return (STPair st1 st2)

appendRecur (TSeq tp) (STPair (STPair t1 (STId s1)) (STId s2)) 
                      (STPair (STPair t2 (STId s3)) (STId s4)) =
    do s5 <- emit (SegInter s1 s2 s3 s4)
       s1' <- emit (SegMerge s1 s2)
       s3' <- emit (SegMerge s3 s4)
       s6 <- appendRecur tp (STPair t1 s1') (STPair t2 s3')
       return (STPair s6 s5)


                                    

