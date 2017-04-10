{- A Compiler from SNESL to SVCODE  -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import DataTrans
import SneslParser
import SneslTyping
import Control.Monad (zipWithM)


newtype SneslTrans a = SneslTrans {rSneslTrans :: SId -> 
                                       Either String (a,[SDef], SId)}

instance Monad SneslTrans where
    return a = SneslTrans $ \ sid -> Right (a, [], sid)

    m >>= f = SneslTrans $ \ sid -> 
        case rSneslTrans m sid  of
            Left err -> Left err
            Right (a, sv, sid')  -> 
                case rSneslTrans (f a) sid' of 
                    Left err' -> Left err'
                    Right (a', sv', sid'') -> Right (a', sv++sv', sid'')



instance Functor SneslTrans where
  fmap f t = t >>= return . f

instance Applicative SneslTrans where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



type CompEnv = [(Id, STree)]
env0 :: CompEnv
env0 = []


compiler :: Exp ->  Either String SSym 
compiler e = case rSneslTrans (translate e (STId 0) env0 []) 1   of 
                 Right (st, sv, _) -> Right $ SSym (SDef 0 Ctrl:sv) st
                 Left err -> Left err 


compTypeInfer :: Exp -> TyEnv -> SneslTrans Type
compTypeInfer e tye =  
    case typeInfer e tye of 
        Right t -> return t 
        Left err -> fail err

getVarType :: Id -> TyEnv -> SneslTrans Type
getVarType x tye = 
    case lookup x tye of
        Just t -> return t
        Nothing -> fail "Variable type error" 


translate :: Exp -> STree -> CompEnv -> TyEnv -> SneslTrans STree 

translate (Var x) ctrl env tye = 
    case lookup x env of 
        Just tr -> return tr
        Nothing -> fail "Variable binding Error"

translate (Lit l) (STId i) env tye =  
       emit (MapConst i l)


translate (Tup e1 e2) ctrl env tye = 
    do t1 <- translate e1 ctrl env tye 
       t2 <- translate e2 ctrl env tye 
       return (STPair t1 t2)


translate (Let pat e1 e2) ctrl env tye = 
    do tp <- compTypeInfer e1 tye
       let tye' = typeBind pat tp ++ tye
       t <- translate e1 ctrl env tye'
       translate e2 ctrl (bind pat t ++ env) tye'


translate (Call fname es) ctrl env tye = 
    do args <- mapM (\e -> translate e ctrl env tye) es
       tys <-  mapM (\e -> compTypeInfer e tye) es 
       transFunc fname fe0 args tys ctrl 


translate (GComp e0 ps) ctrl env tye = 
     do -- type binds
        tps <- mapM (\(_,e) -> compTypeInfer e tye) ps
        let tye' = (concat $ zipWith (\(p,_) (TSeq tp) -> typeBind p tp) ps tps)  ++ tye 
        
        -- varable binds
        trs <- mapM (\(_,e) -> translate e ctrl env tye') ps
        let binds = concat $ zipWith (\(p,_) (STPair t (STId s))-> bind p t) ps trs
        
        let (STPair t (STId s0)) = head trs 
        ctrl' <- emit (Usum s0) 
        
        -- free variables distribution
        let freeVars = getVars (GComp e0 ps)
        freeVarsTps <- mapM (\x -> getVarType x tye') freeVars         
        freeVarsTrs <- mapM (\x -> translate (Var x) ctrl env tye') freeVars
        newVarTrs  <- zipWithM (\xtp x -> distr xtp x s0) freeVarsTps freeVarsTrs      
        let binds' = zipWith (\v t -> (v,t)) freeVars newVarTrs

        -- translate body        
        st <- translate e0 ctrl' (binds ++ binds' ++ env) tye'
        return (STPair st (STId s0))
        

translate (RComp e0 e1) ctrl env tye = 
    do (STId s1) <- translate e1 ctrl env tye  
       (STId s2) <- emit (B2u s1)  
       ctrl' <- emit (Usum s2)  
       let freeVars = getVars e0 
       freeVarsTrs <- mapM (\x -> translate (Var x) ctrl env tye) freeVars 
       newVarTrs <- mapM (\x -> pack x s1) freeVarsTrs
       let binds = zipWith (\ v t -> (v,t)) freeVars newVarTrs
       s3 <- translate e0 ctrl' (binds ++ env) tye 
       return (STPair s3 (STId s2)) 


type FuncEnv = [(Id, [STree] -> [Type] -> STree -> SneslTrans STree)]

-- [STree] are the arguments
transFunc :: Id -> FuncEnv -> [STree] -> [Type] -> STree -> SneslTrans STree
transFunc fid fe0 args tys ctrl = case lookup fid fe0 of 
    Just f -> f args tys ctrl
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



pack :: STree -> SId -> SneslTrans STree
pack (STId s) b = emit (Pack s b)

pack (STPair t (STId s)) b = 
    do (STId st1) <- emit (Distr b s)
       st2 <- pack t st1 
       st3 <- emit (UPack s b)
       return (STPair st2 st3)


-- get the free varibales in the expression
getVars :: Exp -> [Id]
getVars (Var x) = [x]
getVars (Lit a) = []
getVars (Tup e1 e2) = concat $ map getVars [e1,e2]
getVars (Let p e1 e2) = filter (\x -> not $ x `elem` binds) (getVars e2) 
    where binds = bindVars p 
getVars (Call fname es) = concat $ map getVars es     
getVars (GComp e0 ps) = filter (\x -> not $ x `elem` binds) e0vs
    where e0vs = getVars e0 
          binds = concat $ map (\(p,e) -> bindVars p) ps
getVars (RComp e0 e1) = concat $ map getVars [e0,e1]

bindVars :: Pat -> [Id]
bindVars (PVar x) = [x]
bindVars PWild = [] 
bindVars (PTup p1 p2) = concat $ map bindVars [p1,p2] 



-- emit one instruction
emit :: Instr -> SneslTrans STree
emit i = SneslTrans $ \ sid -> Right (STId sid, [SDef sid i] ,sid+1)



fe0 :: FuncEnv
fe0 = [("_plus", \[STId s1, STId s2] _ _ -> emit (MapAdd s1 s2)),
       ("_times", \[STId s1, STId s2] _ _ -> emit (MapTimes s1 s2)),
       ("_div", \[STId s1, STId s2] _ _ -> emit (MapDiv s1 s2)),
       ("_eq",\[STId s1, STId s2] _ _ -> emit (MapEqual s1 s2)),

       ("index", \[STId s] _ ctrl -> iotas s),                     

       ("scanExPlus", \[STPair (STId t) (STId s)] _ _ -> scanExPlus t s),

       ("reducePlus", \[STPair (STId t) (STId s)] _ _ -> reducePlus t s),
       
        ("_append", \[t1'@(STPair t1 (STId s1)), t2'@(STPair t2 (STId s2))] tys ctrl -> 
                  appendSeq (head tys) t1' t2'), 

       ("concat", \[STPair (STPair t (STId s1)) (STId s2)] _ _ -> concatSeq t s1 s2)]



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

-- 先不考虑pair
appendSeq :: Type -> STree -> STree -> SneslTrans STree
appendSeq _ (STPair (STId t1) (STId s1)) 
          (STPair (STId t2) (STId s2)) =
    do fs <- emit (InterMerge s1 s2)
       st <- emit (PriSegInter t1 s1 t2 s2)
       return (STPair st fs)

--appendSeq (TSeq (TTup t1 t2)) (STPair t1'@(STPair t1 (STId s1)) (STId s2)) 

appendSeq (TSeq (TSeq t)) (STPair t1'@(STPair t1 (STId s1)) (STId s2)) 
                          (STPair t2'@(STPair t2 (STId s3)) (STId s4)) =
    do fs <- emit (InterMerge s2 s4)
       t <- appendRecur t1' s2 t2' s4
       return (STPair t fs)


appendRecur :: STree -> SId -> STree -> SId -> SneslTrans STree 
appendRecur (STId s1) s2 (STId s3) s4 = emit (PriSegInter s1 s2 s3 s4)
appendRecur (STPair t1 (STId s1)) s2 
            (STPair t2 (STId s3)) s4 = 
    do s5 <- emit (SegInter s1 s2 s3 s4)
       (STId s1') <- emit (SegMerge s1 s2)
       (STId s3') <- emit (SegMerge s3 s4)
       s6 <- appendRecur t1 s1' t2 s3'
       return (STPair s6 s5)


bind :: Pat ->  STree -> CompEnv
bind (PVar x) a = [(x, a)]
bind PWild s = []
bind (PTup p1 p2) (STPair t1 t2) = ps1 ++ ps2
    where ps1 = bind p1 t1
          ps2 = bind p2 t2

