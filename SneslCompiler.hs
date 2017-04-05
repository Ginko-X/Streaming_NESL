{- A Compiler from SNESL to SVCODE -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import DataTrans
import SneslParser


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
compiler e = case rSneslTrans (translate e (STId 0) env0) 1   of 
                 Right (st, sv, _) -> Right $ SSym (SDef 0 Ctrl:sv) st
                 Left err -> Left err 


translate :: Exp -> STree -> CompEnv -> SneslTrans STree 

translate (Var x) ctrl env = 
    case lookup x env of 
        Just tr -> return tr
        Nothing -> fail "Variable Error"

translate (Lit l) (STId i) env =  
       emit (MapConst i l)


translate (Tup e1 e2) ctrl env = 
    do t1 <- translate e1 ctrl env 
       t2 <- translate e2 ctrl env 
       return (STPair t1 t2)


translate (Let pat e1 e2) ctrl env = 
    do t <- translate e1 ctrl env
       translate e2 ctrl (bind pat t ++ env)


translate (Call fname es) ctrl env = 
    do args <- mapM (\e -> translate e ctrl env) es
       transFunc fname fe0 args ctrl 


translate (GComp e0 ps) ctrl env = 
     do trs <- mapM (\(_,e) -> translate e ctrl env) ps
        let binds = concat $ zipWith (\(p,_) (STPair t (STId s))-> bind p t) ps trs
        let (STPair t (STId s)) = head trs -- the segment descriptor 
        ctrl' <- emit (Usum s) 
        let freeVars = getVars (GComp e0 ps)
        freeVarsTrs <- mapM (\x -> translate (Var x) ctrl env) freeVars       
        newVarTrs  <- mapM (\x -> distr x s) freeVarsTrs
        let binds' = zipWith (\v t -> (v,t)) freeVars newVarTrs
        st <- translate e0 ctrl' (binds ++ binds' ++ env)
        return (STPair st (STId s))
        

translate (RComp e0 e1) ctrl env = 
    do (STId s1) <- translate e1 ctrl env  
       (STId s2) <- emit (B2u s1)  
       ctrl' <- emit (Usum s2)  
       let freeVars = getVars e0 
       freeVarsTrs <- mapM (\x -> translate (Var x) ctrl env) freeVars 
       newVarTrs <- mapM (\x -> pack x s1) freeVarsTrs
       let binds = zipWith (\ v t -> (v,t)) freeVars newVarTrs
       s3 <- translate e0 ctrl' (binds ++ env) 
       return (STPair s3 (STId s2)) 


type FuncEnv = [(Id, [STree] -> STree -> SneslTrans STree)]

-- [STree] are the arguments
transFunc :: Id -> FuncEnv -> [STree] -> STree -> SneslTrans STree
transFunc fid fe0 args ctrl = case lookup fid fe0 of 
    Just f -> f args ctrl
    Nothing -> fail "Function call error."



distr :: STree -> SId -> SneslTrans STree
distr (STId s1) s = emit (Distr s1 s)
distr (STPair t1 t2) s = 
    do st1 <- distr t1 s
       st2 <- distr t2 s
       return (STPair st1 st2)


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


emit :: Instr -> SneslTrans STree
emit i = SneslTrans $ \ sid -> Right (STId sid, [SDef sid i] ,sid+1)



fe0 :: FuncEnv
fe0 = [("_plus", \[STId s1, STId s2] _ -> emit (MapAdd s1 s2)),
       ("_times", \[STId s1, STId s2] _ -> emit (MapTimes s1 s2)),
       ("_div", \[STId s1, STId s2] _ -> emit (MapDiv s1 s2)),
       ("_eq",\[STId s1, STId s2] _ -> emit (MapEqual s1 s2)),

       ("index", \[STId s] ctrl -> iotas s),                     

       ("scanExPlus", \[STPair (STId t) (STId s)] _ -> scanExPlus t s),

       ("reducePlus", \[STPair (STId t) (STId s)] _ -> reducePlus t s),
       
       --("_append", \[STPair t1 (STId s1) ,STPair t2 (STId s2)] _ -> 
       --       do t <- appendSeq t1 t2; 
       --          (STId s') <- emit (Append s1 s2);
       --          s'' <- emit (Concat s');
       --          return (STPair t s'')), 
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
    do s' <- emit (Concat s1 s2)
       return (STPair t s')


appendSeq :: STree -> STree -> SneslTrans STree
appendSeq (STId t1) (STId t2) = 
    do t' <- emit (Append t1 t2) 
       return t'
appendSeq (STPair t1 (STId s1)) (STPair t2 (STId s2)) = 
    do t' <- appendSeq t1 t2
       s' <- emit (Append s1 s2)
       return (STPair t' s')



bind :: Pat ->  STree -> CompEnv
bind (PVar x) a = [(x, a)]
bind PWild s = []
bind (PTup p1 p2) (STPair t1 t2) = ps1 ++ ps2
    where ps1 = bind p1 t1
          ps2 = bind p2 t2

