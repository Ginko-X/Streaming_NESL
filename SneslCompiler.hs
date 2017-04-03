{- Compiler from SNESL to SVCODE 
 + not finished -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax
import DataTrans
import SneslParser


newtype SneslTrans a = SneslTrans {rSneslTrans :: SId -> Either (a,[SDef], SId) String}

instance Monad SneslTrans where
    return a = SneslTrans $ \ sid -> Left (a, [], sid)

    m >>= f = SneslTrans $ \ sid -> 
        case rSneslTrans m sid  of
            Right err -> Right err
            Left (a, sv, sid')  -> 
                case rSneslTrans (f a) sid' of 
                    Right err' -> Right err'
                    Left (a', sv', sid'') -> Left (a', sv++sv', sid'')



instance Functor SneslTrans where
  fmap f t = t >>= return . f

instance Applicative SneslTrans where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta



type CompEnv = [(Id, STree)]

env0 :: CompEnv
env0 = [("x", (STId 10))]


compiler :: Exp ->  Either SSym String
compiler e = case rSneslTrans (translate e (STId 0) env0) 1   of 
                 Left (st, sv, _) -> Left $ SSym (SDef 0 Ctrl:sv) st
                 Right err -> Right err 
             --where ty = typing e



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
    do (STId s1) <- translate e1 ctrl env  -- s1 : <T>
       (STId s2) <- emit (B2u s1)  -- s2 : <F,T>
       ctrl' <- emit (Usum s2)  -- ctrl' : <*>
       let freeVars = getVars e0  -- [x,y]
       freeVarsTrs <- mapM (\x -> translate (Var x) ctrl env) freeVars --[St1, St2]
       newVarTrs <- mapM (\x -> pack x s1) freeVarsTrs
       let binds = zipWith (\ v t -> (v,t)) freeVars newVarTrs
       s3 <- translate e0 ctrl' (binds ++ env) 
       return (STPair s3 (STId s2)) 


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


-- primitive pack
-- [1,2,3,4,5] [F,T,F,F,T] = [2,5]
ppack :: [a] -> [Bool] -> [a]
ppack [] [] = []
ppack (a:as) (False:fs) = ppack as fs
ppack (a:as) (True:fs) = a: ppack as fs




-- get the free varibales in the expression
getVars :: Exp -> [Id]
getVars (Var x) = [x]
getVars (Lit a) = []
getVars (Tup e1 e2) = concat $ map getVars [e1,e2]
getVars (Let p e1 e2) = filter (\x -> not $ x `elem` binds) (getVars e2) 
    where binds = bindVars p 
getVars (Call fname es) = concat $ map getVars es     
getVars (GComp e0 ps) = filter (\x -> not $ x `elem` binds) (e0vs ++ evs) 
    where e0vs = getVars e0 
          binds = concat $ map (\(p,e) -> bindVars p) ps
          evs = concat $ map (\(_,e) -> getVars e) ps
getVars (RComp e0 e1) = concat $ map getVars [e0,e1]

bindVars :: Pat -> [Id]
bindVars (PVar x) = [x]
bindVars PWild = [] 
bindVars (PTup p1 p2) = concat $ map bindVars [p1,p2] 


emit :: Instr -> SneslTrans STree
emit i = SneslTrans $ \ sid -> Left (STId sid, [SDef sid i] ,sid+1)


type FuncEnv = [(Id, [STree] -> STree -> SneslTrans STree)]

fe0 :: FuncEnv
fe0 = [("_plus", \[STId s1, STId s2] _ -> emit (MapAdd s1 s2)),

       ("index", \[STId s] ctrl -> iotas ctrl s),                     

       ("scanExPlus", \[STId s] _ -> emit (ScanExPlus s))]


iotas :: STree -> SId -> SneslTrans STree
iotas _ s = 
    do (STId s1) <- emit (ToFlags s)
       (STId s2) <- emit (Usum s1)
       (STId s3) <- emit (MapConst s2 (IVal 1))
       (STId s4) <- emit (ScanExPlus s3)
       return (STPair (STId s4) (STId s1)) 


-- [STree] are the arguments
transFunc :: Id -> FuncEnv -> [STree] -> STree -> SneslTrans STree
transFunc fid fe0 args ctrl = case lookup fid fe0 of 
    Just f -> f args ctrl
    Nothing -> fail "Function call error."


bind :: Pat -> STree -> CompEnv
bind (PVar x) a = [(x, a)]
bind PWild s = []
bind (PTup p1 p2) (STPair t1 t2) = ps1 ++ ps2
    where ps1 = bind p1 t1
          ps2 = bind p2 t2


-- the number of 'True' must be equal to the length of the first list
pdist :: [a] -> [Bool] -> [a]
pdist [] [] = []
pdist v@(a:s) (False:fs) = a : pdist v fs 
pdist (a:s) (True:fs) = pdist s fs 




b2u [] = []
b2u (False:fs) = True : b2u fs 
b2u (True: fs) = False: True: b2u fs

-- unary sum of the number of Fs 
-- e.g. <F,F,T,F,T> => <1,1,1> or <*,*,*> actually
--usum :: SId -> 
--usum s@(SSVal []) = s 
--usum (SSVal ((BVal True):xs)) = usum (SSVal xs)
--usum (SSVal ((BVal False):xs)) = concatSeq (SSVal [IVal 1]) $ usum (SSVal xs)




