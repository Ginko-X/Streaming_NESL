{- Compiler from SNESL to SVCODE 
 + not finished -}

module SneslCompiler where 

import SneslSyntax
import SvcodeSyntax



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
env0 = [("x", STcons 10 STnil)]


compiler :: Exp -> Either SSym String
compiler e = case rSneslTrans (translate e env0) 0   of 
                 Left (st, sv, _) -> Left $ SSym sv st
                 Right err -> Right err 



translate :: Exp -> CompEnv -> SneslTrans STree 

translate (Var x) env = 
    case lookup x env of 
        Just v -> return v
        Nothing -> fail "Variable Error"

translate (Lit l) env =  
    do sid <- getSid
       emit (MapConst sid l) 


translate (Tup e1 e2) env = 
    do t1 <- translate e1 env 
       t2 <- translate e2 env 
       return (concatTree t1 t2)

translate (Let pat e1 e2) env = 
    do t <- translate e1 env
       translate e2 (bind pat t ++ env)
       --return $ concatTree t1 t2


translate (Call fname es) env = 
    do ts <- mapM (\e -> translate e env) es
       builtinTrans fname env ts 


translate (GComp e0 ps) env = 
     do ts <- mapM (\(_,e) -> translate e env) ps
        let binds = concat $ zipWith (\(p,_) t -> bind p t) ps ts
        translate e0 (env++binds)
        
--translate (RComp e0 e1) env = 



builtinTrans :: Id -> CompEnv -> [STree] -> SneslTrans STree
builtinTrans f env ts = undefined


getSid :: SneslTrans SId
getSid = SneslTrans $ \ s -> Left (s,[],s)


emit :: Instr -> SneslTrans STree
emit i = SneslTrans $ \ sid -> Left (STcons sid STnil, [SDef sid i] ,sid+1)


bind :: Pat -> STree -> CompEnv
bind (PVar x) a = [(x, a)]
bind PWild s = []
bind (PTup p1 p2) (STcons s1 s2) = ps1 ++ ps2
    where ps1 = bind p1 (STcons s1 STnil)
          ps2 = bind p2 s2


concatTree :: STree -> STree -> STree
concatTree STnil t = t 
concatTree t STnil = t 
concatTree (STcons t1 t1') t2 = STcons t1 (concatTree t1' t2)


-- unary sum of the number of Fs 
-- e.g. <F,F,T,F,T> => <1,1,1> or <*,*,*> actually
usum :: SvVal -> SvVal
usum s@(SSVal []) = s 
usum (SSVal ((BVal True):xs)) = usum (SSVal xs)
usum (SSVal ((BVal False):xs)) = concatSeq (SSVal [IVal 1]) $ usum (SSVal xs)


concatSeq :: SvVal -> SvVal -> SvVal
concatSeq (SSVal s1) (SSVal s2) = SSVal (s1++s2)



