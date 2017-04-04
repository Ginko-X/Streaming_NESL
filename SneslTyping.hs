module SneslTyping where

import SneslSyntax


-- Snesl types
data Type = TInt 
          | TBool
          | TTup Type Type
          | TSeq Type
          deriving Show



type TyEnv = [(Id, Type)]

--newtype SneslTyping a = SneslTyping {rSneslTyping :: Either String a}

type SneslTyping a = Either String a 

--instance Monad SneslTyping where
--    return a = SneslTyping $ Right a 

--    m >>= f = SneslTyping $ 
--        case (rSneslTyping m) of
--            Right a -> case rSneslTyping (f a) of 
--                           Right b -> Right b 
--                           Left err' -> Left err' 
--            Left err -> Left err



--instance Functor SneslTyping where
--  fmap f t = t >>= return . f

--instance Applicative SneslTyping where
--  pure = return
--  tf <*> ta = tf >>= \f -> fmap f ta


typing :: Exp -> Either String Type
typing e = 
    case typeInfer e [] of 
        Right t -> Right t 
        Left err -> Left $ "Typing error: " ++ err 



typeInfer :: Exp -> TyEnv  -> SneslTyping Type
typeInfer (Var x) env = 
    case lookup x env of
        Just t -> return t
        Nothing -> fail $ "Bad variable: " ++ x

typeInfer (Lit a) env = 
    case a of 
        IVal _ -> return TInt
        BVal _ -> return TBool

typeInfer (Tup e1 e2) env = 
    do t1 <- typeInfer e1 env 
       t2 <- typeInfer e2 env
       return (TTup t1 t2)

typeInfer (Let p e1 e2) env = 
    do tp <- typeInfer e1 env
       typeInfer e2 (typeBind p tp ++ env)

typeInfer (Call f es) env = 
    do ts <- mapM (\x -> typeInfer x env) es 
       funcTypeInfer f funcTyEnv0 ts


typeInfer (GComp e0 ps) env = 
    do ts <- mapM (\(_,e) -> typeInfer e env) ps
       if all isSeq ts 
       then (do let binds = concat $ zipWith (\(p,_) (TSeq t') -> typeBind p t') ps ts 
                t0 <- typeInfer e0 (binds ++ env)
                return $ TSeq t0)
       else fail "Comprehension type mismatch"


typeInfer (RComp e1 e2) env = 
    do t2 <- typeInfer e2 env 
       case t2 of 
           TBool -> (do t1 <- typeInfer e1 env; return $ TSeq t1)
           _ -> fail "Comprehension type mismatch"


isSeq :: Type -> Bool
isSeq (TSeq _ ) = True
isSeq _  = False


typeBind :: Pat -> Type -> TyEnv
typeBind (PVar x) t = [(x,t)]
typeBind (PWild) t = []
typeBind (PTup p1 p2) (TTup t1 t2) = (typeBind p1 t1) ++ (typeBind p2 t2)     



type FuncTyEnv = [(Id, [Type] -> SneslTyping Type)]

funcTyEnv0 :: FuncTyEnv
funcTyEnv0 = [("_plus", \[TInt, TInt] -> return TInt),
              ("index", \[TInt] -> return $ TSeq TInt)]

funcTypeInfer :: Id -> FuncTyEnv -> [Type] -> SneslTyping Type
funcTypeInfer f env args = 
    case lookup f env of 
        Just f -> f args
        Nothing -> fail $ "Function type mismatch: " ++ f 

