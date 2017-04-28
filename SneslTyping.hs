module SneslTyping where

import SneslSyntax


typing :: Exp -> Either String Type
typing e = 
    case typeInfer e [] of 
        Right t -> Right t 
        Left err -> Left $ "Typing error: " ++ err 



type TyEnv = [(Id, Type)]

type SneslTyping a = Either String a 

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

 --a special case
typeInfer (SeqNil tp) env = return $ TSeq tp


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
funcTyEnv0 = [("not", \t -> funcTypeChk "not" t [TBool] TBool ),
              ("_uminus", \t -> funcTypeChk "_uminus" t [TInt] TInt),              

              ("_plus", \t -> funcTypeChk "_plus" t [TInt,TInt] TInt),
              ("_minus", \t -> funcTypeChk "_minus" t [TInt,TInt] TInt),          
              ("_times", \t -> funcTypeChk "_times" t [TInt,TInt] TInt),
              ("_div", \t -> funcTypeChk "_div" t [TInt,TInt] TInt),
              ("_eq", \t -> funcTypeChk "_eq" t [TInt,TInt] TBool),
              ("_leq", \t -> funcTypeChk "_leq" t [TInt,TInt] TBool),

              ("index", \t -> funcTypeChk "index" t [TInt] (TSeq TInt)),              

              ("scanExPlus", \t -> funcTypeChk "scanExPlus" t [TSeq TInt] (TSeq TInt)),
             
              ("reducePlus", \t -> funcTypeChk "reducePlus" t [TSeq TInt] TInt),

              ("_append", \t -> funcTypeChkAppend t),

              ("concat", \t -> funcTypeChkConcat t)]


funcTypeInfer :: Id -> FuncTyEnv -> [Type] -> SneslTyping Type
funcTypeInfer f env args = 
    case lookup f env of 
        Just f -> f args
        Nothing -> fail $ "Typing: Undefined function: " ++ f 

funcTypeChk :: Id -> [Type] ->[Type] -> Type -> SneslTyping Type
funcTypeChk fname t1 t2 ft = 
    if t1 == t2 
    then return ft 
    else fail $ "Function type or arity mismatch:" ++ fname

funcTypeChkConcat t = 
    case t of 
       [TSeq (TSeq t')] -> return $ TSeq t'
       _ -> fail "Function type or arity mismatch: concat"


funcTypeChkAppend t = 
    case t of 
       [TSeq t1, TSeq t2] -> if t1 == t2 then return $ TSeq t1 
                              else fail "Function type mismatch: _append"
       _ -> fail "Function type or arity mismatch: _append"

