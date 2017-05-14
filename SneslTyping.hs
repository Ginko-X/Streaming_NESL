module SneslTyping where

import SneslSyntax


runTyping :: [Def] -> Either String TyEnv
runTyping defs = 
    let id0 = fst $ unzip tyEnv0
    in case typingDefs defs tyEnv0 of 
         Right t -> Right $ takeWhile (\(x,_) -> not $ x `elem` id0) t 
         Left err -> Left $ "SNESL typing error: " ++ err 




typingDefs :: [Def] -> TyEnv -> SneslTyping TyEnv
typingDefs [] r = return r  
typingDefs (d:defs) r = 
    do r' <- typingDef d r 
       typingDefs defs r'


typingDef :: Def -> TyEnv -> SneslTyping TyEnv

typingDef (EDef i e) r = 
    case typeInfer e r of 
        Right t -> Right ((i,t):r)
        Left err -> Left err 

typingDef (FDef fname args e) r =
    do let f vs = (typeInfer e (zip args vs ++ r1))
           r1 = (fname, TFun f):r
       return r1



typingExp :: Exp -> Either String Type
typingExp e = 
    case typeInfer e tyEnv0 of 
        Right t -> Right t 
        Left err -> Left $ "Typing error: " ++ err 



type TyEnv = [(Id, Type)]


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


typeInfer (SeqNil tp) env = return $ TSeq tp

typeInfer (Seq es) env = 
    do tps <- mapM (\x -> typeInfer x env) es 
       let tp0 = head tps
       if all (==tp0) tps 
       then return $ TSeq tp0
       else fail "Sequence elements type error!"

typeInfer (Let p e1 e2) env = 
    do tp <- typeInfer e1 env
       typeInfer e2 (typeBind p tp ++ env)

typeInfer (Call i es) env = 
    do ts <- mapM (\x -> typeInfer x env) es
       case lookup i env of 
          Just (TFun f) -> f ts
          Nothing -> fail $ "Typing: Undefined function: " ++ i


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



--type FuncTyEnv = [(Id, [Type] -> SneslTyping Type)]

tyEnv0 :: TyEnv
tyEnv0 = [("not", TFun (\t -> funcTypeChk "not" t [TBool] TBool )),
          ("_uminus", TFun (\t -> funcTypeChk "_uminus" t [TInt] TInt)),              

          ("_plus", TFun (\t -> funcTypeChk "_plus" t [TInt,TInt] TInt)),
          ("_minus",TFun (\t -> funcTypeChk "_minus" t [TInt,TInt] TInt)),          
          ("_times",TFun (\t -> funcTypeChk "_times" t [TInt,TInt] TInt)),
          ("_div",TFun (\t -> funcTypeChk "_div" t [TInt,TInt] TInt)),
          ("_eq", TFun (\t -> funcTypeChk "_eq" t [TInt,TInt] TBool)),
          ("_leq", TFun (\t -> funcTypeChk "_leq" t [TInt,TInt] TBool)),

          ("index", TFun (\t -> funcTypeChk "index" t [TInt] (TSeq TInt))),              

          ("scanExPlus", TFun (\t -> funcTypeChk "scanExPlus" t [TSeq TInt] (TSeq TInt))),
         
          ("reducePlus", TFun (\t -> funcTypeChk "reducePlus" t [TSeq TInt] TInt)),

          ("_append", TFun (\t -> funcTypeChkAppend t)),

          ("concat",TFun (\t -> funcTypeChkConcat t))]



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

