module SneslTyping where

import SneslSyntax


runTypingDefs :: [Def] -> TyEnv -> Either String TyEnv
runTypingDefs defs env = 
    case rTyping $ typingDefs defs env of 
        Right t -> Right t 
        Left err -> Left $ "Type-check error: " ++ err 



typingDefs :: [Def] -> TyEnv -> SneslTyping TyEnv
typingDefs [] r = return r  
typingDefs (d:defs) r = typingDef d r >>= typingDefs defs


typingDef :: Def -> TyEnv -> SneslTyping TyEnv
typingDef (FDef fname args rettp e) r =
    do let argtps = snd $ unzip args 
       tp <- typeInfer e ((fname, TFun argtps rettp): args ++ r)           
       if tp == rettp
       then return $ (fname, TFun argtps rettp) : r
       else fail $ "Function type mismatch: " ++ 
                      fname ++ "," ++ show tp ++ "," ++ show rettp

--typingDef (EDef i e) r = 
--    case typeInfer e r of 
--        Right t -> Right ((i,t):r)
--        Left err -> Left err 



runTypingExp :: Exp -> TyEnv -> Either String Type
runTypingExp e env = 
    case  rTyping $ typeInfer e env of 
        Right t -> Right t 
        Left err -> Left $ "Type-check error: " ++ err 



type TyEnv = [(Id, Type)]


typeInfer :: Exp -> TyEnv  -> SneslTyping Type
typeInfer (Var x) env = 
    case lookup x env of
        Just t -> return t
        Nothing -> fail $ "bad variable: " ++ x

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
       else fail "sequence elements type error!"

typeInfer (Let p e1 e2) env = 
    do tp <- typeInfer e1 env
       typeInfer e2 (typeBind p tp ++ env)

typeInfer (Call i es) env = 
    do ts <- mapM (\x -> typeInfer x env) es
       case lookup i env of 
          Just (TFun atps rtp) ->  -- user-defined functions
             if ts == atps then return rtp
                else fail $ "function arguments type mismatch: " ++ i
          Just (TVar f) -> f ts  -- built-in functions
          Just _ -> fail $ "function and variable names are identical: " ++ i
          Nothing -> fail $ "undefined function " ++ i


typeInfer (GComp e0 ps) env = 
    do ts <- mapM (\(_,e) -> typeInfer e env) ps
       if all isSeq ts 
       then (do let binds = concat $ 
                             zipWith (\(p,_) (TSeq t') -> typeBind p t') ps ts
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



tyEnv0 :: TyEnv
tyEnv0 = [("not", TFun [TBool] TBool ),
          ("_uminus", TFun [TInt] TInt),              

          ("_plus", TFun [TInt,TInt] TInt ),
          ("_minus",TFun [TInt,TInt] TInt ),          
          ("_times",TFun [TInt,TInt] TInt ),
          ("_div",TFun [TInt,TInt] TInt ),
          ("_eq", TFun [TInt,TInt] TBool ),
          ("_leq", TFun [TInt,TInt] TBool ),

          ("index", TFun [TInt] (TSeq TInt)),              
          ("scanExPlus", TFun [TSeq TInt] (TSeq TInt)),         
          ("reducePlus", TFun [TSeq TInt] TInt ),
          ("_append", TVar (\t -> funcTypeChkAppend t)),
          ("concat",TVar (\t  -> funcTypeChkConcat t)),
          ("the",TVar (\t -> funcTypeChkThe t)),
          ("empty", TVar(\t -> funcTypeChkEmpty t)),
          ("part", TVar (\t -> funcTypeChkPart t))]

funcTypeChkPart t = 
    case t of 
      [TSeq t', TSeq TBool] -> return $ TSeq (TSeq t')
      _ -> fail "Function type or arity mismatch: part "

funcTypeChkEmpty t = 
    case t of 
      [TSeq t'] -> return $ TBool
      _ -> fail "Function type or arity mismatch: the "

funcTypeChkConcat t = 
    case t of 
       [TSeq (TSeq t')] -> return $ TSeq t'
       _ -> fail "Function type or arity mismatch: concat"


funcTypeChkAppend t = 
    case t of 
       [TSeq t1, TSeq t2] -> if t1 == t2 then return $ TSeq t1 
                              else fail "Function type mismatch: _append"
       _ -> fail "Function type or arity mismatch: _append"


funcTypeChkThe t = 
    case t of 
       [TSeq t'] -> return t'
       _ -> fail "Function type or arity mismatch: the"
