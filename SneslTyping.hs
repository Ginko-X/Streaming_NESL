module SneslTyping where

import SneslSyntax


runTypingEnv :: [Def] -> Either String TyEnv
runTypingEnv defs = 
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
typingDef (FDef fname args rettp e) r =
    do let argtps = snd $ unzip args 
       tp <- typeInfer e (args ++ r)           
       if tp == rettp
       then return $ (fname, TFun argtps rettp) : r
       else fail $ "Function type mismatch: " ++ fname ++ show tp ++ "," ++ show rettp

--typingDef (EDef i e) r = 
--    case typeInfer e r of 
--        Right t -> Right ((i,t):r)
--        Left err -> Left err 





typingExp :: Exp -> TyEnv -> Either String Type
typingExp e env = 
    case typeInfer e (env++tyEnv0) of 
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
          Just (TFun atps rtp) -> if ts == atps then return rtp
                                  else fail $ "Function type mismatch: " ++ i
          Just (TVar f) -> f ts  
          Nothing -> fail $ "SNESL type error: undefined function " ++ i


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

          ("concat",TVar (\t  -> funcTypeChkConcat t))]


funcTypeChkConcat t = 
    case t of 
       [TSeq (TSeq t')] -> return $ TSeq t'
       _ -> fail "Function type or arity mismatch: concat"


funcTypeChkAppend t = 
    case t of 
       [TSeq t1, TSeq t2] -> if t1 == t2 then return $ TSeq t1 
                              else fail "Function type mismatch: _append"
       _ -> fail "Function type or arity mismatch: _append"

