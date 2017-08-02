module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

data SExp = Ctrl 
           | EmptyCtrl  
           | ToFlags SId
           | Usum SId
           | Const AVal
           | MapConst SId AVal           
           | MapOne OP SId
           | MapTwo OP SId SId
           | SegscanPlus SId SId 
           | ReducePlus SId SId
           | Pack SId SId
           | UPack SId SId
           | Distr SId SId
           | SegDistr SId SId
           | SegFlagDistr SId SId SId
           | PrimSegFlagDistr SId SId SId 
           | B2u SId
           | SegConcat SId SId
           | USegCount SId SId
           | SegMerge SId SId 
           | InterMergeS [SId]  
           | SegInterS [(SId,SId)] 
           | PriSegInterS [(SId,SId)] 
           | Check SId SId
           | IsEmpty SId        
           deriving Show
         

data SInstr = SDef SId SExp  -- deriving Show 
            | WithCtrl SId [SId] [SInstr] STree
            | SCall FId [SId] [SId]

data SFun = SFun [SId] STree [SInstr] Int -- deriving Show 


data OP = Uminus | Not  -- unary 
        | Add | Minus | Times | Div | Equal | Leq | Mod   -- binary
        deriving (Eq,Show)

type OpEnv = [(OP, [SvVal] -> SvVal)]
opEnv0 = [(Uminus, \[SIVal as] -> SIVal $ map (\x -> -x) as),
          (Not, \[SBVal as] -> SBVal $ map not as),
          (Add, \[SIVal as, SIVal bs] -> SIVal $ zipWith (+) as bs),
          (Minus, \[SIVal as, SIVal bs] -> SIVal $ zipWith (-) as bs),
          (Times, \[SIVal as, SIVal bs] -> SIVal $ zipWith (*) as bs),
          (Div, \[SIVal as, SIVal bs] -> SIVal $ zipWith div as bs),
          (Mod, \[SIVal as, SIVal bs] -> SIVal $ zipWith mod as bs),
          (Equal, \[SIVal as, SIVal bs] -> SBVal $ zipWith (==) as bs),
          (Leq, \[SIVal as, SIVal bs] -> SBVal $ zipWith (<=) as bs)]




type OpAEnv = [(OP, ([AVal] -> AVal, Type))]
opAEnv0 = [(Uminus, (\[IVal a] -> IVal (-a), TInt)),
          (Not, (\[BVal a] -> BVal $ not a, TBool)),
          (Add, (\[IVal a, IVal b] -> IVal $ a+b, TInt)),
          (Minus, (\[IVal a, IVal b] -> IVal $ a-b, TInt)) ,
          (Times, (\[IVal a, IVal b] -> IVal $ a*b, TInt)),
          (Div, (\[IVal a, IVal b] -> IVal $ a `div` b, TInt)),
          (Mod, (\[IVal a, IVal b] -> IVal $ a `mod` b, TInt)),
          (Equal, (\[IVal a, IVal b] -> BVal $ a == b, TBool) ),
          (Leq, (\[IVal a, IVal b] -> BVal $ a <= b, TBool))]



instance Show SInstr where
  show (SDef sid i) = "S" ++ show sid ++ " := " ++ show i 
  show (WithCtrl sid ss instrs st) = "WithCtrl S" ++ show sid ++ " (import " 
        ++ show ss ++ "):"   ++ concat (map (("\n\t"++).show) instrs) 
        ++ "\n\tReturn: " ++ show st 
  show (SCall f s1 s2) = "SCall " ++ f ++  "\n\tParameters: " 
         ++ show s1 ++ "\n\tReturn: " ++ show s2
         

instance Show SFun where
  show (SFun args ret code count) = "\nParameters: " ++ show args ++ "\n" 
    ++ showseq "; \n" code ++ "\nReturn: " ++ show ret 
    ++ "\nSid count: " ++ show count  ++ "\n"
                             

-- svcode values
data SvVal = SIVal [Int]
           | SBVal [Bool] 
           | SSVal SvVal [Bool]  -- Sequence
           | SPVal SvVal SvVal -- Pair
           deriving Eq


instance Show SvVal where
    show (SIVal is) = "Ints <" ++ showseq "," is ++ ">"
    show (SBVal bs) = "Bools <" ++ showseq "," bs ++ ">"    
    show (SSVal v bs) = "Seq (" ++ show v ++ ","++ show bs ++")"    
    show (SPVal v1 v2) = "Pair (" ++ show v1 ++"," ++ show v2 ++ ")"


showseq delim [] = ""
showseq delim [x] = show x
showseq delim (x:xs) = show x ++ delim ++ showseq delim xs




data STree = IStr SId
           | BStr SId
           | SStr STree SId
           | PStr STree STree
           | FStr ([STree] -> SneslTrans STree)   -- for built-in functions
           | FDStr [STree] STree   -- user-defined functions


instance Show STree where
  show (IStr i) = "(IStr "++ show i ++ ")"
  show (BStr b) = "(BStr " ++ show b ++ ")"
  show (SStr t s) = "(SStr " ++ show t ++ ", " ++ show s ++ ")"
  show (PStr t1 t2) = "(PSTr " ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (FStr _) = "<Built-in function STree>"
  show (FDStr sts st) = "(FDStr "++ show sts ++ ", " ++ show st ++ ")"


type VEnv = [(Id, STree)]

type FEnv = [(FId, SFun)]

newtype SneslTrans a = SneslTrans {rSneslTrans :: SId -> VEnv -> 
                                       Either String (a,[SInstr], SId)}

instance Monad SneslTrans where
    return a = SneslTrans $ \ sid _ -> Right (a, [], sid)

    m >>= f = SneslTrans $ \ sid ve -> 
        case rSneslTrans m sid ve of
            Left err -> Left err
            Right (a, sv, sid')  -> 
                case rSneslTrans (f a) sid' ve of 
                    Left err' -> Left err'
                    Right (a', sv', sid'') -> Right (a', sv++sv', sid'')

    fail err = SneslTrans $ \ _ _ -> Left err 


instance Functor SneslTrans where
  fmap f t = t >>= return . f

instance Applicative SneslTrans where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta


getSupExp :: SExp -> SId -> ([SId],String)
getSupExp Ctrl _ = ([],"Ctrl")
getSupExp EmptyCtrl _ = ([],"EmptyCtrl")
getSupExp (Const a) c = ([c],"Const " ++ show a)

getSupExp (MapConst s1 a) _ = ([s1],"MapConst " ++ show a)
getSupExp (MapOne op s1) _ = ([s1],"MapOne " ++ show op)
getSupExp (MapTwo op s1 s2) _ = ([s1,s2],"MapTwo " ++ show op)

getSupExp (InterMergeS ss) _ = (ss,"InterMergeS")
getSupExp (SegInterS ss) _ = (concat $ map (\(x,y) -> [x,y]) ss , "SegInterS")
getSupExp (PriSegInterS ss) _ = (concat $ map (\(x,y) -> [x,y]) ss, "PriSegInterS") 

getSupExp (Distr s1 s2) _ = ([s1,s2], "Distr")
getSupExp (SegDistr s1 s2) _ = ([s1,s2],"SegDistr")
getSupExp (SegFlagDistr s1 s2 s3) _ = ([s2,s1,s3],"SegFlagDistr")
getSupExp (PrimSegFlagDistr s1 s2 s3) _ = ([s2,s1,s3],"PrimSegFlagDistr")

getSupExp (ToFlags s1) _ = ([s1], "ToFlags")
getSupExp (Usum s1) _ = ([s1],"Usum")
getSupExp (B2u s1) _ = ([s1],"B2u")
getSupExp (IsEmpty s1) _ = ([s1],"IsEmpty")

getSupExp (SegscanPlus s1 s2) _ = ([s2,s1],"SegscanPlus")
getSupExp (ReducePlus s1 s2) _ = ([s2,s1],"ReducePlus")
getSupExp (Pack s1 s2) _ = ([s2,s1],"Pack")
getSupExp (UPack s1 s2) _ = ([s2,s1],"UPack")
getSupExp (SegConcat s1 s2) _ = ([s2,s1],"SegConcat")
getSupExp (USegCount s1 s2) _ = ([s2,s1],"USegCount")
getSupExp (SegMerge s1 s2) _ = ([s2,s1],"SegMerge")  
getSupExp (Check s1 s2) _ = ([s1,s2],"Check")

