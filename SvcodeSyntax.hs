module SvcodeSyntax where

import SneslSyntax

type SId = Int  -- stream id

data SExp = Ctrl 
           | EmptyCtrl 
           | WithCtrl SId [SInstr] STree
           | SCall FId [(SId,SId)] STree
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
           | SegMerge SId SId 
           | InterMergeS [SId]
           | SegInterS [(SId,SId)]
           | PriSegInterS [(SId,SId)] 
           | Check SId SId           
           deriving Show
         
data SInstr  = SDef SId SExp  -- deriving Show 

data SFun = SFun SId [STree] STree [SInstr] -- deriving Show 


data OP = Uminus | Not  -- unary 
        | Add | Minus | Times | Div | Equal | Leq   -- binary
        deriving (Eq,Show)

type OpEnv = [(OP, [SvVal] -> SvVal)]
opEnv0 = [(Uminus, \[SIVal as] -> SIVal $ map (\x -> -x) as),
          (Not, \[SBVal as] -> SBVal $ map not as),
          (Add, \[SIVal as, SIVal bs] -> SIVal $ zipWith (+) as bs),
          (Minus, \[SIVal as, SIVal bs] -> SIVal $ zipWith (-) as bs),
          (Times, \[SIVal as, SIVal bs] -> SIVal $ zipWith (*) as bs),
          (Div, \[SIVal as, SIVal bs] -> SIVal $ zipWith div as bs),
          (Equal, \[SIVal as, SIVal bs] -> SBVal $ zipWith (==) as bs),
          (Leq, \[SIVal as, SIVal bs] -> SBVal $ zipWith (<=) as bs)]


instance Show SInstr where
  show (SDef sid i) = "S" ++ show sid ++ " := " ++ show i 


instance Show SFun where
  show (SFun ctrl args ret code) = "\nCtrl:" ++ show ctrl ++ "\nArguments: " 
      ++ show args ++ "\n" ++ showseq "; \n" code ++ "\nReturn: " ++ show ret
                             

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
           | FStr ([STree] -> SneslTrans STree)   -- built-in functions
           | FDStr [STree] STree   -- user-defined functions


instance Show STree where
  show (IStr i) = "(IStr "++ show i ++ ")"
  show (BStr b) = "(BStr " ++ show b ++ ")"
  show (SStr t s) = "(SStr " ++ show t ++ "," ++ show s ++ ")"
  show (PStr t1 t2) = "(PSTr " ++ show t1 ++ "," ++ show t2 ++ ")"
  show (FStr _) = "<Built-in function STree>"


type VEnv = [(Id, STree)]

type FEnv = [(FId, SFun)]

newtype SneslTrans a = SneslTrans {rSneslTrans :: SId -> VEnv -> FEnv -> 
                                       Either String (a,[SInstr], SId)}

instance Monad SneslTrans where
    return a = SneslTrans $ \ sid ve fe -> Right (a, [], sid)

    m >>= f = SneslTrans $ \ sid ve fe -> 
        case rSneslTrans m sid ve fe of
            Left err -> Left err
            Right (a, sv, sid')  -> 
                case rSneslTrans (f a) sid' ve fe of 
                    Left err' -> Left err'
                    Right (a', sv', sid'') -> Right (a', sv++sv', sid'')

    fail err = SneslTrans $ \ _ _ _ -> Left err 


instance Functor SneslTrans where
  fmap f t = t >>= return . f

instance Applicative SneslTrans where
  pure = return
  tf <*> ta = tf >>= \f -> fmap f ta

