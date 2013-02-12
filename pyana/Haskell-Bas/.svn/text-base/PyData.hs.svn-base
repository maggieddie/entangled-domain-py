module TI.PyData (module TI.PyData) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

-- grammar
type Symbol = String

data Program = Program [VarDef] Exp deriving (Show, Eq, Ord)
data VarDef  = Define Symbol AE deriving (Show, Eq, Ord)
data Exp     = AExp AE | CExp Context   deriving (Show, Eq, Ord)

data AE =
        Var Symbol
      | ELam Lam
      | AStr String
      | AInt Integer
      | ABool Bool
      | AFloat Double
      | AHalt
      | ANone
      | AEllipsis
      | AVoid
      deriving (Show, Eq, Ord)

data Lam =  Lam {formals :: [Symbol], body :: Context}
            deriving (Show, Eq, Ord)

                     
data Context = App AE [AE]

             | SetThen Symbol AE Context

             | If2 AE Context Context
               
             | UQuery QUnop  AE AE
             | UGeneral GUnop AE AE
               
             | ContainerK Container [ElemE] AE
               
             | ContainerRef BCRef AE AE AE
             | ContainerRmv BCRmv AE AE AE
             | CompBiop BComp AE AE AE
             | ArithBiop BArith  AE AE AE
             | GBiop BGeneral AE AE AE
               
             | ContainerSet Triop AE AE AE AE

             | CpsFun ForK [AE]             

             | SetField AE Symbol AE AE
             | GetField AE Symbol AE
             | Error AE AE

             | CHalt
             
               deriving (Show, Eq, Ord)             
                
data GUnop = BitwiseNot | UPlus | UMinus                      
                  | Assert1  | PyPrint | QNot
                    deriving (Show, Eq, Ord)
data QUnop =  QInteger | QString | QTuple | QDict | QPyList | QSet
                deriving (Show, Eq, Ord)

data ForK = ForSetK
            | ForPyListK
            | ForTupleK
            | ForDictK
              deriving (Show, Eq, Ord)

data Container = PyListK | TupleK | DictK | SetK
                 deriving (Show, Eq, Ord)

data ElemE  = SingleE AE | DictPair (AE, AE)
              deriving (Show, Eq, Ord)
            
data BCRef = PyListRef | TupleRef | DictRef
                      deriving (Show, Eq, Ord)

data BCRmv = PyListRemove | TupleRemove | DictRemove
                      deriving (Show, Eq, Ord)
                               
data BComp = Less | Greater  | QEqual | QNotEqual
              | LessEqual | GreaterEqual
              | QIn | QNotIn
              | QEq | QNeq
                deriving (Show, Eq, Ord)
                     
data BArith = Plus | Minus | Multiply | Div
               | Quotient | Modulo | Expt
               | BitwiseAnd | BitwiseOr | BitwiseXor
               | ShiftRight | ShiftLeft
                 deriving (Show, Eq, Ord)
                     
data BGeneral =  Assert2 deriving (Show, Eq, Ord)
          
data Triop = PyListSet | DictSet | TupleSet deriving (Show, Eq, Ord)
              



-- the test function
copyExp :: Context -> AE
copyExp contxt = case contxt of
             -- app  -> app
              
              setthen@(SetThen v ae ce) ->  ae

             -- uop@(QUnop QInteger ae1 ae2) -> ae1
                                            
              --CpsFun aes            -> L.head aes
              -- ContainerK _  elems ae   -> ae
              ArithBiop _  ae1 ae2 ae3 -> ae3
testProg :: Program -> Context
testProg prog = case prog of
                  (Program vardefs (CExp contxt)) -> contxt
                  otherwise -> error "context not matched"
              
extractExp :: Program -> Exp
extractExp prog = case prog of
                    (Program vardefs exp) -> exp
                    otherwise -> error "not a program!"
                                          
-- state space

data Bas  =
    BasInt Integer
  | BasFloat Double
  | BasStr String
  | BasBool Bool
  deriving (Show, Eq, Ord)

type State = (Context, BEnv, VStore, SStore, OStore, Time)

type Bind = (Symbol, Time)
type Addr = (Symbol, Time)
type BEnv = M.Map Symbol Bind
type VStore = M.Map Bind (S.Set Val)
type SStore = M.Map Bind (S.Set Symbol)
type OStore = M.Map Addr (S.Set Obj)


data Val = Clo {lam :: Lam, benv :: BEnv}
         | VBas Bas
         | VAddr Addr
         | VHalt
         | VAbstract Symbol
         | VVoid
           deriving (Show, Eq, Ord)

data Obj = PyList [S.Set Val]
        |  Tuple [S.Set Val]
        |  Set [S.Set Val]
        |  Dict [(S.Set Val, S.Set Val)]
           deriving (Show, Eq, Ord)
           
type Clo = (Lam, BEnv)
    
   -- finite set of the time/contours and addresses?
type Time = [Int]
--type Addr = Bind

-- transition rules is in PyEval.hs

-- datas for Type information
data Type = Int | Float | Bool | String |
           TPyList | PyTuple | PySet | PyDict | Proc | TAbstract Symbol
           deriving (Show, Eq, Ord)
            
type MonoTypeStore = M.Map Symbol (S.Set Type)

