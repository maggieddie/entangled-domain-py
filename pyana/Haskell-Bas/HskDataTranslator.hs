{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module TI.HskDataTranslator (module TI.HskDataTranslator) where
import TI.PyData
import TI.PyEval
import Data.SExp
import Data.Maybe
import Control.Exception
import Text.Printf
import System.Environment (getArgs)
import qualified Data.List as L

                           

sexpToProgram :: SExp -> Program
sexpToProgram [sexp|(program @~ @list:rest)|] = makeProgram rest
sexpToProgram _ = error "bad format in sexpToProgram"

makeProgram :: [SExp] -> Program
makeProgram [] = error "bad format in  makeProgram"
makeProgram [s] = Program [] (sexpToExp s)
makeProgram (s:ss) = 
  let (Program defs e) = makeProgram ss
  in Program (sexpToVarDef s:defs) e

sexpToVarDef :: SExp -> VarDef
sexpToVarDef [sexp|(define @sym:name @:exp)|] = (Define name (sexpToAE exp))
sexpToVarDef _ = error "bad format in vardef"

sexpToExp :: SExp -> Exp

                              
sexpToExp [sexp|@str:str|] = AExp $ AStr str
sexpToExp [sexp|@int:int|] = AExp $ AInt int
--sexpToExp [sexp|@bool:bool|] = AExp$ ABool bool

sexpToExp [sexp|@float:fl|] = AExp $ AFloat fl
-- the following may be not correct!
sexpToExp [sexp|false|] = AExp$ ABool False
sexpToExp [sexp|true|] = AExp$ ABool True
sexpToExp [sexp|$halt|] = AExp $ AHalt
sexpToExp [sexp|None|] = AExp $ ANone
sexpToExp [sexp|Ellipsis|] = AExp $ AEllipsis
sexpToExp [sexp|(void)|] = AExp $ AVoid
sexpToExp [sexp|@sym:s |] = AExp (Var s)

sexpToExp [sexp| (lambda @list:formals @:body) |] = 
  AExp (ELam (Lam (map (\x -> let (Just str) = sexpSymbolValue x
                              in str) formals) (sexpToContext body)))
-- end of AE

-- parse CE
-- the containerK                                        
sexpToExp [sexp|(py-list*-k @~ @list:rest) |] =
    let elems = init rest
        ae = last rest 
    in CExp $ ContainerK PyListK (map sexpToElemE  elems) (sexpToAE ae)
                                                                     
sexpToExp [sexp|(tuple-k @~ @list:rest) |] =
    let elems = init rest
        ae = last rest
    in CExp $ ContainerK TupleK (map sexpToElemE  elems) (sexpToAE ae)
                                                                     
sexpToExp [sexp|(set-k @~ @list:rest) |] =
    let elems = init rest
        ae = last rest
    in CExp $ ContainerK SetK (map sexpToElemE  elems) (sexpToAE ae)
                                                                     
sexpToExp [sexp|(dict-k @~ @list:rest) |] =
    let elems = init rest
        ae = last rest
    in CExp $ ContainerK DictK (map sexpToElemE  elems) (sexpToAE ae)
    

-- forK
sexpToExp [sexp|(for-set-k @~ @list:aes)|] =
   CExp $ CpsFun ForSetK (map sexpToAE aes)
sexpToExp [sexp|(for-py-list-k @~ @list:aes)|] =
   CExp $ CpsFun ForPyListK (map sexpToAE aes)
sexpToExp [sexp|(for-tuple-k @~ @list:aes)|] = 
   CExp $ CpsFun ForTupleK (map sexpToAE aes)
sexpToExp [sexp|(for-dict-k @~ @list:aes)|] = 
   CExp $ CpsFun ForDictK (map sexpToAE aes)
   

    
-- set-field!
sexpToExp [sexp|(set-field! @:ae1 @sym:var @:ae2 @:aek) |]=
   CExp $ SetField (sexpToAE ae1) var (sexpToAE ae2) (sexpToAE aek)
             
-- get-field
sexpToExp [sexp|(get-field  @:ae @sym:var  @:aek) |]=
   CExp $ GetField (sexpToAE ae) var (sexpToAE aek)     

-- set-then! and if
sexpToExp [sexp|(set-then! @sym:var @:ae @:ce)|] = CExp $ SetThen var (sexpToAE ae) (sexpToContext ce)
sexpToExp [sexp|(if @:ae @:cet @:cef)|] = CExp $ If2 (sexpToAE ae) (sexpToContext cet) (sexpToContext cef)  
sexpToExp [sexp|(error @:ae1 @:ae2) |] = CExp $ Error (sexpToAE ae1) (sexpToAE ae2)
                                 

-- the cps unop query : LParen and RParen                                   
sexpToExp [sexp|((cps integer?) @:ae @:aek)|] =
   CExp $ UQuery QInteger (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps  string?) @:ae @:aek)|] =
   CExp $ UQuery QString (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps tuple?) @:ae @:aek)|] =
   CExp $ UQuery QTuple (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps dict?) @:ae @:aek)|] =
   CExp $ UQuery QDict (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps py-list?) @:ae @:aek)|] =
   CExp $ UQuery QPyList (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps set?) @:ae @:aek)|] =
   CExp $ UQuery QSet (sexpToAE ae) (sexpToAE aek)

-- cps unop general
sexpToExp [sexp|((cps bit-wise-not) @:ae @:aek) |] =
   CExp $ UGeneral BitwiseNot  (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps + ) @:ae @:aek) |] =
   CExp $ UGeneral UPlus  (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps - ) @:ae @:aek) |] =
   CExp $ UGeneral UMinus  (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps assert1) @:ae @:aek) |] = 
   CExp $ UGeneral Assert1  (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|((cps py-print) @:ae @:aek )|] =
   CExp $ UGeneral PyPrint  (sexpToAE ae) (sexpToAE aek)
sexpToExp [sexp|(( cps not?) @:ae @:aek) |] =
   CExp $ UGeneral QNot  (sexpToAE ae) (sexpToAE aek)
             
-- cps biop conainer ref             
sexpToExp [sexp|((cps py-list-ref) @:aec @:aei @:aek) |] =
   CExp $ ContainerRef PyListRef  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)
sexpToExp [sexp|(( cps tuple-ref) @:aec @:aei @:aek) |] =
   CExp $ ContainerRef TupleRef  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)
sexpToExp [sexp|(( cps dict-ref ) @:aec @:aei @:aek) |] =
   CExp $ ContainerRef DictRef  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)
-- cps biop container remove
sexpToExp [sexp|((cps py-list-remove!) @:aec @:aei @:aek) |] =
   CExp $ ContainerRmv PyListRemove  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)
sexpToExp [sexp|(( cps tuple-remove!) @:aec @:aei @:aek) |] =
   CExp $ ContainerRmv TupleRemove  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)
sexpToExp [sexp|(( cps dict-remove!) @:aec @:aei @:aek) |] =
   CExp $ ContainerRmv DictRemove  (sexpToAE aec) (sexpToAE aei) (sexpToAE aek)

-- cps compbiop
sexpToExp [sexp|((cps <) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop Less  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps > ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop Greater  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps equal? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QEqual  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps not-equal? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QNotEqual  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps <= ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop LessEqual  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps >= ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop GreaterEqual  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps in? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QIn  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps not-in? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QNotIn  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps eq? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QEq (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps not-eq? ) @:ae1 @:ae2 @:aek) |] =
   CExp $ CompBiop QNeq  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)

-- cps arithmetic biop
sexpToExp [sexp|(( cps + ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Plus  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps - ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Minus  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps * ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Multiply  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps / ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ArithBiop Div  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps quotient ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Quotient  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps modulo ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Modulo  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps expt ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop Expt  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps bitwise-and ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop BitwiseAnd  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps bitwise-or ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop BitwiseOr  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps bitwise-xor ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop BitwiseXor  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps >> ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop ShiftRight  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
sexpToExp [sexp|(( cps << ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ ArithBiop ShiftLeft  (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)
              
sexpToExp [sexp|(( cps assert2 ) @:ae1 @:ae2 @:aek) |] = 
   CExp $ GBiop Assert2 (sexpToAE ae1) (sexpToAE ae2) (sexpToAE aek)


--container set triop
sexpToExp [sexp|(( cps py-list-set! ) @:aec @:aei @:aev  @:aek) |] = 
   CExp $ ContainerSet PyListSet  (sexpToAE aec) (sexpToAE aei) (sexpToAE aev) (sexpToAE aek)
sexpToExp [sexp|((cps dict-set! ) @:aec @:aei @:aev  @:aek) |] = 
   CExp $ ContainerSet DictSet  (sexpToAE aec) (sexpToAE aei) (sexpToAE aev) (sexpToAE aek)
sexpToExp [sexp|((cps tuple-set! ) @:aec @:aei @:aev  @:aek) |] = 
   CExp $ ContainerSet TupleSet  (sexpToAE aec) (sexpToAE aei) (sexpToAE aev) (sexpToAE aek)


--  application
sexpToExp [sexp|(@:ae @~ @list:aes) |] = CExp $ App (sexpToAE ae) (map sexpToAE  aes)  
-- 
sexpToExp s = error $ printf "Failed match in sexpToExp: %s" (unparseSExp s)
--

sexpToAE :: SExp -> AE
sexpToAE s = let (AExp e) = sexpToExp s in e 

sexpToContext :: SExp -> Context
sexpToContext s = let (CExp e) = sexpToExp s in e

sexpToElemE :: SExp -> ElemE
sexpToElemE [sexp| (@:ae1 @:ae2)|] = DictPair ((sexpToAE ae1), (sexpToAE ae2))
sexpToElemE [sexp| @:ae|] = SingleE (sexpToAE ae)
                                 

genHskDataT :: String -> IO ()
genHskDataT path = do
  contents <- readFile path
  let (Right sexpContents) = parseSExp contents
      hskData = sexpToProgram sexpContents
  print (show hskData)
            
  
              


   -- parseInput "/home/shuying/hs/TI/tests/t13.py.cps2"
       
