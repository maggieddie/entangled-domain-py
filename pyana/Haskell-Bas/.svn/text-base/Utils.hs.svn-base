module TI.Utils (module TI.Utils) where
import TI.PyData
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import Text.Printf

nextT :: Int -> Int
nextT t = t + 1

tick :: Time ->Int -> Time
tick historyTime k =  L.take k (getLastTime historyTime)    where
    getLastTime lst = case lst of
                        [] -> [nextT 0]
                        (x:xs) -> ((nextT x):x:xs)

alloc ::Time -> Symbol -> Addr
alloc t s = (s, t)

makeBinding :: Time -> Symbol -> Bind
makeBinding t s = (s,t)
                  
atomEval :: BEnv -> VStore -> AE -> S.Set Val
atomEval β σ ae = case ae of
                    (Var v) ->   myLookup ( myLookup v β) σ
                    (ELam elam) -> S.singleton (Clo elam β)
                    (AStr s)    -> S.singleton (VBas (BasStr s))
                    (AInt i)    -> S.singleton (VBas (BasInt i))
                    (ABool b)   -> S.singleton (VBas (BasBool b))
                    (AFloat fl) -> S.singleton (VBas (BasFloat fl))       
                    AHalt       -> S.singleton (VHalt)
                    otherwise   -> S.singleton (VVoid)  -- The AEllipses, ANone, AVoid is temprarily treated as VVoid | 
                                   
extendBEnv :: BEnv -> [Symbol] -> [Bind] -> BEnv
extendBEnv  β vars bindings = foldr (\ (v, b) m -> M.insert v b m) β (zip vars bindings)

updateVStore :: VStore -> [Bind] -> [S.Set Val]-> VStore
updateVStore σ_v bindings vals =  foldr (\ (b, val) m -> M.insertWith S.union b  val m) σ_v (zip bindings vals)

updateSStore :: SStore -> Bind -> S.Set Symbol -> SStore
updateSStore σ_s binding connectedVar = M.insertWith S.union binding connectedVar σ_s

updateOStore :: OStore -> Bind -> S.Set Obj -> OStore
updateOStore σ_o binding obj = M.insertWith S.union binding obj σ_o
                               

-- the containers are not atom anymore
getObj :: Container ->  [ElemE] -> BEnv -> VStore -> S.Set Obj
getObj container elems β σ_v =
    case container of
      PyListK ->  S.singleton $ PyList singleVal
      TupleK  -> S.singleton $ Tuple singleVal
      SetK    -> S.singleton $ Set  singleVal
      DictK   -> S.singleton $ Dict dictVal
    where
      -- the element expression in the container will be turned into a lis t of set of Val
      singleVal =   L.map (\x -> case x of
                                   SingleE ae ->  atomEval β σ_v ae) elems
      dictVal = L.map (\x -> case x of 
                               DictPair (ae1, ae2) -> (atomEval β σ_v ae1, atomEval β σ_v ae2)) elems    

buildNewObjVar :: (Show a) => String -> a -> String
buildNewObjVar obj index  =   obj ++ "." ++ (show index)

-- new version
-- bldObjDottedVar :: (Show a) => a -> String -> String
bldObjDottedVar :: String -> String -> String
bldObjDottedVar index obj = obj ++ "." ++   index

-- 
--bldObjDottedVars :: (Show a) => [String] -> a -> [String]
bldObjDottedVars ::  [String] -> String -> [String]
bldObjDottedVars objLst field = L.map (bldObjDottedVar field)  objLst

mkDottedObjBindings :: [String] -> Time -> BEnv -> [Bind]
mkDottedObjBindings dottedObjLst time benv = L.map (\x -> if M.member x benv then (benv M.! x) else (alloc time x)) dottedObjLst

getValsOfConnectedDottedObjs :: [String] -> BEnv -> VStore -> S.Set Val
getValsOfConnectedDottedObjs dottedObjLst benv vstore =
    L.foldr func S.empty dottedObjLst where
        func x resSet
             | M.member x benv = S.union resSet ( vstore M.! (benv M.! x))
             | otherwise = S.union resSet S.empty

-- the helper functions used in the container ref

getObjs :: S.Set Val -> OStore -> S.Set Obj
getObjs addrs σ_o = S.fold (\x resSet -> case x of
                                  VAddr addr -> S.union  ( myLookup addr σ_o) resSet
                                  otherwise  -> resSet)
                    S.empty addrs


 

getFieldVals :: S.Set Obj -> Val -> S.Set Val
getFieldVals objs valIndex = S.fold (\x resSet
                                         -> case x of
                                              PyList valL -> S.union (valL L.!! (fromIntegral index)) resSet
                                              Tuple valT -> S.union (valT L.!! (fromIntegral index)) resSet
                                              Set   valS -> S.union (valS L.!! (fromIntegral index)) resSet
                                              Dict  valD ->
                                                  let dictRes = S.fold (\ (keySet, valSet) allValSet
                                                                            -> S.union allValSet
                                                                               (if S.member valIndex keySet then valSet
                                                                                else S.empty)) S.empty (S.fromList valD)
                                                  in S.union (dictRes) resSet)
                             S.empty objs  where
                                 index = case valIndex of
                                           VBas (BasInt i) -> i
                                           otherwise -> error "the index of container should be int!!!"

-- used in the set-field! to get the chained connected var
bldConnectedVarLst :: Symbol -> BEnv -> SStore -> [String]
bldConnectedVarLst var benv sstore =
    let conVarSet =  M.lookup (benv M.! var) sstore --myLookup ( myLookup var benv) sstore
    in  bldLst conVarSet
        where
          bldLst cvs =
              case cvs of
                Nothing -> []
                Just res ->
                    let conVar = L.head $ S.elems res
                    in (conVar : (bldConnectedVarLst conVar benv sstore))
              --  S.null cvs = []
              -- otherwise  =
              --   let conVar = L.head $ S.elems cvs
              --   in (conVar : (bldConnectedVarLst conVar benv sstore))
          

-- 
getVarSet :: AE -> S.Set Symbol
getVarSet ae = case ae of
                 Var v -> S.singleton v
                 otherwise -> S.empty

getVar :: AE -> Symbol
getVar ae = case ae of
              Var v -> v
              otherwise -> ""

myLookup ::  (Ord k, Show k) => k -> M.Map k a -> a
myLookup k m =
    let pk = M.lookup k m
    in
      case pk of
        (Just key) -> key
        Nothing -> error $ printf "%s Not Found!! " (show k)
        
--
mergeBEnvs ::  BEnv -> BEnv -> BEnv
mergeBEnvs benv1 benv2 = M.foldrWithKey  (\ key val resM -> if  M.member key resM
                                                          then
                                                             if key `elem` ["cc", "return", "x"]
                                                             then M.insert key val resM
                                                             else M.insert key (resM M.! key) resM
                                                          else M.insert key val resM)
                        benv1
                        benv2
                                                                     
-- 
updateSStore2 :: BEnv -> SStore -> [Bind] -> [Symbol] -> SStore
updateSStore2 benv sstore bindings vars =
    L.foldr (\(key, val) resM ->  if M.member val  benv then M.insert key (S.singleton val) resM else resM )
     sstore  (L.zip bindings vars)
                                    
--

bestEffortArithVal :: Val -> Val -> Val
bestEffortArithVal val1 val2 =
    case val1 of
      VBas (BasInt _) -> case val2 of
                         VBas  (BasInt _) -> VBas ( BasInt 0)
                         VBas  (BasFloat _) -> VBas  (BasFloat 0.001)
                         VBas  (BasStr _) -> VBas   (BasStr "String")
                         otherwise -> VVoid
      VBas   (BasStr _) -> case val2 of
                         VBas (BasInt _)  ->VBas (BasStr "String")
                         otherwise ->  VVoid
      VBas  (BasFloat _) -> VBas (BasFloat 0.001)
      otherwise -> VVoid
                   
mapSetArith :: S.Set Val -> S.Set Val -> S.Set Val
mapSetArith valSet1 valSet2 =
    S.fromList [bestEffortArithVal val1 val2 | val1 <- (S.elems valSet1), val2 <- (S.elems valSet2)]
