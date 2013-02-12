module TI.PyEval (module TI.PyEval) where
import TI.PyData
import TI.Utils
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import Text.Printf

-- State exploration

next :: State -> [State]
next (ContainerK container aes aek, β, σ_v, σ_s, σ_o, t) =
    let t' = tick t 1
    in L.nub [let v = L.head formals
                  binding = makeBinding t' v
                  addr    = alloc t' v -- using the same alloc to get unique identifier
                  obj  = getObj container aes β σ_v              
                  β'   = extendBEnv benv [v] [binding]
                  σ_v' = updateVStore σ_v [binding] [S.singleton (VAddr  addr)]
                  σ_o' = updateOStore σ_o addr obj
                  σ_s' = updateSStore σ_s binding S.empty
              in (body, β', σ_v', σ_s', σ_o', t')
              | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek
             ]
    

next (ContainerSet containerSet aec aei aev aek, β, σ_v, σ_s, σ_o, t) =  

    let t' = tick t 1
        container = case aec of
                      Var v -> v
                      otherwise   ->  error "wrong matching in the container set"
    in L.nub [let  obj = L.head $ S.elems (σ_s M.! (β M.! container)) -- container symbol
                   index = L.head $ S.elems $ atomEval β σ_v aei
                   newObjVar = buildNewObjVar obj index 
                   
                   newVal     = L.head $ S.elems $  atomEval β σ_v aev

                   binding = if M.member newObjVar β then β M.! newObjVar
                             else makeBinding t' newObjVar
                                  
                   svar = case aev of
                            Var v -> S.singleton v
                            _ -> S.empty
                                 
                   σ_s' = updateSStore σ_s binding svar
                                              
                   β' = extendBEnv benv [newObjVar] [binding]
                        
                   newState = case newVal of
                                VAddr vaddr@(_, _) ->
                                    let  addr = alloc t' newObjVar
                                        
                                         vobj  = σ_o M.! vaddr
                                         σ_v' = updateVStore σ_v [binding] [S.singleton (VAddr  addr)]
                                         σ_o' = updateOStore σ_o addr vobj
                                        
                                    in (body, β', σ_v', σ_s', σ_o', t')
                                    
                                _ -> let σ_v' = updateVStore σ_v [binding] [S.singleton newVal]
                                         σ_o' = σ_o                        
                                     in (body, β', σ_v', σ_s', σ_o', t')
                                  
              in newState
              | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek
             ]



-- The transition rule for container ref biop

next (ContainerRef bcref aec aei aek, β, σ_v, σ_s, σ_o, t) =

    let t' = tick t 1
        container = case aec of
                      Var v -> v
                      otherwise   ->  error "wrong matching in the container set"
    in L.nub [let  v = L.head formals

                   obj = L.head $ S.elems (σ_s M.! (β M.! container)) -- container symbol
                   index = L.head $ S.elems $ atomEval β σ_v aei
                   newObjVar = buildNewObjVar obj index                   

                   newVal = if M.member newObjVar β then atomEval β σ_v (Var newObjVar)
                            else
                              let addrs = σ_v M.! (β M.! container)
                                  objs = getObjs addrs σ_o
                              in getFieldVals objs index
                                   
                   binding = makeBinding t' v
                   β'   = extendBEnv benv [v] [binding]
                   σ_v' = updateVStore σ_v [binding] [newVal]
                   σ_o' = σ_o
                   σ_s' = updateSStore σ_s binding S.empty
                                  
              in (body, β', σ_v', σ_s', σ_o', t')
              | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek
              ]

-- The abstract type is not in for simplicity, but need to be added later on! 
next (SetThen var ae ce, β, σ_v, σ_s, σ_o, t) =

    let t' =  tick t 1
        varS = getVarSet ae                   
        svar = case ae of
                 Var v -> S.singleton v
                 otherwise -> S.empty
        newVal = atomEval β σ_v ae
        
        binding = β M.! var
        binding2 = if S.null varS
                   then alloc t'  ""
                   else alloc t'  (getVar ae)
        β' = if S.null varS
             then β
             else extendBEnv β [(getVar ae)] [binding2]
           
       
        σ_v' = updateVStore σ_v [binding] [newVal]
               
        σ_v'' = if (not (S.null varS)) -- ae is var
                then
                    if  M.notMember (getVar ae)  β
                    then
                        let σ_v' =  updateVStore σ_v [binding2] [S.singleton (VAbstract (getVar ae))]
                        in updateVStore σ_v' [binding] [atomEval β' σ_v' ae]
                    else  updateVStore σ_v [binding] [atomEval β σ_v ae] -- updateVStore σ_v' [binding2] [atomEval β σ_v ae]
                else  updateVStore σ_v [binding] [atomEval β σ_v ae]                
                
        σ_s' = updateSStore σ_s binding svar
            
    in [(ce, β', σ_v'', σ_s', σ_o, t')]
       
next (If2 ae cet cef, β, σ_v, σ_s, σ_o, t) =
    [(cet, β, σ_v, σ_s, σ_o, t), (cef, β, σ_v, σ_s, σ_o, t)]

next (UQuery unop ae aek, β, σ_v, σ_s, σ_o, t) =
    
    let t' = tick t 1
    in L.nub $ L.concat
           [let rv = L.head formals
                binding1 = makeBinding t' rv
                β'  = extendBEnv benv [rv] [binding1]
                σ_v' = updateVStore σ_v  [binding1] [S.fromList [(VBas (BasBool False))]]
                σ_o' = σ_o
                σ_s' = updateSStore σ_s binding1 S.empty
                ae_var = getVar ae
                newState = if S.null $ getVarSet ae then [(body,  β', σ_v', σ_s', σ_o', t')] -- not var
                           else
                               case body of
                                 If2 ae_if cet cef -> let binding2 = makeBinding t' ae_var
                                                          β'' = extendBEnv β' [ae_var] [binding2]
                                                          σ_v'' = updateVStore σ_v' [binding2] [atomEval benv σ_v' ae]
                                                          σ_s'' = updateSStore σ_s' binding2 (σ_s M.! (benv M.! ae_var))
                                                      in [(cet, β'', σ_v'', σ_s'', σ_o, t'), (cef, β', σ_v', σ_s', σ_o', t')]
                                 otherwise ->  [(body,  β', σ_v', σ_s', σ_o', t')]
                                                         
                         
            in newState
                | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek ]            

-- Some duplicated code as above
next (UGeneral guop ae aek, β, σ_v, σ_s, σ_o, t) =

    let t' = tick t 1
    in L.nub [
            let rv = L.head formals
                binding = makeBinding t' rv
                β'  = extendBEnv benv [rv] [binding]                      
                σ_s' = updateSStore σ_s binding S.empty
                σ_v' =
                     case guop of
                       UPlus ->  updateVStore σ_v  [binding] [S.fromList [(VBas (BasInt 10000))]] -- best effort 
                       UMinus -> updateVStore σ_v  [binding] [S.fromList [(VBas (BasInt 10000))]] -- best effort  
                       Assert1 ->  σ_v    
                       PyPrint -> σ_v    
                       QNot ->  updateVStore σ_v  [binding] [S.fromList [(VBas (BasBool True))]] -- best effort                           
                newState = case guop of
                             UPlus ->  (body, β', σ_v', σ_s', σ_o, t')
                             UMinus ->  (body, β', σ_v', σ_s', σ_o, t')
                             QNot  ->  (body, β', σ_v', σ_s', σ_o, t')
                             Assert1 ->  (body, β, σ_v, σ_s, σ_o, t)
                             PyPrint ->  (body, β, σ_v, σ_s, σ_o, t)
            in newState | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek ] 
           
next (SetField ae1 var ae2 aek, β, σ_v, σ_s, σ_o,  t) =
    let t' = tick t 1
    in L.nub [

                   let newVal = L.head $ S.elems (atomEval β σ_v ae2)
                       dottedObj = bldObjDottedVar var (getVar ae1)
                       conDottedObjVarLst = bldObjDottedVars (bldConnectedVarLst  (getVar ae1) β σ_s) var
                       conDottedObjVals = if L.null conDottedObjVarLst
                                          then S.empty
                                          else getValsOfConnectedDottedObjs conDottedObjVarLst β σ_v
                       conBindings = if L.null conDottedObjVarLst
                                     then []
                                     else mkDottedObjBindings conDottedObjVarLst t' β
                       dottedObjBinding =
                            case e of
                               (Clo (Lam formals body) benv) ->
                                   if M.member dottedObj benv
                                   then  myLookup dottedObj benv
                                   else (alloc t' dottedObj)
                               otherwise ->
                                   if M.member dottedObj β
                                   then  myLookup dottedObj β
                                   else (alloc t' dottedObj)
                                
                       svar = getVarSet ae2
                       σ_s' = updateSStore σ_s dottedObjBinding svar
                       β' = case e of
                               (Clo (Lam formals body) benv) ->
                                   extendBEnv benv [dottedObj] [dottedObjBinding]
                               otherwise -> extendBEnv β [dottedObj] [dottedObjBinding]
                       
                       β'' = if L.null conDottedObjVarLst
                             then β'
                             else extendBEnv β' conDottedObjVarLst conBindings
                        
                       newState = case newVal of
                                    VAddr vaddr@(_, _ ) ->
                                              let  addr = alloc t' dottedObj
                                                   obj =  myLookup vaddr σ_o
                                                   conAddres = if L.null conDottedObjVarLst
                                                               then []
                                                               else L.map (\x -> VAddr (alloc t' x)) conDottedObjVarLst
                                                   conAddres2 = if L.null conDottedObjVarLst
                                                                then []
                                                                else L.map (alloc t') conDottedObjVarLst
                                                   conAddrSet = L.map S.singleton conAddres                 
                                                   allObjs = S.union obj (getObjs conDottedObjVals σ_o)
                                                                  
                                                   σ_v' = updateVStore σ_v [dottedObjBinding] [S.union (S.singleton (VAddr addr)) conDottedObjVals]
                                                   σ_o' = updateOStore σ_o addr allObjs
                                                               
                                                   σ_v'' = if L.null conDottedObjVarLst
                                                           then σ_v'
                                                           else updateVStore σ_v' conBindings conAddrSet
                                                   σ_o'' = if L.null conDottedObjVarLst
                                                           then σ_o'
                                                           else L.foldr (\(key, val) resM -> M.insert key val resM) σ_o'  (L.zip conAddres2 (replicate (L.length conDottedObjVarLst) allObjs))
                                                 --pdateOStore σ_o' conAddres2 (replicate (L.length conDottedObjVarLst) allObjs)
                                              in
                                                case e of
                                                  (Clo (Lam formals body) benv) ->
                                                      (body, β'', σ_v'', σ_s', σ_o'', t')
                                                  otherwise -> (CHalt, β'', σ_v'', σ_s', σ_o'', t')
                                           
                                    otherwise ->
                                        let newValSet = if L.null conDottedObjVarLst
                                                        then S.singleton newVal
                                                        else
                                                            S.union (S.singleton newVal) conDottedObjVals
                                            σ_v' = updateVStore σ_v [dottedObjBinding] [newValSet]
                                            σ_v'' = if L.null conDottedObjVarLst
                                                    then σ_v'
                                                    else updateVStore σ_v' conBindings (replicate (L.length conDottedObjVarLst) newValSet)                                     
                                        in
                                          case e of
                                                  (Clo (Lam formals body) benv) ->
                                                      (body, β'', σ_v'', σ_s', σ_o, t')
                                                  otherwise -> (CHalt, β'', σ_v'', σ_s', σ_o, t')
                                         
                       
     
           --in newState |  (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek ]
                   in newState | e <- S.toList $ atomEval β σ_v aek]
            
    
next (GetField ae var aek, β, σ_v, σ_s, σ_o, t) =
    let t' = tick t 1
    in L.nub [
            let rv = L.head formals
                dottedObj = bldObjDottedVar var (getVar ae)
                conDottedObjVarLst =  bldObjDottedVars (bldConnectedVarLst (getVar ae) β σ_s) var
                conDottedObjVals = if L.null conDottedObjVarLst
                                   then S.empty
                                   else getValsOfConnectedDottedObjs conDottedObjVarLst β σ_v
                binding  = makeBinding t' rv
                β' = extendBEnv benv [rv] [binding]
                valRes = func
                    where
                      func
                          | M.notMember dottedObj β = conDottedObjVals
                          | otherwise =
                              let dottedVal = atomEval β σ_v (Var dottedObj)
                              in func2 dottedVal
                                  where
                                    func2 val
                                        | L.null  conDottedObjVarLst = val
                                        | otherwise = S.union val  conDottedObjVals
                                                           

                                   
                               -- otherwise =  conDottedObjVals

                         -- if M.member dottedObj β
                         -- then
                            --  let dottedVal = atomEval β σ_v (Var dottedObj)
                             -- in func dottedVal where
                                --  func val
                                   --    | L.null  conDottedObjVarLst = val
                                     --  otherwise = S.union val  conDottedObjVals
                         -- else  conDottedObjVals
                             
                σ_v' = updateVStore σ_v [binding] [valRes]
                σ_s' = updateSStore σ_s binding S.empty
                        
            in (body, β', σ_v', σ_s', σ_o, t') | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek ]


next (ArithBiop op ae1 ae2 aek, β, σ_v, σ_s, σ_o, t) =
    let t' = tick t 1
    in L.nub [
            let rv = L.head formals
                binding = makeBinding t' rv
                β' = extendBEnv benv [rv] [binding]                     
                possibleValSet = mapSetArith (atomEval benv σ_v ae1) (atomEval benv σ_v ae2)
                σ_v' = updateVStore σ_v [binding] [possibleValSet]
                σ_s' = updateSStore σ_s binding S.empty
                
            in (body, β', σ_v', σ_s', σ_o, t')  | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek]

next (CompBiop op ae1 ae2 aek,  β, σ_v, σ_s, σ_o, t) =
    let t' = tick t 1
    in L.nub [
            let rv = L.head formals
                binding = makeBinding t' rv
                β' = extendBEnv benv [rv] [binding]                     
                possibleValSet = S.singleton (VBas (BasBool False)) -- best effort value
                σ_v' = updateVStore σ_v [binding] [possibleValSet]
                σ_s' = updateSStore σ_s binding S.empty
                
            in (body, β', σ_v', σ_s', σ_o, t')  | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek]

next (GBiop op ae1 ae2 aek, β, σ_v, σ_s, σ_o, t) =   
  L.nub  [(body, β, σ_v, σ_s, σ_o, t)  | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aek]

next (App aefunctor aeLst,  β, σ_v, σ_s, σ_o, t) =
    let t' = tick t 1
    in L.nub [
            let paramVals = L.map func aeLst
                            where
                              func x
                                   |  S.null (getVarSet x) = atomEval β σ_v x
                                   |  otherwise = if  M.member (getVar x) β
                                                  then (atomEval β σ_v x)
                                                  else S.singleton (VAbstract (getVar x))
    
                bindings = L.map (makeBinding t') formals
                β' = extendBEnv benv formals bindings
                σ_v' = updateVStore σ_v bindings paramVals
                σ_s' = updateSStore2 β σ_s bindings (L.map getVar aeLst)

                newState = if (getVar aefunctor) `elem` ["return", "cc"]
                           then (body, (mergeBEnvs β β'), σ_v', σ_s', σ_o, t')
                           else (body, β', σ_v', σ_s', σ_o, t')
            in newState  | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aefunctor]

-- next (CpsFun forK aeLst, β, σ_v, σ_s, σ_o, t) =
--    let t' = tick t 1
--    in
--      L.nub [
--            let containerS = atomEval β σ_v (L.head aeLst)
                            
                            
  --          in newState  | (Clo (Lam formals body) benv) <- S.toList $ atomEval β σ_v aefunctor]
--]
    
next _ = []

--State Space exploration —  return all the explored states
explore :: [State] -> [State] -> [State]
explore seen todo = case todo of
                      [] -> seen
                      (x:xs) -> explore (L.insert x seen) (L.union (next x) xs)
                                
--collapsing the value and Obj  store
summarizeV :: [State] -> VStore
summarizeV stateLst = foldr (\ (_, _, vstore, _, _, _ ) m -> (M.unionWith S.union vstore m)) M.empty stateLst

summarizeO :: [State] -> OStore
summarizeO stateLst = foldr (\ (_, _, _, _, ostore, _ ) m -> (M.unionWith S.union ostore m)) M.empty stateLst

-- collapsing stores to get type information
monoTypeStore ::   VStore -> OStore -> MonoTypeStore
monoTypeStore vstore ostore =
    M.foldrWithKey (\ (var, _) vs m -> M.insertWith S.union var (inferTypes vs ostore) m)  M.empty  vstore

inferTypes :: S.Set Val ->  OStore -> S.Set Type
inferTypes setofVal ostore =
    foldr (\ val  m -> case val of
                             VAddr addr -> S.union (inferObjType (myLookup addr ostore)) m
                             VAbstract av -> S.union (S.singleton (TAbstract av))  m 
                             otherwise -> S.union (inferBasType val) m)
          S.empty (S.toList setofVal)


inferBasType :: Val -> S.Set Type
inferBasType val = case val of
                     Clo {lam = lam, benv = benv} -> S.singleton Proc
                     VBas (BasInt _ ) ->  S.singleton Int
                     VBas (BasFloat _ ) ->  S.singleton Float
                     VBas (BasStr _ ) ->  S.singleton String
                     VBas (BasBool _ ) ->  S.singleton Bool
                     VHalt ->  S.singleton Proc
                     otherwise -> S.empty --error $ printf  (show val)


inferObjType :: S.Set Obj -> S.Set Type
inferObjType objs =
    foldr (\ obj m -> case obj of
                        PyList vals ->  S.union (S.singleton TPyList) m
                        Tuple vals ->   S.union (S.singleton PyTuple) m
                        Set vals ->   S.union (S.singleton PySet) m
                        Dict vals ->  S.union (S.singleton PyDict) m
          )
          S.empty (S.toList objs)



-- entry point
inferPyType :: Program -> MonoTypeStore
inferPyType prog = case prog of
                     (Program vardefs (CExp contxt)) -> analyze contxt benv0 where benv0 = initBenv vardefs   
                         
                     otherwise -> error "context not matched in the entry point"
               
-- analyzer entry point
analyze :: Context -> BEnv -> MonoTypeStore
analyze ce benv =
    let states = explore [] [(ce, benv, M.empty, M.empty, M.empty, [])]
    in monoTypeStore (summarizeV states) (summarizeO states)


initBenv :: [VarDef] -> BEnv
initBenv vardefs = foldr ( \ (Define var ae) m ->
                               let t = tick [] 1
                                   binding = makeBinding t var
                               in M.insert var binding m) M.empty vardefs

-- little test
-- mydict = {'carl':40,'alan':2, 'bob':1,'danny':3}
--thercontainer = [1, 3, 5]
main1 =  inferPyType (Program [Define "g$x" AVoid] (CExp (ContainerK PyListK [(SingleE (AInt 1)), (SingleE (AInt 3)), (SingleE (AInt 5))] (ELam (Lam {formals = ["rv0"], body = (SetThen "g$x" (Var "rv0") (App AHalt [AVoid]))})))))

main2 = inferPyType (Program [(Define "g$mydict" AVoid), (Define "g$othercontainer" AVoid)] (CExp (ContainerK DictK [(DictPair (AStr "carl", AInt 40)), (DictPair (AStr "alan", AInt 2)), (DictPair (AStr "bob", AInt 1)), (DictPair (AStr "danny", AInt 3))] (ELam (Lam {formals = ["rv0"], body = (ContainerK PyListK [(SingleE (AInt 1)), (SingleE (AInt 3)), (SingleE (AInt 5))] (ELam (Lam {formals = ["rv1"], body = (SetThen "g$othercontainer" (Var "rv1") (App AHalt [AVoid]))})))})))))

main3 = inferPyType (Program [Define "break" AVoid,Define "return" AVoid,Define "continue" AVoid,Define "$current-handler" AVoid,Define "g$a" AVoid,Define "g$b" AVoid,Define "g$y" AVoid,Define "g$o.x" AVoid] (CExp (SetThen "g$a" (Var "g$o") (SetThen "g$b" (Var "g$o") (SetThen "g$o.x" (AInt 1) (SetThen "g$y" (Var "g$a.x") (App AHalt [AVoid])))))))
