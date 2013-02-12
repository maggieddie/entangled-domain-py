------------------------------------------------------------------------
-- * New Object Map for the Curried Object Model 
-- * The complexity of insertions and lookups are k*logn, 
--     where k is the size of set to insert or lookup, 
--           logn is the complexity to find a place to insert or lookup
-- * There are two version of lookup, one uses Monad, 
--                                    the other does not.
-------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S


-- ordinary map
type k :-> v = M.Map k v

--  Powerset 
type P v = Set v

-- our old curried obj map, the key is set of any storable values
type OldMap k v = (P k) :->  (P v)

-- the second part of our new ObjMap
-- with element mapping to a set of sets that contains the element
type ElemSetMap k =  k :-> (P (P k))

-- new Object Map
type ObjMap k v = (OldMap k v, ElemSetMap k)

-- the insertions algorithm (set field)
insertObjMap :: (Ord k) =>  P k -> P v -> ObjMap k v -> ObjMap k v
insertObjMap keySet valSet (oldMap, elemSetMap)  = (extendedOldMap, elemSetMap') where
  extendedOldMap = M.insert keySet valSet oldMap
  elemSetMap'    = M.unionWith S.union elemSetMap 
                   $ M.fromList [(k, S.singleton keySet) | k <- S.toList keySet]
    
                   
-- the non-monad lookup algorithm (get field)
lookupObjMap :: (Ord k, Ord v) =>  P k -> ObjMap k v -> P v
lookupObjMap keySet (oldMap, elemSetMap) = resSet  where
  resSet =    flattenLookup oldMap $ flattenLookup elemSetMap keySet
      
-- helper function for the non-monad lookup
flattenLookup :: (Ord k, Ord v) =>  (k :-> P v) -> P k -> P v
flattenLookup  m keySet = resSet where
  resSet = foldr (\ elem flatResSet -> S.union flatResSet 
                                            (maybe S.empty (\ id -> id) (M.lookup elem m)))
              S.empty (S.toList keySet) 
  

-- the lookup algorithm (get field)  using Monad  
lookupObjMapM :: (Ord k, Ord v) =>  P k -> ObjMap k v -> P v
lookupObjMapM keySet (oldMap, elemSetMap) =
  S.fromList $ S.toList keySet >>= flattenLookupM elemSetMap >>= flattenLookupM oldMap
  
  
-- helper for lookupObjMapM 
flattenLookupM :: Ord k =>  (k :-> (P v)) -> k -> [v]
flattenLookupM map key = maybe [] S.toList $ M.lookup key map 

-- some test cases
main :: IO()
main = do
  let m1 =  insertObjMap (S.fromList ["foo", "bar"])  (S.fromList [3])  (M.empty, M.empty)
     -- m2 =  insertObjMap (S.fromList ["foo", "bar", "baz"]) (S.fromList [5]) m1   
      m3 =  insertObjMap (S.fromList ["foo", "baz"]) (S.fromList [5]) m1
      
  print m1    
  print m3
  print $  lookupObjMapM (S.fromList ["foo", "bas"]) m3  -- or use lookupObjM
  print $  lookupObjMapM (S.fromList ["asfsdfds"]) m3

  
