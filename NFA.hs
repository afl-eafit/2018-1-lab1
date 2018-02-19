module DFA where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Delta s a = Map.Map s (Map.Map a (Set.Set s))

data DFA s a = DFA
  { start  :: Set.Set s
  , final  :: Set.Set s
  , states :: Set.Set s
  , sigma  :: Set.Set a
  , delta  :: Delta s a
  } deriving Show

initDFA :: Set.Set s -> DFA s a
initDFA q0 =
  DFA q0 Set.empty q0 Set.empty Map.empty

trans :: (Ord a, Ord s) => (s, a, Set.Set s) -> DFA s a -> DFA s a
trans (q1, a, q2) (DFA q0 f q s tf)
  | q2 == Set.empty = (DFA q0 f q s tf)
  | otherwise = DFA q0 f (Set.insert q1 (Set.union q2 q)) (Set.insert a s)
    (insert tf)
  where insert = Map.insertWith (Map.unionWith Set.union) q1
                 (Map.singleton a q2)

accept :: (Ord a, Ord s) => Set.Set s -> DFA s a -> DFA s a
accept q1 (DFA q0 f q s tf) =
  DFA q0 (Set.union q1 f) (Set.union q1 q) s tf
