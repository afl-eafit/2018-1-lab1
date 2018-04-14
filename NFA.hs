{-|
Module      : NFA
Description : Defines NFAs with multiple initial states

To be used in CM0081 2018-1 lab 1. Definas NFAs and functions to create them
-}
module NFA where

import qualified Data.Map as Map (Map, empty, singleton, insertWith, unionWith)
import qualified Data.Set as Set (Set, empty, insert, union)

-- | Represents the transition funtion in a NFA
type Delta s a = Map.Map s (Map.Map a (Set.Set s))

-- | Represents a NFA as a 5-tuplei
data NFA s a = NFA
  { start  :: Set.Set s -- ^ Set of initial states
  , final  :: Set.Set s -- ^ Set of accepting states
  , states :: Set.Set s -- ^ Set of states
  , sigma  :: Set.Set a -- ^ Alphabet
  , delta  :: Delta s a -- ^ Transition function
  } deriving Show

-- | Creates an NFA with a set of initial states
initNFA :: Set.Set s -- ^Initial states of the automata
        -> NFA s a   -- ^Automata with only initial states
initNFA q0 = NFA q0 Set.empty q0 Set.empty Map.empty

-- | Adds transitions with of a state and symbol to a NFA
trans :: (Ord a, Ord s)
      => (s, a, Set.Set s)
      -- ^ Has the state where the transition starts, the symbol of the
      -- transition and the set of states in which the transition ends
      -> NFA s a
      -- ^ The automata to be modified
      -> NFA s a
      -- ^ The automata with new transitions

trans (q1, a, q2) nfa@(NFA q0 f q s tf)
  | q2 == Set.empty = nfa
  | otherwise       = NFA q0 f (Set.insert q1 (Set.union q2 q)) (Set.insert a s)
                        (insert q1 a q2 tf)
  where
    -- | Inserts (q1, a, q2) into the delta function of a NFA
    insert :: (Ord a, Ord s) => s -> a -> Set.Set s -> Delta s a -> Delta s a
    insert q1' a' q2' = Map.insertWith (Map.unionWith Set.union)
                     q1' (Map.singleton a' q2')

-- | Adds a set of acceptance states to a NFA
accept :: (Ord a, Ord s)
       => Set.Set s
       -- ^ Set of acceptance states to add
       -> NFA s a
       -- ^ NFA to be modified
       -> NFA s a
       -- ^ NFA with new acceptance states

accept q1 (NFA q0 f q s tf) = NFA q0 (Set.union q1 f) (Set.union q1 q) s tf
