module V2 where

import qualified Data.Set as Set
import           Logic

-- TODO: instead of using Either, use custom types with more
-- information about the error and the process in general
--
-- TODO: see more about combinators for recursion: iterate/until/fixpoint

-- Search by inference rules
-- 1. apply inference rules for individual propositions
--  1.1. if new proposition, add to kb
--  1.2. check if q is contained within kb
--  (do this for all propositions within the kb
--  INCLUDED the new ones added by inference rules)
--
-- 2. apply inference rules for sylogisms
--  2.1. if new proposition, add to kb
--  2.2. check if q is contained within kb
--  (do this for all permutations of propositions within the kb,
--  INCLUDED the new ones added by inference rules)
searchByInference :: (Ord a, Eq a) => Set.Set (Proposition a) -> Proposition a -> Either String Bool
searchByInference kb query
    -- TODO-maybe: check just once for accumullatedKB
    | Set.null kb = Left "Empty knowledge base"
    | Set.member query kb = Right True
    | Set.member query immediateInferences = Right True
    | Set.member query syllogismsKB = Right True
    | otherwise = Right False
    where
        immediateInferences = searchImmediateRules kb
        syllogismsKB = searchSyllogismsRules $ Set.union kb immediateInferences


-- | searchImmediateRules applies all immediate inference rules, returning the updated
-- knowledge base if the query is found to be true.
--
-- It does NOT return the original kb together
searchImmediateRules :: (Ord a, Eq a) => Set.Set (Proposition a) -> Set.Set (Proposition a)
searchImmediateRules = foldMap applyInferenceRules

-- It does NOT return the original kb together
-- TODO: make functional more composable
searchSyllogismsRules :: (Ord a, Eq a) => Set.Set (Proposition a) -> Set.Set (Proposition a)
searchSyllogismsRules knowledge = searchSyllogismsRules' knowledge Set.empty
    -- TODO: halt?
    where
        searchSyllogismsRules' kb usedPairs
            | Set.null kb = Set.empty
            | otherwise = Set.union newKB (searchSyllogismsRules' newKB newUsedPairs)
            where
                pairs = [(x,y) | x <- Set.toList kb, y <- Set.toList kb, x /= y,
                                not $ Set.member (x,y) usedPairs]
                newUsedPairs = Set.union usedPairs (Set.fromList pairs)
                newKB = foldMap (\(x,y) -> applyInferenceRules (And x y)) pairs
