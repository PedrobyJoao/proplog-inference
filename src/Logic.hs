{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns  #-}
module Logic
    ( Proposition(..)
    , Symbol(..)
    , Model
    , eval
    , doesEntail
    , getSymbols
    ,applyInferenceRules
    ) where

import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

-- suppose a knowledge base `kb`:
-- kb: And(
--      p v q
-- )
--
-- enumerable models:
-- p   |   q
-- false | false
-- false | true
-- true  | false
-- true  | true
--
-- kb |= r
--
-- r must be true for all enumerable models where the knowledge base is also true.
--
-- for each model:
--  evaluate(kb, model) -- it should go through each kb statement, and check
--  if it is true:
--      evaluate(kb, query)


-- TODO: smart constructors for the types with validatiors if needed
newtype Symbol a = Symbol a
    deriving (Show, Eq, Generic, Ord)

instance Hashable a => Hashable (Symbol a)

data Proposition a
    = Atom (Symbol a)
    | And (Proposition a) (Proposition a)
    | Or (Proposition a) (Proposition a)
    | Not (Proposition a)
    | Implies (Proposition a) (Proposition a)
    | Bicond (Proposition a) (Proposition a)
    deriving (Show, Eq)

type Model a = HM.HashMap (Symbol a) Bool

-- | applyInferenceRules applies any of the enumerated
-- inference rules if possible
--
-- TODO: maybe add mode inference rules
--
-- TODO: maybe a more elegant way of pattern matching
-- on nthe more complex ones
applyInferenceRules :: Eq a => Proposition a -> Maybe (Proposition a)
applyInferenceRules (Atom _) = Nothing
applyInferenceRules (modusPonens -> Just result)           = Just result
applyInferenceRules (modusTollens -> Just result)          = Just result
applyInferenceRules (hypotheticalSyllogism -> Just result) = Just result
applyInferenceRules (addition -> Just result)              = Just result
applyInferenceRules (simplificationP -> Just result)        = Just result
applyInferenceRules (simplificationQ -> Just result)        = Just result
applyInferenceRules (disjunctiveSyllogism -> Just result) = Just result
applyInferenceRules (resolution -> Just result)            = Just result
applyInferenceRules (absorption -> Just result)            = Just result
applyInferenceRules (constructiveDilemma -> Just result)  = Just result
applyInferenceRules (destructiveDilemma -> Just result)   = Just result
applyInferenceRules p = case p of
    Not (Not x)    -> Just x -- double negation elimination
    Implies x y    -> Just (Or (Not x) y) -- implication elimination
    Bicond x y     -> Just (And (Implies x y) (Implies y x)) -- bicondition elimination
    Not (And x y)  -> Just (Or (Not x) (Not y)) -- De Morgan's law
    Not (Or x y)   -> Just (And (Not x) (Not y)) -- De Morgan's law
    And x (Or y z) -> Just (Or (And x y) (And x z)) -- distribution
    Or x (And y z) -> Just (And (Or x y) (Or x z)) -- distribution
    _              -> Nothing


-- | Evaluates if a proposition is true given a model
eval :: (Hashable a, Eq a) => Proposition a -> Model a -> Bool
eval (Atom s) model      = HM.lookup s model == Just True
eval (And p q) model     = eval p model && eval q model
eval (Or p q) model      = eval p model || eval q model
eval (Not p) model       = not $ eval p model
eval (Implies p q) model = not (eval p model) || eval q model
eval (Bicond p q) model  = eval p model == eval q model

-- | Checks if kb |= query
-- Time complexity: O(2^n) where n is the number of unique symbols
-- This is because we need to check all possible truth value combinations
-- by recursively trying True/False for each symbol
doesEntail :: (Hashable a, Eq a, Ord a) => Proposition a -> Proposition a -> Set.Set (Symbol a) -> Model a -> Bool
doesEntail kb query symbols model
    | null symbols = if eval kb model then eval query model else True
    | otherwise =
        let
            (randomSymbol, remaining) = Set.deleteFindMin symbols
            trueModel = HM.insert randomSymbol True model
            falseModel = HM.insert randomSymbol False model
        in
           doesEntail kb query remaining trueModel &&
           doesEntail kb query remaining falseModel

-- | Returns all symbols in a proposition
getSymbols :: Ord a => Proposition a -> Set.Set (Symbol a)
getSymbols prop = case prop of
    Atom s      -> Set.singleton s
    And p q     -> getSymbols p `Set.union` getSymbols q
    Or p q      -> getSymbols p `Set.union` getSymbols q
    Not p       -> getSymbols p
    Implies p q -> getSymbols p `Set.union` getSymbols q
    Bicond p q  -> getSymbols p `Set.union` getSymbols q

-- ***** Inference rules *****

-- | Modus Ponens: From (P → Q) ∧ P, infer Q
modusPonens :: Eq a => Proposition a -> Maybe (Proposition a)
modusPonens (And (Implies x y) z)
    | x == z    = Just y
modusPonens _ = Nothing

-- | Modus Tollens: From (P → Q) ∧ ¬Q, infer ¬P
modusTollens :: Eq a => Proposition a -> Maybe (Proposition a)
modusTollens (And (Implies x y) (Not z))
    | y == z    = Just (Not x)
modusTollens _ = Nothing

-- | Hypothetical Syllogism: From (P → Q) ∧ (Q → R), infer (P → R)
hypotheticalSyllogism :: Eq a => Proposition a -> Maybe (Proposition a)
hypotheticalSyllogism (And (Implies x y) (Implies z w))
    | y == z    = Just (Implies x w)
hypotheticalSyllogism _ = Nothing

-- | Addition (OR Introduction)
-- From P, infer P ∨ Q
addition :: Proposition a -> Maybe (Proposition a)
addition p = Just (Or p (Not p))  -- Using P ∨ ¬P which is always valid

-- | SimplificationP (AND Elimination)
-- From P ∧ Q, infer P
simplificationP :: Proposition a -> Maybe (Proposition a)
simplificationP (And p _) = Just p
simplificationP _         = Nothing

-- | SimplificationQ (AND Elimination)
-- From P ∧ Q, infer Q
simplificationQ :: Proposition a -> Maybe (Proposition a)
simplificationQ (And _ q) = Just q
simplificationQ _         = Nothing

-- | Disjunctive Syllogism
-- From P ∨ Q, ¬P, infer Q
disjunctiveSyllogism :: Eq a => Proposition a -> Maybe (Proposition a)
disjunctiveSyllogism (And (Or x y) (Not z))
    | x == z    = Just y
disjunctiveSyllogism _ = Nothing

-- | Resolution
-- From P ∨ Q, ¬P ∨ R, infer Q ∨ R
resolution :: Eq a => Proposition a -> Maybe (Proposition a)
resolution (And (Or x y) (Or (Not z) w))
    | x == z    = Just (Or y w)
resolution _ = Nothing

-- | Absorption
-- From P → Q, infer P → (P ∧ Q)
absorption :: Proposition a -> Maybe (Proposition a)
absorption (Implies p q) = Just (Implies p (And p q))
absorption _             = Nothing

-- | Constructive Dilemma
-- From (P → Q) ∧ (R → S), P ∨ R, infer Q ∨ S
constructiveDilemma :: Eq a => Proposition a -> Maybe (Proposition a)
constructiveDilemma (And (And (Implies p q) (Implies r s)) (Or x y))
    | p == x && r == y = Just (Or q s)
constructiveDilemma _ = Nothing

-- | Destructive Dilemma
-- From (P → Q) ∧ (R → S), ¬Q ∨ ¬S, infer ¬P ∨ ¬R
destructiveDilemma :: Eq a => Proposition a -> Maybe (Proposition a)
destructiveDilemma (And (And (Implies p q) (Implies r s)) (Or (Not x) (Not y)))
    | q == x && s == y = Just (Or (Not p) (Not r))
destructiveDilemma _ = Nothing
