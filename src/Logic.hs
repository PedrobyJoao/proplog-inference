{-# LANGUAGE DeriveGeneric #-}
module Logic
    ( Proposition(..)
    , Symbol(..)
    , Model
    , eval
    , doesEntail
    , getSymbols
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

-- TODO: smart constructors for the types with validatiors

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
