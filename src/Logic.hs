{-# LANGUAGE DeriveGeneric #-}
module Logic where

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

-- TODO: smart constructors for the types

newtype Symbol = Symbol String
    deriving (Show, Eq, Generic, Ord)

instance Hashable Symbol

data Proposition
    = Atom Symbol
    | And Proposition Proposition
    | Or Proposition Proposition
    | Not Proposition
    | Implies Proposition Proposition
    | Bicond Proposition Proposition
    deriving (Show, Eq)

type Model = HM.HashMap Symbol Bool

-- | Evaluates if a proposition is true given a model
eval :: Proposition -> Model -> Bool
eval (Atom s) model      = HM.lookup s model == Just True
eval (And p q) model     = eval p model && eval q model
eval (Or p q) model      = eval p model || eval q model
eval (Not p) model       = not $ eval p model
eval (Implies p q) model = not (eval p model) || eval q model
eval (Bicond p q) model  = eval p model == eval q model

-- | Checks if kb |= query
doesEntail :: Proposition -> Proposition -> Set.Set Symbol -> Model -> Bool
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
getSymbols :: Proposition -> Set.Set Symbol
getSymbols prop = case prop of
    Atom s      -> Set.singleton s
    And p q     -> getSymbols p `Set.union` getSymbols q
    Or p q      -> getSymbols p `Set.union` getSymbols q
    Not p       -> getSymbols p
    Implies p q -> getSymbols p `Set.union` getSymbols q
    Bicond p q  -> getSymbols p `Set.union` getSymbols q
