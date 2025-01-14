{-# LANGUAGE PatternSynonyms #-}
module LogicSpec where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import           Logic
import           Test.Hspec

-- Smart constructors for common patterns
pattern AndT :: Proposition -> Proposition -> Proposition
pattern AndT a b <- Logic.And a b
  where AndT a b = Logic.And a b

pattern OrT :: Proposition -> Proposition -> Proposition
pattern OrT a b <- Logic.Or a b
  where OrT a b = Logic.Or a b

pattern ImpliesT :: Proposition -> Proposition -> Proposition
pattern ImpliesT a b <- Logic.Implies a b
  where ImpliesT a b = Logic.Implies a b

pattern BicondT :: Proposition -> Proposition -> Proposition
pattern BicondT a b <- Logic.Bicond a b
  where BicondT a b = Logic.Bicond a b

pattern NotT :: Proposition -> Proposition
pattern NotT a <- Logic.Not a
  where NotT a = Logic.Not a

-- Smart constructor for atoms
p, q, r :: Proposition
p = Atom (Symbol "p")
q = Atom (Symbol "q")
r = Atom (Symbol "r")

spec :: Spec
spec = do
    describe "Logic" $ do
        describe "eval" $ do
            it "evaluates simple atom" $ do
                let model = HM.fromList [(Symbol "p", True)]
                eval p model `shouldBe` True

            it "evaluates AND operation" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (AndT p q) model `shouldBe` False

            it "evaluates complex expressions" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (AndT (OrT p q) q) model `shouldBe` False

        describe "getSymbols" $ do
            it "gets symbols from a complex proposition" $ do
                let prop = AndT p (OrT q r)
                getSymbols prop `shouldBe` Set.fromList [Symbol "p", Symbol "q", Symbol "r"]

