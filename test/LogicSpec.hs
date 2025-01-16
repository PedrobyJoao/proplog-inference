{-# LANGUAGE PatternSynonyms #-}
module LogicSpec where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import           Logic
import           Test.Hspec

pattern AndT :: Proposition a -> Proposition a -> Proposition a
pattern AndT a b <- Logic.And a b
  where AndT a b = Logic.And a b

pattern OrT :: Proposition a -> Proposition a -> Proposition a
pattern OrT a b <- Logic.Or a b
  where OrT a b = Logic.Or a b

pattern ImpliesT :: Proposition a -> Proposition a -> Proposition a
pattern ImpliesT a b <- Logic.Implies a b
  where ImpliesT a b = Logic.Implies a b

pattern BicondT :: Proposition a -> Proposition a -> Proposition a
pattern BicondT a b <- Logic.Bicond a b
  where BicondT a b = Logic.Bicond a b

pattern NotT :: Proposition a -> Proposition a
pattern NotT a <- Logic.Not a
  where NotT a = Logic.Not a

-- Smart constructor for atoms now needs type annotation
p, q, r :: Proposition String
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

            it "evaluates atom not in model" $ do
                let model = HM.fromList []
                eval p model `shouldBe` False

            it "evaluates AND operation" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (AndT p q) model `shouldBe` False

            it "evaluates complex expressions" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (NotT(AndT (OrT p q) q)) model `shouldBe` True

            it "evaluates implication" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", True)]
                eval (ImpliesT p q) model `shouldBe` True
                eval (ImpliesT p (NotT q)) model `shouldBe` False

            it "evaluates biconditional" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", True)]
                eval (BicondT p q) model `shouldBe` True
                eval (BicondT p (NotT q)) model `shouldBe` False

            describe "logical equivalences" $ do
                it "validates De Morgan's Law for AND" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (NotT (AndT p q)) model `shouldBe`
                        eval (OrT (NotT p) (NotT q)) model

                it "validates De Morgan's Law for OR" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (NotT (OrT p q)) model `shouldBe`
                        eval (AndT (NotT p) (NotT q)) model

                it "validates double negation" $ do
                    let model = HM.fromList [(Symbol "p", True)]
                    eval (NotT (NotT p)) model `shouldBe` eval p model

                it "validates implication elimination" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (ImpliesT p q) model `shouldBe`
                        eval (OrT (NotT p) q) model

                it "validates contrapositive" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (ImpliesT p q) model `shouldBe`
                        eval (ImpliesT (NotT q) (NotT p)) model

        describe "getSymbols" $ do
            it "gets symbols from a complex proposition" $ do
                let prop = AndT p (OrT q r)
                getSymbols prop `shouldBe` Set.fromList [Symbol "p", Symbol "q", Symbol "r"]

        describe "doesEntail" $ do
            it "validates simple entailment" $ do
                let kb = p
                    query = p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates modus ponens" $ do
                let kb = AndT (ImpliesT p q) p
                    query = q
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates modus tollens" $ do
                let kb = AndT (ImpliesT p q) (NotT q)
                    query = NotT p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates disjunctive syllogism" $ do
                let kb = AndT (OrT p q) (NotT p)
                    query = q
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "rejects invalid entailment" $ do
                let kb = OrT p q
                    query = p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` False

