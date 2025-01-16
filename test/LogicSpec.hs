module LogicSpec where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import           Logic
import           Test.Hspec

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
                eval (And p q) model `shouldBe` False

            it "evaluates complex expressions" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (Not(And (Or p q) q)) model `shouldBe` True

            it "evaluates implication" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", True)]
                eval (Implies p q) model `shouldBe` True
                eval (Implies p (Not q)) model `shouldBe` False

            it "evaluates biconditional" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", True)]
                eval (Bicond p q) model `shouldBe` True
                eval (Bicond p (Not q)) model `shouldBe` False

            describe "logical equivalences" $ do
                it "validates De Morgan's Law for AND" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (Not (And p q)) model `shouldBe`
                        eval (Or (Not p) (Not q)) model

                it "validates De Morgan's Law for OR" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (Not (Or p q)) model `shouldBe`
                        eval (And (Not p) (Not q)) model

                it "validates double negation" $ do
                    let model = HM.fromList [(Symbol "p", True)]
                    eval (Not (Not p)) model `shouldBe` eval p model

                it "validates implication elimination" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (Implies p q) model `shouldBe`
                        eval (Or (Not p) q) model

                it "validates contrapositive" $ do
                    let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                    eval (Implies p q) model `shouldBe`
                        eval (Implies (Not q) (Not p)) model

        describe "getSymbols" $ do
            it "gets symbols from a complex proposition" $ do
                let prop = And p (Or q r)
                getSymbols prop `shouldBe` Set.fromList [Symbol "p", Symbol "q", Symbol "r"]

        describe "doesEntail" $ do
            it "validates simple entailment" $ do
                let kb = p
                    query = p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates modus ponens" $ do
                let kb = And (Implies p q) p
                    query = q
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates modus tollens" $ do
                let kb = And (Implies p q) (Not q)
                    query = Not p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "validates disjunctive syllogism" $ do
                let kb = And (Or p q) (Not p)
                    query = q
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` True

            it "rejects invalid entailment" $ do
                let kb = Or p q
                    query = p
                    symbols = getSymbols kb `Set.union` getSymbols query
                doesEntail kb query symbols HM.empty `shouldBe` False

