module LogicSpec where

import Test.Hspec
import Logic
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Logic" $ do
        describe "eval" $ do
            it "evaluates simple atom" $ do
                let model = HM.fromList [(Symbol "p", True)]
                eval (Atom (Symbol "p")) model `shouldBe` True

            it "evaluates AND operation" $ do
                let model = HM.fromList [(Symbol "p", True), (Symbol "q", False)]
                eval (And (Atom (Symbol "p")) (Atom (Symbol "q"))) model `shouldBe` False

        describe "getSymbols" $ do
            it "gets symbols from a simple proposition" $ do
                getSymbols (Atom (Symbol "p")) `shouldBe` Set.singleton (Symbol "p")

            it "gets symbols from a complex proposition" $ do
                let prop = And (Atom (Symbol "p")) (Or (Atom (Symbol "q")) (Atom (Symbol "r")))
                getSymbols prop `shouldBe` Set.fromList [Symbol "p", Symbol "q", Symbol "r"]
