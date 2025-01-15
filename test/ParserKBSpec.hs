{-# LANGUAGE PatternSynonyms #-}
module ParserKBSpec where

import Test.Hspec
import Parser
import Logic
import qualified Data.HashMap.Strict as HM

-- Helper functions to make tests more readable
p, q, r :: Proposition String
p = Var (Symbol "p")
q = Var (Symbol "q")
r = Var (Symbol "r")

spec :: Spec
spec = do
  describe "extractKB" $ do
    it "extracts single proposition correctly" $ do
      let input = "knowledge(\n    p -> q\n)"
      extractKB input `shouldBe` Right (ImpliesT p q)

    it "extracts multiple propositions correctly" $ do
      let input = "knowledge(\n    p -> q\n    r -> p\n    r\n)"
      let expected = AndT (ImpliesT p q)
                        (AndT (ImpliesT r p) r)
      extractKB input `shouldBe` Right expected

    it "handles empty knowledge base" $ do
      let input = "knowledge(\n)"
      extractKB input `shouldBe` Left "Empty knowledge base"

    it "handles missing knowledge base" $ do
      let input = "p = \"It is raining\"\nq = \"The ground is wet\""
      extractKB input `shouldBe` Left "No knowledge base found"
