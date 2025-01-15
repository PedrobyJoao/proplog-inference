module ParserSpec where

import Test.Hspec
import Parser
import Logic
import qualified Data.HashMap.Strict as HM

spec :: Spec
spec = do
  describe "extractVars" $ do
    it "extracts variables and their values correctly" $ do
      let input = "p = \"It is raining\"\nq = \"The ground is wet\""
      let expected = HM.fromList [(Symbol "p", "It is raining"), (Symbol "q", "The ground is wet")]
      extractVars input `shouldBe` Right expected

    it "handles empty lines between variable definitions" $ do
      let input = "p = \"It is raining\"\n\nq = \"The ground is wet\""
      let expected = HM.fromList [(Symbol "p", "It is raining"), (Symbol "q", "The ground is wet")]
      extractVars input `shouldBe` Right expected

    it "handles leading whitespace" $ do
      let input = "  p = \"It is raining\"\n  q = \"The ground is wet\""
      let expected = HM.fromList [(Symbol "p", "It is raining"), (Symbol "q", "The ground is wet")]
      extractVars input `shouldBe` Right expected

    it "handles empty input" $ do
      extractVars "" `shouldBe` Right HM.empty
      
    it "handles variable names with multiple characters" $ do
      let input = "rain = \"It is raining\"\nwet = \"The ground is wet\""
      let expected = HM.fromList [(Symbol "rain", "It is raining"), (Symbol "wet", "The ground is wet")]
      extractVars input `shouldBe` Right expected
