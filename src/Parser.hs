module Parser where

import           Data.Char           (isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.List           (dropWhile, isPrefixOf, takeWhile)
import           Logic
import           System.Environment  (getArgs)
import           System.Exit         (die)

-- see examples/ for file examples to be parsed
-- input file format:
--
-- p = "some statement"
-- q = "some other statement"
-- ...
--
-- knowledge(
--    p v q
--    p ^ q
--    ~p
--    p -> q
--    p <-> q
-- )
--
-- query(
--    q
-- )

type PropVarValue a = HM.HashMap (Symbol a) String

-- | extractVars extracts the propositional variables and their values
extractVars :: String -> Either String (PropVarValue String)
extractVars contents = Right $ HM.fromList $ map parseLine $ filter isVarLine $ lines contents
  where
    isVarLine line = not (null line) && ('=' `elem` line)

    parseLine :: String -> (Symbol String, String)
    parseLine line =
      let cleaned = dropWhile isSpace line
          (var, rest) = break (== '=') cleaned
          varName = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace var
          value = takeWhile (/= '"') $ dropWhile (\c -> isSpace c || c == '=' || c == '"') rest
      in (Symbol varName, value)

-- | extractKB extracts the knowledge base from string given defined grammar
extractKB :: String -> Either String (Proposition String)
extractKB contents = undefined


-- | extractQuery extracts the query from string given defined grammar
extractQuery :: String -> Either String (Proposition String)
extractQuery contents = undefined

-- | Parse a single proposition line into a Proposition
parseProposition :: String -> Proposition String
parseProposition = undefined
