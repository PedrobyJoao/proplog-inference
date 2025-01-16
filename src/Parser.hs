module Parser where

import           Data.Char           (isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.List           (dropWhile, isPrefixOf, takeWhile)
import           Logic

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

-- | parse extracts vars, KB and query from a file content
parse :: String -> Either String (PropVarValue String, Proposition String, Proposition String)
parse contents = do
    let nonEmptyLines = filter (not . null . dropWhile isSpace) $ lines contents
        nonCommentLines = filter (not . isCommentLine) nonEmptyLines
        cleanContents = unlines nonCommentLines
    vars <- extractVars cleanContents
    kb <- extractKB cleanContents
    query <- extractQuery cleanContents
    Right (vars, kb, query)
  where
    isCommentLine line = "#" `isPrefixOf` dropWhile isSpace line

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
extractKB contents =
    let
        (_, kbSection) = break (isPrefixOf "knowledge[") $ lines contents
    in case kbSection of
        [] -> Left "No knowledge base found"
        (_:rest) ->
            let kbLines = takeWhile (/= "]") rest
                cleanedLines = filter (not . null . dropWhile isSpace) kbLines
                propositions = map (parseProposition . dropWhile isSpace) cleanedLines
            in case propositions of
                []     -> Left $ "Empty knowledge base: " ++ show rest
                (p:ps) -> Right $ foldr And p ps

-- | extractQuery extracts the query from string given defined grammar
extractQuery :: String -> Either String (Proposition String)
extractQuery contents =
    let (_, querySection) = break (isPrefixOf "query[") $ lines contents
    in case querySection of
        [] -> Left "No query found"
        (_:rest) ->
            let queryLines = takeWhile (/= "]") rest
                cleanedLines = filter (not . null . dropWhile isSpace) queryLines
            in case cleanedLines of
                []    -> Left "Empty query"
                (x:_) -> Right $ parseProposition $ dropWhile isSpace x

-- | Parse a single proposition line into a Proposition
parseProposition :: String -> Proposition String
parseProposition = parseExpr . tokenize . filter (not . (`elem` " \t"))
  where
    tokenize :: String -> [String]
    tokenize [] = []
    tokenize ('(':xs) = "(" : tokenize xs
    tokenize (')':xs) = ")" : tokenize xs
    tokenize ('~':xs) = "~" : tokenize xs
    tokenize xs =
        let (token, rest) = span (`notElem` "()~^v<->") xs
        in if null token
           then case rest of
                ('-':'>':more)     -> "->" : tokenize more
                ('<':'-':'>':more) -> "<->" : tokenize more
                (x:more)           -> [x] : tokenize more
                []                 -> []
           else token : tokenize rest

parseExpr :: [String] -> Proposition String
parseExpr [] = error "Empty expression"
parseExpr tokens = 
    let (expr, rest) = parseExpr' tokens
        rest' = dropWhile (== ")") rest
    in if null rest'
       then expr
       else error $ "Unexpected tokens after parsing: " ++ show rest'

-- Operator precedence (highest to lowest)
-- 1. ~ (not)
-- 2. ^ (and)
-- 3. v (or)
-- 4. -> (implies)
-- 5. <-> (biconditional)

parseExpr' :: [String] -> (Proposition String, [String])
parseExpr' tokens = parseBiconditional tokens

parseBiconditional :: [String] -> (Proposition String, [String])
parseBiconditional tokens =
    let (left, rest) = parseImplication tokens
    in parseBiconditional' left rest

parseBiconditional' :: Proposition String -> [String] -> (Proposition String, [String])
parseBiconditional' left ("<->":tokens) =
    let (right, rest) = parseImplication tokens
    in parseBiconditional' (Bicond left right) rest
parseBiconditional' left rest = (left, rest)

parseImplication :: [String] -> (Proposition String, [String])
parseImplication tokens =
    let (left, rest) = parseDisjunction tokens
    in parseImplication' left rest

parseImplication' :: Proposition String -> [String] -> (Proposition String, [String])
parseImplication' left ("->":tokens) =
    let (right, rest) = parseDisjunction tokens
    in parseImplication' (Implies left right) rest
parseImplication' left rest = (left, rest)

parseDisjunction :: [String] -> (Proposition String, [String])
parseDisjunction tokens =
    let (left, rest) = parseConjunction tokens
    in parseDisjunction' left rest

parseDisjunction' :: Proposition String -> [String] -> (Proposition String, [String])
parseDisjunction' left ("v":tokens) =
    let (right, rest) = parseConjunction tokens
    in parseDisjunction' (Or left right) rest
parseDisjunction' left rest = (left, rest)

parseConjunction :: [String] -> (Proposition String, [String])
parseConjunction tokens =
    let (left, rest) = parseNegation tokens
    in parseConjunction' left rest

parseConjunction' :: Proposition String -> [String] -> (Proposition String, [String])
parseConjunction' left ("^":tokens) =
    let (right, rest) = parseNegation tokens
    in parseConjunction' (And left right) rest
parseConjunction' left rest = (left, rest)

parseNegation :: [String] -> (Proposition String, [String])
parseNegation [] = error "Empty expression"
parseNegation ("~":tokens) = 
    let (expr, rest) = parseNegation tokens
    in (Not expr, rest)
parseNegation ("(":tokens) =
    let (expr, rest) = parseExpr' tokens
    in case rest of
        (")":remaining) -> (expr, remaining)
        [] -> error "Unexpected end of input - missing closing parenthesis"
        _ -> error $ "Unexpected tokens after closing parenthesis: " ++ show rest
parseNegation (")":_) = error "Unexpected closing parenthesis"
parseNegation (t:tokens)
    | t `elem` ["->", "<->", "^", "v"] = error $ "Unexpected operator: " ++ t
    | otherwise = (Atom (Symbol t), tokens)
