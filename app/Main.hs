module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import           Logic
import           Parser
import           System.Environment  (getArgs)
import           System.Exit         (die)

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
-- query(q)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> do
            contents <- readFile filepath
            case parse contents of
                Left err -> die $ "Parse error: " ++ err
                Right (vars, kb, query) -> do
                    putStrLn "Variables:"
                    mapM_ (\(Symbol k, v) -> putStrLn $ k ++ " = \"" ++ v ++ "\"") $ HM.toList vars
                    putStrLn "\nKnowledge Base:"
                    print kb
                    putStrLn "\nQuery:"
                    print query

                    let allSymbols = getSymbols query `Set.union` getSymbols kb
                    let result = doesEntail kb query allSymbols HM.empty

                    putStrLn $ "\nDoes KB entail query? " ++ show result

        _ -> die "Usage: logic-inference <filepath>"
