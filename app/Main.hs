module Main where

import           Logic
import           Parser
import           System.Environment (getArgs)
import           System.Exit        (die)

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
            -- TODO: remove empty lines
            print "hello"

        _ -> die "Usage: logic-inference <filepath>"
