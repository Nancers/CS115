-- Nancy Cao
-- Assignment 4
-- Part B: Reverse.hs

module Main where

import Prelude
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    -- Check there is only 1 argument
    case args of
        (x:[]) -> do
            file <- readFile x
            mapM_ putStrLn (reverse (lines file))
            exitSuccess
        _ -> do
            fail "usage: reverse filename"
            exitFailure