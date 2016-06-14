-- Nancy Cao
-- Assignment 4
-- Part B: Columns.hs

module Main where

import Prelude
import Data.Char
import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    -- Check there is more than 1 argument
    case args of
        (_:[]) -> do
            fail "usage: columns n1 n2 ... filename"
        args -> do
            -- Check arguments before filename are digits
            case all (== True) (map isDigit (map head (init args))) of
                True -> do
                    -- Check if numbers are positive
                    let nums = map read (init args) in
                        case all (> 0) nums of
                            True -> do
                                -- Check if reading from file or console   
                                case (last args) of
                                    -- From console
                                    ['-'] -> do
                                        file <- hGetContents stdin
                                        let wordsList = map words (lines file) in
                                            mapM_ putStrLn (map unwords (map (getWords nums) wordsList))
                                    -- From file
                                    filename -> do
                                        file <- readFile filename
                                        let wordsList = map words (lines file) in
                                            mapM_ putStrLn (map unwords (map (getWords nums) wordsList))
                            _ -> do
                                fail "usage: columns n1 n2 ... filename"
                _ -> do
                    fail "usage: columns n1 n2 ... filename"

-- Gets the words of a line at the specified columns, if any
-- list of column numbers -> list of words in a line -> list of words to return
getWords :: [Int] -> [String] -> [String]
getWords [] _ = []
getWords _ [] = []
getWords x y = mapMaybe (getOneWord y) x

-- Gets the word at the index, if any
-- list of words in a line -> the column number -> possible word returned
getOneWord :: [String] -> Int -> Maybe String
getOneWord [] _ = Nothing
getOneWord (x:_) 1 = Just x
getOneWord (_:xs) n = getOneWord xs (n - 1)