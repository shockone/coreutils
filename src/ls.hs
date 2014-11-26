module Main where

import System.Directory
import System.Environment

main :: IO ()
main = getArgs >>= getDirectoryContents . firstDirectory >>= mapM_ putStrLn

firstDirectory :: [String] -> String
firstDirectory [] = "."
firstDirectory xs = head xs
