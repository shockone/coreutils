module Main where

import System.Environment
import Control.Monad

main :: IO ()
main = getArgs >>= mapM_ (readFile >=> putStr)
