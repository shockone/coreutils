module Main where

import System.Directory
import System.Environment

main :: IO ()
main = do
  args  <- getArgs
  files <- getDirectoryContents (head args)
  print files
