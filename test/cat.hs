module Main (main, specs) where

import Test.Hspec
import System.Process
import Data.List

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
  describe "options" $ providing params $ \paramsSet ->
    it ("gives the same result as the gnu cat for " ++ (intercalate " " paramsSet)) $ do
      myCatResult  <- (readProcess executablePath (paramsSet ++ [fixture "cat/example"]) [])
      gnuCatResult <- (readProcess "cat" (paramsSet ++ [fixture "cat/example"]) [])
      myCatResult `shouldBe` gnuCatResult


executablePath :: FilePath
executablePath = projectPath ++ "dist/build/cat/cat"

fixture :: String -> FilePath
fixture f = fixturesPath ++ f

fixturesPath :: FilePath
fixturesPath = projectPath ++ "test/fixtures/"

projectPath :: FilePath
projectPath = "/Users/me/dev/haskell/coreutils/"

params :: [[String]]
--params = concatMap permutations $ subsequences ["-A", "-b", "-e", "-E", "-n", "-s", "-t", "-T", "-u", "-v"]
params = concatMap permutations $ subsequences ["-n", "-s", "-T", "-b", "-E"]


providing :: [d] -> (d -> Spec) -> Spec
providing providedData spec = sequence_ $ map spec providedData
