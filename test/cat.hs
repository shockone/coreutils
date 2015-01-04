module Main (main, specs) where

import Test.Hspec
import System.Process.ByteString
import Data.List
import qualified Data.ByteString as BS

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
  describe "options" $ providing params $ \paramsSet ->
    it ("Produces the same result as GNU cat for options " ++ (intercalate " " paramsSet) ++ ".") $ do
      let parameters = (paramsSet ++ [fixture "cat/example"])
          stdin = BS.empty
      (_, myCatResult, _)  <- (readProcessWithExitCode executablePath parameters stdin)
      (_, gnuCatResult, _) <- (readProcessWithExitCode "cat" parameters stdin)
      myCatResult `shouldBe` gnuCatResult


executablePath :: FilePath
executablePath = projectPath ++ "dist/build/cat/cat"

fixture :: String -> FilePath
fixture f = fixturesPath ++ f

fixturesPath :: FilePath
fixturesPath = projectPath ++ "dist/build/cat/"

projectPath :: FilePath
projectPath = "/Users/me/dev/haskell/coreutils/"

params :: [[String]]
--params =  subsequences ["-A", "-b", "-e", "-E", "-n", "-s", "-t", "-T", "-u", "-v"]
params = concatMap permutations $ subsequences ["-n", "-s", "-T", "-b", "-E", "-v"]


providing :: [d] -> (d -> Spec) -> Spec
providing providedData spec = sequence_ $ map spec providedData
