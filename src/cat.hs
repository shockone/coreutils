module Main where

import Control.Monad
import Options.Applicative

type Filename = String
data LineNumbering = WithNumbers | WithoutNumbers
data Arguments = Arguments [Filename] LineNumbering

sample :: Parser Arguments
sample = Arguments
  <$> some (argument str (metavar "FILES..."))
  <*> flag WithoutNumbers WithNumbers
      ( long "number" <> short 'n' <> help "number all output lines" )

greet :: Arguments -> IO ()
greet (Arguments filenames WithoutNumbers) = mapM_ (readFile >=> putStr) filenames
greet (Arguments _ WithNumbers) = putStrLn "With numbers!"

main :: IO ()
main = execParser opts >>= greet
  where opts = info (helper <*> sample)
                    ( fullDesc
                      <> progDesc "Print a greeting for TARGET"
                      <> header "hello - a test for optparse-applicative"
                    )
