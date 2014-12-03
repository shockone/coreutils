module Main where

import Control.Monad
import Options.Applicative
import Data.List(nub)

type Filename    = String
type FileContent = [String]

data Option = Number | All | NonBlank deriving (Eq)
data Arguments = Arguments [Filename] [Option]

main :: IO ()
main = execParser opts >>= greet
  where opts = info (helper <*> argumentsParser)
                    ( fullDesc
                      <> progDesc "Print a greeting for TARGET"
                      <> header "hello - a test for optparse-applicative"
                    )


number :: Parser Option
number = flag' Number $ long "number" <> short 'n' <> help "number all output lines"

showAll :: Parser Option
showAll = flag' All $ long "show-all" <> short 'A' <> help "equivalent to -vET"

argumentsParser :: Parser Arguments
argumentsParser = Arguments
  <$> some (argument str (metavar "FILES..."))
  <*> (many $ number <|> showAll)

greet :: Arguments -> IO ()
greet (Arguments filenames options) = mapM_ (readFile >=> putStrLn . unlines . (parse (nub options)) . lines) filenames

parse :: [Option] -> FileContent -> FileContent
parse [] content = content
parse (opt:rest) content = parse rest (apply opt content)

apply :: Option -> FileContent -> FileContent
apply Number content = addNumbers content
apply _ content = content


addNumbers :: FileContent -> FileContent
addNumbers content = zipWith cnc [1..] content
  where numberMaxWidth = ceiling . log . fromIntegral . length $ content
        cnc num line = pad (show num) ++ line
        pad num = replicate (numberMaxWidth - length num)' ' ++ num ++ " "
