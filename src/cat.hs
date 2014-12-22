module Main where

import Control.Monad
import Options.Applicative
import Data.List(nub, delete)

import Cat.Types
import Cat.Flags as Flags
import Cat.Decorators as Decorators


main :: IO ()
main = parseArguments >>= processArguments


parseArguments :: IO Arguments
parseArguments = execParser argumentsParserWithInfo


argumentsParserWithInfo :: ParserInfo Arguments
argumentsParserWithInfo = info (helper <*> argumentsParser) description


description :: InfoMod Arguments
description = fullDesc <> progDesc "Print a greeting for TARGET"
                       <> header "hello - a test for optparse-applicative"


argumentsParser :: Parser Arguments
argumentsParser = Arguments
  <$> some (argument str (metavar "FILES..."))
  <*> many (Flags.number <|> Flags.showAll <|> Flags.numberNonBlank <|> Flags.showEnds)


processArguments :: Arguments -> IO ()
processArguments (Arguments filenames options) = concatenatedContent filenames >>= putStr . unlines . parse options


concatenatedContent :: [FilePath] -> IO FileContent
concatenatedContent = mapM readFile >=> return . lines . join


parse :: [Option] -> FileContent -> FileContent
parse opts content = foldl (flip Decorators.decorate) content (sanitize opts)


sanitize :: [Option] -> [Option]
sanitize opts = foldl (\o f -> f o) opts functions
  where
    functions = [
                  nub,
                  \xs -> if elem NumberNonBlank xs then delete Number xs else xs
                ]
