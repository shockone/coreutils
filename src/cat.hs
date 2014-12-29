{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Monad
import Data.List           (delete, nub)
import Options.Applicative

import Cat.Decorators as Decorators
import Cat.Parsers    as Parsers
import Cat.Types


main ∷ IO ()
main = do
    (filePaths, options) <- parseArguments
    concatenatedContent  <- concatenate filePaths
    let output = apply options concatenatedContent
        in putStr (unlines output)


parseArguments ∷ IO ([String], [Option])
parseArguments = execParser argumentsParserWithInfo


argumentsParserWithInfo ∷ ParserInfo ([String], [Option])
argumentsParserWithInfo = info (helper <*> argumentsParser) description


description ∷ InfoMod ([String], [Option])
description = fullDesc <> progDesc "Print a greeting for TARGET"
                       <> header "hello - a test for optparse-applicative"


argumentsParser ∷ Parser ([String], [Option])
argumentsParser = (,) <$> filePaths <*> options
  where filePaths = some (argument str (metavar "FILES"))
        options   = many (Parsers.showAll
                      <|> Parsers.numberNonBlank
                      <|> Parsers.showNonprintingAndEnds
                      <|> Parsers.showEnds
                      <|> Parsers.number
                      <|> Parsers.squeezeBlank
                      <|> Parsers.showNonprintingAndTabs
                      <|> Parsers.showTabs
                      <|> Parsers.showNonprinting
                      <|> Parsers.u
                         )


concatenate ∷ [String] → IO [String]
concatenate = mapM readFile >=> return . lines . join


apply ∷ [Option] → [String] → [String]
apply opts content = foldl Decorators.decorate content (sanitize opts)


sanitize ∷ [Option] → [Option]
sanitize opts = foldl (\o f -> f o) opts functions
  where
    functions = [
                  nub,
                  \xs -> if ShowTabs `elem` xs then ShowTabs:delete ShowTabs xs else xs,
                  \xs -> if NumberNonBlank `elem` xs then delete Number xs else xs,
                  \xs -> if SqueezeBlank `elem` xs then SqueezeBlank:delete SqueezeBlank xs else xs,
                  \xs -> if ShowEnds `elem` xs then delete ShowEnds xs ++ [ShowEnds] else xs
                ]
