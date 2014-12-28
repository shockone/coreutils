{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Control.Monad
import           Data.List           (delete, nub)
import           Options.Applicative

import           Cat.Decorators      as Decorators
import           Cat.Parsers         as Parsers
import           Cat.Types


main ∷ IO ()
main = parseArguments >>= processArguments


parseArguments ∷ IO Arguments
parseArguments = execParser argumentsParserWithInfo


argumentsParserWithInfo ∷ ParserInfo Arguments
argumentsParserWithInfo = info (helper <*> argumentsParser) description


description ∷ InfoMod Arguments
description = fullDesc <> progDesc "Print a greeting for TARGET"
                       <> header "hello - a test for optparse-applicative"


argumentsParser ∷ Parser Arguments
argumentsParser = Arguments
  <$> some (argument str (metavar "FILES..."))
  <*> many (Parsers.showAll
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


processArguments ∷ Arguments → IO ()
processArguments (Arguments filenames options) = concatenatedContent filenames >>= putStr . unlines . parse options


concatenatedContent ∷ [FilePath] → IO FileContent
concatenatedContent = mapM readFile >=> return . lines . join


parse ∷ [Option] → FileContent → FileContent
parse opts content = foldl Decorators.decorate content (sanitize opts)


sanitize ∷ [Option] → [Option]
sanitize opts = foldl (\o f -> f o) opts functions
  where
    functions = [
                  nub,
                  \xs -> if elem ShowTabs xs then ShowTabs:(delete ShowTabs xs) else xs,
                  \xs -> if elem NumberNonBlank xs then delete Number xs else xs,
                  \xs -> if elem SqueezeBlank xs then SqueezeBlank:(delete SqueezeBlank xs) else xs,
                  \xs -> if elem ShowEnds xs then (delete ShowEnds xs) ++ [ShowEnds] else xs
                ]
