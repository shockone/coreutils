{-# LANGUAGE UnicodeSyntax #-}
module Main where

--import Control.Monad
import Data.List           (delete, nub)
import qualified Data.ByteString as BS
import Options.Applicative

import Cat.Decorators as Decorators
import Cat.Parsers
import Cat.Types


main ∷ IO ()
main = do
    (filePaths, options) <- parseArguments
    concatenatedContent  <- concatenate filePaths
    let output = apply options concatenatedContent
        in BS.putStr output


parseArguments ∷ IO ([String], [Option])
parseArguments = merge <$> execParser argumentsParserWithInfo
    where merge (filePaths, options) = (filePaths, concat options)


argumentsParserWithInfo ∷ ParserInfo ([String], [Options])
argumentsParserWithInfo = info (helper <*> argumentsParser) description


description ∷ InfoMod ([String], [Options])
description = fullDesc <> progDesc "Print a greeting for TARGET"
                       <> header "hello - a test for optparse-applicative"


argumentsParser ∷ Parser ([String], [Options])
argumentsParser = (,) <$> filePathsParser <*> optionsParser


filePathsParser ∷ Parser [String]
filePathsParser = some (argument str (metavar "FILES"))


optionsParser ∷ Parser [Options]
optionsParser = many optionPa
    where optionPa = parser      'A'  "show-all"          "equivalent to -vET"                            [ShowNonprinting, ShowEnds, ShowTabs]
                 <|> parser      'b'  "number-nonblank"   "number nonempty output lines, overrides -n"    [NumberNonBlank]
                 <|> shortParser 'e'                      "equivalent to -vE"                             [ShowNonprinting, ShowEnds]
                 <|> parser      'E'  "show-ends"         "display $ at end of each line"                 [ShowEnds]
                 <|> parser      'n'  "number"            "number all output lines"                       [Number]
                 <|> parser      's'  "squeeze-blank"     "suppress repeated empty output lines"          [SqueezeBlank]
                 <|> shortParser 't'                      "equivalent to -vT"                             [ShowNonprinting, ShowTabs]
                 <|> parser      'T'  "show-tabs"         "display TAB characters as ^I"                  [ShowTabs]
                 <|> shortParser 'u'                      "(ignored)"                                     []
                 <|> parser      'v'  "show-nonprinting"  "use ^ and M- notation, except for LFD and TAB" [ShowNonprinting]


concatenate ∷ [String] → IO BS.ByteString
concatenate filePaths = do
    fileContent <- mapM BS.readFile filePaths
    return $ foldl1 BS.append fileContent


apply ∷ [Option] → BS.ByteString → BS.ByteString
apply opts content = foldl Decorators.decorate content (sanitize opts)


sanitize ∷ [Option] → [Option]
sanitize opts = foldl (\o f -> f o) opts functions
  where functions = [ nub,
                      \xs -> if ShowTabs `elem` xs then ShowTabs:delete ShowTabs xs else xs,
                      \xs -> if NumberNonBlank `elem` xs then delete Number xs else xs,
                      \xs -> if SqueezeBlank `elem` xs then SqueezeBlank:delete SqueezeBlank xs else xs,
                      \xs -> if ShowEnds `elem` xs then delete ShowEnds xs ++ [ShowEnds] else xs
                    ]
