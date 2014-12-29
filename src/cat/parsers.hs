module Cat.Parsers where
-- TODO: don't export everything.

import Options.Applicative
import Cat.Types


parser :: Char -> String -> String -> [Option] -> Parser [Option]
parser s l h o= flag' o $ short s <> help h <> long l


shortParser :: Char -> String -> [Option] -> Parser [Option]
shortParser s h o = flag' o $ short s <> help h
