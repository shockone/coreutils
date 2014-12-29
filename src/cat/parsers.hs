module Cat.Parsers where
-- TODO: don't export everything.

import Options.Applicative

import Cat.Types


showAll :: Parser [Option]
showAll = parser 'A' "show-all"  "equivalent to -vET" [ShowNonprinting, ShowEnds, ShowTabs]


numberNonBlank :: Parser [Option]
numberNonBlank = parser 'b' "number-nonblank" "number nonempty output lines, overrides -n" [NumberNonBlank]


showNonprintingAndEnds :: Parser [Option]
showNonprintingAndEnds = shortParser 'e' "equivalent to -vE" [ShowNonprinting, ShowEnds]


showEnds :: Parser [Option]
showEnds = parser 'E' "show-ends" "display $ at end of each line" [ShowEnds]


number :: Parser [Option]
number = parser 'n' "number" "number all output lines" [Number]


squeezeBlank :: Parser [Option]
squeezeBlank = parser 's' "squeeze-blank" "suppress repeated empty output lines" [SqueezeBlank]


showNonprintingAndTabs :: Parser [Option]
showNonprintingAndTabs = shortParser 't' "equivalent to -vT" [ShowNonprinting, ShowTabs]


showTabs :: Parser [Option]
showTabs = parser 'T' "show-tabs" "display TAB characters as ^I" [ShowTabs]


showNonprinting :: Parser [Option]
showNonprinting = parser 'v' "show-nonprinting" "use ^ and M- notation, except for LFD and TAB" [ShowNonprinting]


u :: Parser [Option]
u = shortParser 'u' "(ignored)" []


parser :: Char -> String -> String -> [Option] -> Parser [Option]
parser s l h o= flag' o $ short s <> help h <> long l


shortParser :: Char -> String -> [Option] -> Parser [Option]
shortParser s h o = flag' o $ short s <> help h
