module Cat.Parsers where
-- TODO: don't export everything.

import Options.Applicative

import Cat.Types


showAll :: Parser Option
showAll = parser ShowAll 'A' "show-all"  "equivalent to -vET"


numberNonBlank :: Parser Option
numberNonBlank = parser NumberNonBlank 'b' "number-nonblank" "number nonempty output lines, overrides -n"


showNonprintingAndEnds :: Parser Option
showNonprintingAndEnds = shortParser ShowNonprintingAndEnds 'e' "equivalent to -vE"


showEnds :: Parser Option
showEnds = parser ShowEnds 'E' "show-ends" "display $ at end of each line"


number :: Parser Option
number = parser Number 'n' "number" "number all output lines"


squeezeBlank :: Parser Option
squeezeBlank = parser SqueezeBlank 's' "squeeze-blank" "suppress repeated empty output lines"


showNonprintingAndTabs :: Parser Option
showNonprintingAndTabs = shortParser ShowNonprintingAndTabs 't' "equivalent to -vT"


showTabs :: Parser Option
showTabs = parser ShowTabs 'T' "show-tabs" "display TAB characters as ^I"


showNonprinting :: Parser Option
showNonprinting = parser ShowNonprinting 'v' "show-nonprinting" "use ^ and M- notation, except for LFD and TAB"


u :: Parser Option
u = shortParser U 'u' "(ignored)"


parser :: Option -> Char -> String -> String -> Parser Option
parser f s l h = flag' f $ short s <> help h <> long l


shortParser :: Option -> Char -> String -> Parser Option
shortParser f s h = flag' f $ short s <> help h
