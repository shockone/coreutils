module Cat.Flags where

import Options.Applicative

import Cat.Types


number :: Parser Option
number = parser Number 'n' "number" "number all output lines"


numberNonBlank :: Parser Option
numberNonBlank = parser NumberNonBlank 'b' "number-nonblank" "number nonempty output lines, overrides -n"


showAll :: Parser Option
showAll = parser All 'A' "show-all"  "equivalent to -vET"


showEnds :: Parser Option
showEnds = parser ShowEnds 'E' "show-ends" "display $ at end of each line"


parser :: Option -> Char -> String -> String -> Parser Option
parser f s l h = flag' f $ long l <> short s <> help h
