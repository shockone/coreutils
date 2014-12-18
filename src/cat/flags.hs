module Cat.Flags where

import Options.Applicative

import Cat.Types


number :: Parser Option
number = flag' Number $ long "number" <> short 'n' <> help "number all output lines"


numberNonBlank :: Parser Option
numberNonBlank = flag' NumberNonBlank $ long "number-nonblank" <> short 'b' <> help "number nonempty output lines, overrides -n"


showAll :: Parser Option
showAll = flag' All $ long "show-all" <> short 'A' <> help "equivalent to -vET"
