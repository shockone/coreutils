module Cat.Types where

type Filename    = String
type FileContent = [String]

data Option      = Number | All | NonBlank deriving (Eq)
data Arguments   = Arguments [Filename] [Option]
