module Cat.Types where

type Filename    = String
type FileContent = [String]

data Option = ShowAll
            | NumberNonBlank
            | ShowEnds
            | Number
            | SqueezeBlank
            | ShowNonprintingAndTabs
            | ShowTabs
            | ShowNonprinting
            | U
            deriving (Eq)

data Arguments = Arguments [Filename] [Option]
