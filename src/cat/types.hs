module Cat.Types where

data Option = ShowTabs
            | NumberNonBlank
            | SqueezeBlank
            | Number
            | ShowNonprinting
            | ShowEnds
            deriving (Eq, Ord)
