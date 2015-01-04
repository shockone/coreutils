module Cat.Types where

data Option = NumberNonBlank
            | ShowEnds
            | Number
            | SqueezeBlank
            | ShowTabs
            | ShowNonprinting
            deriving (Eq)
