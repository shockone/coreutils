module Cat.Types where

data Option = ShowAll
            | NumberNonBlank
            | ShowNonprintingAndEnds
            | ShowEnds
            | Number
            | SqueezeBlank
            | ShowNonprintingAndTabs
            | ShowTabs
            | ShowNonprinting
            | U
            deriving (Eq)
