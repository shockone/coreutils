module Cat.Types where

type Filename    = String
type FileContent = [String]

data Option = ShowAll | NumberNonBlank | ShowEnds
            | Number | SqueezeBlank | ShowNonprintingAndTabs
            | ShowTabs | ShowNonprinting | U
            deriving (Eq)

data Arguments = Arguments [Filename] [Option]

-- A prototype of the future refactoring.
-- class Option o where
--   parser :: Option o => Parser o
--   decorator :: FileContent -> Filecontent
