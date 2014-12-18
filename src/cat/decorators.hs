module Cat.Decorators (
    addNumbers,
    addNonBlankNumbers
) where

import Cat.Types
import Control.Applicative((<*>))


addNumbers :: FileContent -> FileContent
addNumbers content = format $ enumerate content


addNonBlankNumbers :: FileContent -> FileContent
addNonBlankNumbers content = format $ enumerateNonBlank content


enumerateNonBlank :: FileContent -> [(Maybe Int, String)]
enumerateNonBlank = numerator 1


numerator :: Int -> FileContent -> [(Maybe Int, String)]
numerator _ [] = []
numerator n (l:rest) | null l = (Nothing, l) : numerator n rest
                     | otherwise = (Just n, l) : numerator (n+1) rest


enumerate :: FileContent -> [(Maybe Int, String)]
enumerate = zip ([Just] <*> [1..])


format :: [(Maybe Int, String)] -> FileContent
format numbersWithRows = map (formatLine padding) numbersWithRows
  where padding = calculatePadding $ fromIntegral $ length numbersWithRows


calculatePadding :: Double -> Int
calculatePadding = ceiling . log


formatLine :: Int -> (Maybe Int, String) -> String
formatLine paddingWidth (lineNumber, line) = pad lineNumber paddingWidth ++ line


pad :: Maybe Int -> Int -> String
pad Nothing maxPaddingWidth = replicate maxPaddingWidth ' ' ++ "  "
pad (Just number) maxPaddingWidth = padding ++ shownNumber ++ "  "
  where
    padding = replicate paddingWidth ' '
    paddingWidth = (maxPaddingWidth - length shownNumber + 4)
    shownNumber = show number
