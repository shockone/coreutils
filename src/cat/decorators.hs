module Cat.Decorators ( decorate ) where

import Cat.Types
import Control.Applicative((<*>))


decorate :: Option -> FileContent -> FileContent
decorate Number content         = format $ enumerate content
decorate NumberNonBlank content = format $ enumerateNonBlank content
decorate ShowEnds content       = map (++ "$") content
decorate _ content              = content


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
    paddingWidth = (maxPaddingWidth - length shownNumber + 3)
    shownNumber = show number
