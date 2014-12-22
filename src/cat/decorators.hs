module Cat.Decorators ( decorate ) where

import Cat.Types
import Control.Applicative((<*>))


decorate :: FileContent -> Option -> FileContent
decorate content ShowAll                 = format $ enumerate content
decorate content NumberNonBlank          = format $ enumerateNonBlank content
decorate content ShowNonprintingAndEnds  = undefined
decorate content ShowEnds                = map (++ "$") content
decorate content Number                  = format $ enumerate content
decorate content SqueezeBlank            = removeRepeatedBlankLines content
decorate content ShowNonprintingAndTabs  = undefined
decorate content ShowTabs                = undefined
decorate content ShowNonprinting         = undefined
decorate content U                       = content


removeRepeatedBlankLines :: [String] -> [String]
removeRepeatedBlankLines [] = []
removeRepeatedBlankLines [x] = [x]
removeRepeatedBlankLines ("":"":rest) = removeRepeatedBlankLines ("":rest)
removeRepeatedBlankLines (x:rest) = x:removeRepeatedBlankLines rest


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
