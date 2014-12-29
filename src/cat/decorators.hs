module Cat.Decorators ( decorate ) where

import Cat.Types
import Control.Applicative((<*>))
import Data.String.Utils(replace)


decorate :: [String] -> Option -> [String]
decorate content NumberNonBlank          = format $ enumerateNonBlank content
decorate content ShowEnds                = map (++ "$") content
decorate content Number                  = format $ enumerate content
decorate content SqueezeBlank            = removeRepeatedBlankLines content
decorate content ShowTabs                = map (replace "\t" "^I") content
decorate _ ShowNonprinting               = undefined


removeRepeatedBlankLines :: [String] -> [String]
removeRepeatedBlankLines [] = []
removeRepeatedBlankLines [x] = [x]
removeRepeatedBlankLines ("":"":rest) = removeRepeatedBlankLines ("":rest)
removeRepeatedBlankLines (x:rest) = x:removeRepeatedBlankLines rest


enumerateNonBlank :: [String] -> [(Maybe Int, String)]
enumerateNonBlank = numerator 1


numerator :: Int -> [String] -> [(Maybe Int, String)]
numerator _ [] = []
numerator n (l:rest) | null l = (Nothing, l) : numerator n rest
                     | otherwise = (Just n, l) : numerator (n+1) rest


enumerate :: [String] -> [(Maybe Int, String)]
enumerate = zip ([Just] <*> [1..])


format :: [(Maybe Int, String)] -> [String]
format numbersWithRows = map (formatLine padding) numbersWithRows
  where padding = calculatePadding $ fromIntegral $ length numbersWithRows


calculatePadding :: Double -> Int
calculatePadding = ceiling . log


formatLine :: Int -> (Maybe Int, String) -> String
formatLine paddingWidth (lineNumber, line) = pad lineNumber paddingWidth ++ line


pad :: Maybe Int -> Int -> String
pad Nothing _ = ""
pad (Just number) maxPaddingWidth = padding ++ shownNumber ++ "\t"
  where
    padding = replicate paddingWidth ' '
    paddingWidth = (maxPaddingWidth - length shownNumber + 3)
    shownNumber = show number
