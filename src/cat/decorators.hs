module Cat.Decorators (
    addNumbers
) where

import Cat.Types


addNumbers :: FileContent -> FileContent
addNumbers content = format $ enumerate content


enumerate :: FileContent -> [(Int, String)]
enumerate = zip [1..]


format :: [(Int, String)] -> FileContent
format rows = map (formatLine padding) rows
  where padding = calculatePadding $ fromIntegral $ length rows


calculatePadding :: Double -> Int
calculatePadding = ceiling . log


formatLine :: Int -> (Int, String) -> String
formatLine paddingWidth (number, line) = pad number paddingWidth ++ line


pad :: Int -> Int -> String
pad number maxPaddingWidth = padding ++ shownNumber ++ "  "
  where
    padding = replicate paddingWidth ' '
    paddingWidth = (maxPaddingWidth - length shownNumber + 4)
    shownNumber = show number
