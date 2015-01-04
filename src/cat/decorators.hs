module Cat.Decorators ( decorate ) where

import           Cat.Types
import           Control.Applicative        ((<*>))
import           Data.ByteString.Char8      (ByteString, append, concat,
                                             concatMap, empty, null, pack,
                                             singleton, split)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Search.DFA as S
import           Data.Char                  (chr, ord)
import           Data.List                  (intersperse)
import           Prelude                    hiding (concat, concatMap, null,
                                             unlines)


decorate :: ByteString -> Option -> ByteString
decorate content NumberNonBlank  = splitting content (format . enumerateNonBlank)
decorate content ShowEnds        = replace "\n" "$\n" content
decorate content Number          = splitting content (format . enumerate)
decorate content SqueezeBlank    = splitting content removeRepeatedBlankLines
decorate content ShowTabs        = replace "\t" "^I" content
decorate content ShowNonprinting = splitting content $ map (concatMap np)


replace :: String -> String -> ByteString -> ByteString
replace from to content = toStrict $ S.replace (pack from) (pack to) content


toStrict :: BL.ByteString -> ByteString
toStrict = concat . BL.toChunks


splitting :: ByteString -> ([ByteString] -> [ByteString]) -> ByteString
splitting byteString f = unlines $ f $ split '\n' byteString


unlines :: [ByteString] -> ByteString
unlines [] = empty
unlines ss = concat $ intersperse nl ss
    where nl = singleton '\n'


np :: Char -> ByteString
np ch
    | ord ch >= 32 && ord ch < 127 = pack [ch]
    | ord ch == 127 = pack "^?"
    | ord ch >= 128 + 32 && ord ch < 128 + 127 = pack $ "M-" ++ [chr $ ord ch - 128]
    | ord ch >= 128 + 32 = pack "M-^?"
    | ord ch >= 32 = pack $ "M-^" ++ [chr $ ord ch - 128 + 64]
    | ch == '\t' = pack [ch]
    | ch == '\n' = pack [ch] -- should break.
    | otherwise = pack $ '^':[chr $ ord ch + 64]


removeRepeatedBlankLines :: [ByteString] -> [ByteString]
removeRepeatedBlankLines [] = []
removeRepeatedBlankLines [x] = [x]
removeRepeatedBlankLines (x:y:rest) | null x && null y = removeRepeatedBlankLines (x:rest)
removeRepeatedBlankLines (x:rest) = x:removeRepeatedBlankLines rest


enumerateNonBlank :: [ByteString] -> [(Maybe Int, ByteString)]
enumerateNonBlank = numerator 1


numerator :: Int -> [ByteString] -> [(Maybe Int, ByteString)]
numerator _ [] = []
numerator n (l:rest) | null l = (Nothing, l) : numerator n rest
                     | otherwise = (Just n, l) : numerator (n+1) rest

enumerate :: [ByteString] -> [(Maybe Int, ByteString)]
enumerate = zip ([Just] <*> [1..])


format :: [(Maybe Int, ByteString)] -> [ByteString]
format numbersWithRows = map (formatLine padding) numbersWithRows
  where padding = calculatePadding $ fromIntegral $ length numbersWithRows


calculatePadding :: Double -> Int
calculatePadding = ceiling . log


formatLine :: Int -> (Maybe Int, ByteString) -> ByteString
formatLine paddingWidth (lineNumber, line) = append (pad lineNumber paddingWidth) line


pad :: Maybe Int -> Int -> ByteString
pad Nothing _ = pack ""
pad (Just number) maxPaddingWidth = foldl1 append [padding, pack shownNumber, pack "\t"]
  where
    padding = pack $ replicate paddingWidth ' '
    paddingWidth = (maxPaddingWidth - length shownNumber)
    shownNumber = show number
