module Cat.Decorators ( decorate ) where

import Cat.Types
import Control.Applicative ((<*>))
import Data.Char (ord, chr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Search.DFA as S


decorate :: BS.ByteString -> Option -> BS.ByteString
decorate content NumberNonBlank  = splitting content (format . enumerateNonBlank)
decorate content ShowEnds        = splitting content $ map (flip BS.append (BS.pack "$"))
decorate content Number          = splitting content (format . enumerate)
decorate content SqueezeBlank    = splitting content removeRepeatedBlankLines
decorate content ShowTabs        = splitting content $ map (toStrict1 . S.replace (BS.pack "\t") (BS.pack "^I"))
decorate content ShowNonprinting = splitting content $ map (BS.concatMap np)

toStrict1 :: BL.ByteString -> BS.ByteString
toStrict1 = BS.concat . BL.toChunks

splitting :: BS.ByteString -> ([BS.ByteString] -> [BS.ByteString]) -> BS.ByteString
splitting byteString f = BS.unlines $ f $ if BS.null (last split) then init split else split
    where split = BS.split '\n' byteString


np :: Char -> BS.ByteString
np ch
    | ord ch >= 32 && ord ch < 127 = BS.pack [ch]
    | ord ch == 127 = BS.pack "^?"
    | ord ch >= 128 + 32 && ord ch < 128 + 127 = BS.pack $ "M-" ++ [chr $ ord ch - 128]
    | ord ch >= 128 + 32 = BS.pack "M-^?"
    | ord ch >= 32 = BS.pack $ "M-^" ++ [chr $ ord ch - 128 + 64]
    | ch == '\t' = BS.pack [ch]
    | ch == '\n' = BS.pack [ch] -- should break.
    | otherwise = BS.pack $ '^':[chr $ ord ch + 64]


removeRepeatedBlankLines :: [BS.ByteString] -> [BS.ByteString]
removeRepeatedBlankLines [] = []
removeRepeatedBlankLines [x] = [x]
removeRepeatedBlankLines (x:y:rest) | BS.null x && BS.null y = removeRepeatedBlankLines (x:rest)
removeRepeatedBlankLines (x:rest) = x:removeRepeatedBlankLines rest


enumerateNonBlank :: [BS.ByteString] -> [(Maybe Int, BS.ByteString)]
enumerateNonBlank = numerator 1


numerator :: Int -> [BS.ByteString] -> [(Maybe Int, BS.ByteString)]
numerator _ [] = []
numerator n (l:rest) | BS.null l = (Nothing, l) : numerator n rest
                     | otherwise = (Just n, l) : numerator (n+1) rest

enumerate :: [BS.ByteString] -> [(Maybe Int, BS.ByteString)]
enumerate = zip ([Just] <*> [1..])


format :: [(Maybe Int, BS.ByteString)] -> [BS.ByteString]
format numbersWithRows = map (formatLine padding) numbersWithRows
  where padding = calculatePadding $ fromIntegral $ length numbersWithRows


calculatePadding :: Double -> Int
calculatePadding = ceiling . log


formatLine :: Int -> (Maybe Int, BS.ByteString) -> BS.ByteString
formatLine paddingWidth (lineNumber, line) = BS.append (pad lineNumber paddingWidth) line


pad :: Maybe Int -> Int -> BS.ByteString
pad Nothing _ = BS.pack ""
pad (Just number) maxPaddingWidth = foldl1 BS.append [padding, BS.pack shownNumber, BS.pack "\t"]
  where
    padding = BS.pack $ replicate paddingWidth ' '
    paddingWidth = (maxPaddingWidth - length shownNumber + 3)
    shownNumber = show number
