module Utils where

import Data.List (delete)

elems :: Eq a => [a] -> [a] -> Bool
[] `elems` _ = True
_ `elems` [] = False
needles `elems` haystack@(x:xs) | length haystack < length needles = False
                                | any (x ==) needles = (delete x needles) `elems` xs
                                | otherwise =  needles `elems` xs
