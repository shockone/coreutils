module Utils where

import Data.List (delete)

elems :: Eq a => [a] -> [a] -> Bool
[] `elems` _ = True
_ `elems` [] = False
needles `elems` (x:xs) = newNeedles `elems` xs
    where newNeedles | any (x ==) needles = delete x needles
                     | otherwise = needles
