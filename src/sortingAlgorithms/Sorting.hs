module Sorting(isSorted) where

isSorted :: (Ord a) => [a] -> Bool
isSorted []              = True
isSorted [_]             = True
isSorted (x1: xs@(x2:_)) = (x1 <= x2) && isSorted xs
