module Main(main) where
import           Sorting    (isSorted)
import           Test.HUnit

-- Sorting problem 3: merge sort
-- Implement merge sort!

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort xs@[_] = xs
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left, right) = splitInHalf xs
          splitInHalf ys= (take halfLen ys, drop halfLen ys)
          halfLen = length xs `div` 2
          merge ls [] = ls
          merge [] rs = rs
          merge (l:ls) (r:rs) | l < r     = l : merge ls (r:rs)
                              | otherwise = r : merge (l:ls) rs

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertSorted "['a'..'z'] should be sorted, is not" ['a'..'z']), --sanity check
        TestCase (assertSorted "['c', 'b', 'a'] should sort to ['a', 'b', 'c']" (mergeSort "cba")),
        TestCase (assertSorted "\"qwertyuiopasdfghjklzxcvbnm\" should sort to ['a'..'z']" (mergeSort "qwertyuiopasdfghjklzxcvbnm"))
    ]
    where assertSorted message xs = assertEqual message True (isSorted xs)
