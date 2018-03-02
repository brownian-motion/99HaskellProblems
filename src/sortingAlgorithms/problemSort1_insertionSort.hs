module Main(main) where
import           Sorting    (isSorted)
import           Test.HUnit

-- Sorting problem 1: insertion sort
-- Implement insertion sort!

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldl insert []
    where insert :: (Ord a) => [a] -> a -> [a]
          insert [] x                                  = [x]
          insert (s:sorted) x | s < x       = s : insert sorted x
                                 | otherwise   = x : sorted

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertSorted "['a'..'z'] should be sorted, is not" ['a'..'z']), --sanity check
        TestCase (assertSorted "['c', 'b', 'a'] should sort to ['a', 'b', 'c']" (insertionSort "cba")),
        TestCase (assertSorted "\"qwertyuiopasdfghjklzxcvbnm\" should sort to ['a'..'z']" (insertionSort "qwertyuiopasdfghjklzxcvbnm"))
    ]
    where assertSorted message xs = assertEqual message True (isSorted xs)
