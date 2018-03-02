module Main(main) where
import           Sorting    (isSorted)
import           Test.HUnit

-- Sorting problem 2: selection sort
-- Implement selection sort!

selectionSort :: (Ord a) => [a] -> [a]
selectionSort = selectHelper []
    where selectHelper :: (Ord b, Eq b) => [b] -> [b] -> [b]
          selectHelper sorted []                                  = sorted
          selectHelper [] xs@[_]                                  = xs
          selectHelper sorted xs                                  = selectHelper (largest:sorted) rest
               where (largest, rest) = extractWhen (maximum xs ==) xs

extractWhen :: (Eq a) => (a -> Bool) -> [a] -> (a, [a])
extractWhen _ [] = error "True for no elements"
extractWhen predicate (x:xs) | predicate x = (x, xs)
                             | otherwise = (x', x:xs')
                                 where (x', xs') = extractWhen predicate xs

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertSorted "['a'..'z'] should be sorted, is not" ['a'..'z']), --sanity check
        TestCase (assertSorted "['c', 'b', 'a'] should sort to ['a', 'b', 'c']" (selectionSort "cba")),
        TestCase (assertSorted "\"qwertyuiopasdfghjklzxcvbnm\" should sort to ['a'..'z']" (selectionSort "qwertyuiopasdfghjklzxcvbnm"))
    ]
    where assertSorted message xs = assertEqual message True (isSorted xs)
