module Main(main) where
import           Test.HUnit

-- Problem 21:
-- Insert an element at a given point in a list, starting at 1

insertAt :: Int -> a -> [a]-> [a]
insertAt _ x []     = [x]
insertAt 1 x xs     = x:xs
insertAt k x (y:ys) = y : insertAt (k-1) x ys

tests :: Test
tests = TestList [
    TestCase (assertEqual "insert 5 into [1..3]@3 -> [1 2 5 3]" [1, 2, 5, 3] (insertAt 3 5 [1..3])),
    TestCase (assertEqual "insert 'a' into beginning of 'hello' -> 'ahello'" "ahello" (insertAt 1 'a' "hello"))
    ]

main :: IO Counts
main = runTestTT tests
