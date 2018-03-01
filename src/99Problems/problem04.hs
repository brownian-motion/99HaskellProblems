module Main(main) where

import Test.HUnit

-- Problem 4:
-- Find the number of elements in a list.

myLength  :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

tests :: Test
tests = TestList [
    TestCase (assertEqual "Array should be length 3" 3 (myLength [1..3])),
    TestCase (assertEqual "\"hello\" should be length 5" 5 (myLength "hello"))
    ]

main :: IO Counts
main = runTestTT tests
