module Main(main) where

import Test.HUnit

-- Problem 3:
-- Find the k-th element of a list.

elementAt  :: [a] -> Int -> a
elementAt xs k = xs!!(k-1)

tests :: Test
tests = TestList [
    TestCase (assertEqual "3rd element should be 3" 3 (elementAt [1..4] 3)),
    TestCase (assertEqual "1st element should be h" 'h' (elementAt "hello" 1))
    ]

main :: IO Counts
main = runTestTT tests
