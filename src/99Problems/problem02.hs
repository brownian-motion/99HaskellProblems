module Main(main) where

import Test.HUnit

-- Problem 2:
-- Find the last-but-one element of a list.

my2ndToLast  :: [a] -> a
my2ndToLast (x:[_]) = x
my2ndToLast (_:xs)    = my2ndToLast xs
my2ndToLast _         = error "Too few elements"

tests :: Test
tests = TestList [
    TestCase (assertEqual "2nd-to-last element of [1..4] should be 3" 3 (my2ndToLast [1..4])),
    TestCase (assertEqual "2nd-to-last element of \"Hello\" should be \'l\'" 'l' (my2ndToLast "hello"))
    ]

main :: IO Counts
main = runTestTT tests
