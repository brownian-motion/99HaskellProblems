module Main(main) where

import           Test.HUnit

-- Problem 1:
-- Find the last element of a list.

myLast  :: [a] -> a
myLast []     = error "Can't get the last element of an empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

main :: IO Counts
main = do
    runTestTT (TestList [test1, test2])
    where
        test1 = TestCase (assertEqual "Last element should be 4" 4 (myLast [1, 2, 3, 4]))
        test2 = TestCase (assertEqual "Last element should be o" 'o' (myLast "hello"))
