module Main(main) where
import           Test.HUnit

-- Problem 5:
-- Reverse a list.

myReverse  :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl (\revList x -> x:revList) [] xs

tests :: Test
tests = TestList [
    TestCase (assertEqual "Reverse of [1..4] should be [4..1]" [4, 3, 2, 1] (myReverse [1, 2, 3, 4])),
    TestCase (assertEqual "Reverse of \"hello\" should be \"olleh\"" "olleh" (myReverse "hello"))
    ]

main :: IO Counts
main = runTestTT tests
