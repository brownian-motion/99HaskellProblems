module Main(main) where
import           Test.HUnit

-- problem 22
-- Create a range of numbers

range :: (Enum a, Ord a) => a -> a -> [a]
range lower upper | lower <= upper = lower : range (succ lower) upper
                  | lower > upper = []

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertEqual "[2..13] == range 2 13" [2..13] (range 2 13)),
        TestCase (assertEqual "['a'..'z'] == range 'a' 'z'" ['a'..'z'] (range 'a' 'z'))
    ]
