module Main(main) where
import           Test.HUnit

-- problem 95
-- English number words
-- On financial documents, like cheques, numbers must sometimes be written in full words.
-- Example: 175 must be written as one-seven-five.
-- Write a predicate fullWords/1 to print (non-negative) integer numbers in full words.

fullWords :: (Integral a) => a -> String
fullWords 1 = "one"
fullWords 2 = "two"
fullWords 3 = "three"
fullWords 4 = "four"
fullWords 5 = "five"
fullWords 6 = "six"
fullWords 7 = "seven"
fullWords 8 = "eight"
fullWords 9 = "nine"
fullWords 0 = "zero"
fullWords x | x > 10    = fullWords ( x `div` 10) ++ "-" ++ fullWords ( x `mod` 10)
            | otherwise = error "No negative numbers"

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertEqual "175 -> one-seven-five" "one-seven-five" (fullWords 175))
    ]
