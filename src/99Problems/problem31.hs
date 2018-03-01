module Main(main) where
import           Test.HUnit

-- Problem 31:
-- Determine whether a given integer number is prime.

isPrime :: Integer -> Bool
isPrime n | n < 2     = False
          | n == 2    = True
          | n == 3    = True
          | otherwise = and [not (n `divisibleBy` p) | p <- candidates]
          where divisibleBy x y = (x `mod` y) == 0
                candidates  = [2..upperBound]
                upperBound  = ceiling $ sqrt $ fromInteger n

tests :: Test
tests = TestList [
    TestCase (assertEqual "4 is not prime" False (isPrime 4)),
    TestCase (assertEqual "7 is prime" True (isPrime 7)),
    TestCase (assertEqual "101 is prime" True (isPrime 101)),
    TestCase (assertEqual "-7 is not prime" False (isPrime (-7))),
    TestCase (assertEqual "Big prime number is prime" True (isPrime 96373819)),
    TestCase (assertEqual "Big prime number + 1 is not prime" False (isPrime 96373820))
    ]

main :: IO Counts
main = runTestTT tests
