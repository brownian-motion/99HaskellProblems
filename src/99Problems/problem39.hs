module Main(main) where
import           Test.HUnit

-- Problem 39:
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

primesInRange :: Integer -> Integer -> [Integer]
primesInRange x y = filter isPrime [x..y]

-- Problem 31:
-- Determine whether a given integer number is prime.

isPrime :: Integer -> Bool
isPrime n | n < 2     = False
          | n == 2    = True
          | n == 3    = True
          | otherwise = and [not (n `divisibleBy` p) | p <- candidates]
          where candidates      = [2..upperBound]
                upperBound      = ceiling $ sqrt $ fromInteger n
                divisibleBy x y = (x `mod` y) == 0

tests :: Test
tests = TestList [ --TODO: fix these unit tests
    TestCase (assertEqual "primes from 10 to 20" [11, 13, 17, 19] (primesInRange 10 20))
    ]

main :: IO Counts
main = runTestTT tests
