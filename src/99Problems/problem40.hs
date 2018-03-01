module Main(main) where
import           Test.HUnit

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- Example: 28 = 5 + 23.
-- It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
-- It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system).
-- Write a predicate to find the two prime numbers that sum up to a given even integer.

goldbach :: Integer -> (Integer, Integer)
goldbach x = goldbach_helper x (primesInRange 1 x)
  where goldbach_helper :: Integer -> [Integer] -> (Integer, Integer)
        goldbach_helper _ []                  = error "Goldbach conjecture failed"
        goldbach_helper y (p:candidatePrimes) | isPrime remainder         = (p, remainder)
                                              | otherwise                 = goldbach_helper y candidatePrimes
                                              where remainder = y - p

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
          where candidates  = [2..upperBound]
                upperBound  = ceiling $ sqrt $ fromInteger n

divisibleBy :: (Integral a) => a -> a -> Bool
divisibleBy x y = (x `mod` y) == 0

tests :: Test
tests = TestList [ --TODO: fix these unit tests
    TestCase (assertEqual "goldbach conjecture of 28" (5, 23) (goldbach 28))
    ]

main :: IO Counts
main = runTestTT tests
