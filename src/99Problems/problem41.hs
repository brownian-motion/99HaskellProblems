module Main(main) where
import           Test.HUnit

-- Problem 41
-- Given a range of integers by its lower and upper limit,
-- print a list of all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
-- Very rarely, the primes are both bigger than say 50.
-- Try to find out how many such cases there are in the range 2..3000.

goldbachRange :: Integer -> Integer -> [(Integer, Integer)]
goldbachRange lower upper = [goldbach x | x <- [lower..upper], x `divisibleBy` 2]

goldbachRange' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachRange' lower upper minValue = filter bothAboveMin $ goldbachRange lower upper
          where bothAboveMin (a, b) = (a > minValue) && (b > minValue)

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
    TestCase (assertEqual "goldbach conjecture of evens from 9~20" [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)] (goldbachRange 9 20)),
    TestCase (assertEqual "goldbach conjecture of evens from 4~2000, where both are > 50" [(73,919),(61,1321),(67,1789),(61,1867)] (goldbachRange' 4 2000 50))
    ]

main :: IO Counts
main = runTestTT tests

