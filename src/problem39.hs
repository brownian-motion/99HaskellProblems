module Main(main) where
import           Data.List  (group)
import           Test.HUnit

-- Problem 39:
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

primesInRange :: Integer -> Integer -> [Integer]
primesInRange x y = filter isPrime [x..y]

-- Problem 35:
-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x | null candidatePrimes   = [x]
               | otherwise              = firstPrimeFactor : primeFactors (x `div` firstPrimeFactor)
                 where candidatePrimes  = dropWhile (not . (x `divisibleBy`)) (takeWhile (< x) primes)
                       firstPrimeFactor = head candidatePrimes

primes :: [Integer]
primes = filter isPrime [2..]

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
    TestCase (assertEqual "prime factorization of 10" [(2, 1), (5, 1)] (primeFactorsMult 10)),
    TestCase (assertEqual "prime factorization of 45" [(3, 2), (5, 1)] (primeFactorsMult 45)),
    TestCase (assertEqual "prime factorization of 315" [(3, 2), (5, 1), (7, 1)] (primeFactorsMult 315)),
    TestCase (assertEqual "prime factorization of 3150" [(2, 1), (3, 2), (5, 2), (7, 1)] (primeFactorsMult 3150))
    ]

main :: IO Counts
main = runTestTT tests
