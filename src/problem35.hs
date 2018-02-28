module Main(main) where
import           Test.HUnit

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
tests = TestList [
    TestCase (assertEqual "prime factorization of 10" [2, 5] (primeFactors 10)),
    TestCase (assertEqual "prime factorization of 45" [3, 3, 5] (primeFactors 45)),
    TestCase (assertEqual "prime factorization of 315" [3, 3, 5, 7] (primeFactors 315)),
    TestCase (assertEqual "prime factorization of 3150" [2, 3, 3, 5, 5, 7] (primeFactors 3150))
    ]

main :: IO Counts
main = runTestTT tests
