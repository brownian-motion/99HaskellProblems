module Main(main) where
import           Test.HUnit

-- Problem 33:
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

areCoprime :: (Integral a) => a -> a -> Bool
areCoprime x y = (gcd x y) == 1

tests :: Test
tests = TestList [
    TestCase (assertEqual "gcd(4,3) == 1" True (areCoprime 4 3)),
    TestCase (assertEqual "gcd(70, 55) == 5" False (areCoprime 70 55)),
    TestCase (assertEqual "gcd(108, 60) == 12" False (areCoprime 108 60)),
    TestCase (assertEqual "gcd of two primes is 1" True (areCoprime 96373819 3)),
    TestCase (assertEqual "gcd of (prime+1) and prime should be 1" True (areCoprime 96373820 96373819)),
    TestCase (assertEqual "gcd of prime and (prime+1) should be 1" True (areCoprime 96373819 96373820))
    ]

main :: IO Counts
main = runTestTT tests
