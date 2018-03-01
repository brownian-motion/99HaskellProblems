module Main(main) where
import           Test.HUnit

-- Problem 32:
-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

myGCD :: (Integral a) => a -> a -> a

myGCD a b | b == 0 = a
          | b < 0  = myGCD a (-b)
          | a < b  = myGCD b a
          | a == b = a
          | a > b  = myGCD b (a-b)

tests :: Test
tests = TestList [
    TestCase (assertEqual "gcd(4,3) == 1" 1 (myGCD 4 3)),
    TestCase (assertEqual "gcd(70, 55) == 5" 5 (myGCD 70 55)),
    TestCase (assertEqual "gcd(108, 60) == 12" 12 (myGCD 108 60)),
    TestCase (assertEqual "gcd of two primes is 1" 1 (myGCD 96373819 4007)),
    -- TestCase (assertEqual "gcd of (prime+1) and (prime) should be 1" 1 (myGCD 96373820 96373819)),
    TestCase (assertEqual "gcd(1071, 462) should be 21" 21 (myGCD 1071 462)),
    TestCase (assertEqual "gcd of -6 and 3 should be 3" 3 (myGCD (-6) 3))
    ]

main :: IO Counts
main = runTestTT tests
