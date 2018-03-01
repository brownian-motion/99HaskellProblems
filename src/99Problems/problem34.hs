module Main(main) where
import           Test.HUnit

-- Problem 33:
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

areCoprime :: (Integral a) => a -> a -> Bool
areCoprime x y = gcd x y == 1

-- Problem 34:
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

totient :: (Integral a) => a -> Int
totient x | x < 1     = 0
          | x == 1    = 1
          | otherwise = length $ filter (areCoprime x) [1..(x-1)]

tests :: Test
tests = TestList [
    TestCase (assertEqual "phi(1) == 1" 1 (totient 1)),
    TestCase (assertEqual "phi(10) == 4" 4 (totient 10))
    ]

main :: IO Counts
main = runTestTT tests
