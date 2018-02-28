module Main(main) where
import           Test.HUnit

-- Problem 6:
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

tests :: Test
tests = TestList [
    TestCase (assertEqual "[1..4] should not be a palindrome" False (isPalindrome [1, 2, 3, 4])),
    TestCase (assertEqual "[1, 2, 3, 2, 1] should be a palindrome" True (isPalindrome [1, 2, 3, 2, 1])),
    TestCase (assertEqual "\"tacocat\" should be a palindrome" True (isPalindrome "tacocat"))
    ]

main :: IO Counts
main = runTestTT tests
