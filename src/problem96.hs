module Main(main) where
import           Data.Char  (isDigit, isLetter)
import           Test.HUnit

-- problem 96
-- Ada identifier syntax checker
-- a valid id matches /\w(-?[\d\w])*

isValid :: String -> Bool
isValid [] = False
isValid chars = checkIdStart chars
    where checkIdStart []     = False
          checkIdStart (c:cs) = isLetter c  && checkIdTail cs
          checkIdTail  []           = True
          checkIdTail  ('-':(c:cs)) = (isLetter c || isDigit c) && checkIdTail cs
          checkIdTail  (c:cs)       = (isLetter c || isDigit c) && checkIdTail cs

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertValid "id"),
        TestCase (assertValid "id-with-2-hyphens"),
        TestCase (assertInvalid "id-with--double-hyphens"),
        TestCase (assertValid "longid"),
        TestCase (assertValid "a1234"),
        TestCase (assertInvalid "1234"),
        TestCase (assertInvalid "this-ends-in-"),
        TestCase (assertValid "a"),
        TestCase (assertInvalid "")
    ]
    where assertValid   token = assertEqual (token ++ " should be valid") True (isValid token)
          assertInvalid token = assertEqual (token ++ " should be invalid") False (isValid token)
