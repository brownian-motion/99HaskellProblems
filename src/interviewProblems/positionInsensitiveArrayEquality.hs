module Main(main) where
import           Data.Maybe (fromJust, isJust, isNothing)
import           Test.HUnit

-- Problem: Check if two arrays have the same contents in any order.
-- From https://practice.geeksforgeeks.org/problems/check-if-two-arrays-are-equal-or-not/0

yank :: (Eq a) => a -> [a] -> Maybe [a]
yank _ [] = Nothing
yank x (y:ys) | x == y    = Just ys
              | otherwise = if isNothing result then Nothing else Just (y:ys')
                    where result@(Just ys') = yank x ys

arrEquals :: (Eq a) => [a] -> [a] -> Bool
arrEquals [] []     = True
arrEquals [] _      = False
arrEquals (a:as) bs = isJust yanked && arrEquals as (fromJust yanked)
                          where yanked = yank a bs

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertEqual "arrEquals works for [a] [a]" True $ arrEquals "a" "a"),
        TestCase (assertEqual "arrEquals works for [ab] [ba]" True $ arrEquals "ab" "ba"),
        TestCase (assertEqual "arrEquals works for [ggha] [hgag]" True $ arrEquals "ggha" "hgag"),
        TestCase (assertEqual "arrEquals works for [abcd] [cba]" False $ arrEquals "abcd" "cba")
    ]
