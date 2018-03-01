module Main(main) where
import           Test.HUnit

-- problem 26
-- Create a list of all length-k combinations of a length-n list

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = []
combinations _ []     = []
combinations 1 xs     = map (:[]) xs
combinations k l@(x:xs) | k <= length l = map (x:) (combinations (k-1) xs) ++ combinations k xs
                        | otherwise = []

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertEqual "length-2 combinations of \"abc\" are [ab ac bc]" ["ab", "ac", "bc"] (combinations 2 "abc")),

        TestCase (assertEqual "length-3 combinations of \"abcde\" are [abc abd abe acd ace ade bcd bce bde cde]" ["abc", "abd", "abe", "acd", "ace", "ade", "bcd", "bce", "bde", "cde"] (combinations 3 "abcde"))
    ]
