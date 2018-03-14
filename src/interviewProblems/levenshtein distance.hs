module Main(main) where
import           Test.HUnit

levenshtein :: String -> String -> Int
levenshtein [] b = length b
levenshtein a [] = length a
levenshtein a@(ai:as) b@(bj:bs) = minimum [ 1 +                           levenshtein as b  ,
                                            1 +                           levenshtein a  bs ,
                                            (if ai == bj then 0 else 1) + levenshtein as bs ]

main :: IO Counts
main = runTestTT $ TestList [
        TestCase (assertEqual "levenshtein abc abcd == 1" 1 $ levenshtein "abc" "abcd"),
        TestCase (assertEqual "levenshtein sitting sittin == 1" 1 $ levenshtein "sitting" "sittin"),
        TestCase (assertEqual "levenshtein sitting kitten == 3" 3 $ levenshtein "sitting" "kitten"),
        TestCase (assertEqual "levenshtein kitten sitting == 3" 3 $ levenshtein "kitten" "sitting")
    ]
