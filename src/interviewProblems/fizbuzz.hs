module Main(main) where

fizzbuzzStr :: Int -> String
fizzbuzzStr x | x `divby` 15 = "FizzBuzz"
               | x `divby` 5  = "Buzz"
               | x `divby` 3 = "Fizz"
               | otherwise = show x
               where divby a b = (a `mod` b) == 0

fizzbuzz :: Int -> IO ()
fizzbuzz x = mapM_ (putStrLn . fizzbuzzStr) [1..x]

main :: IO ()
main = fizzbuzz 15
