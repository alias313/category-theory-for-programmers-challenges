module Main where

import MemoTrieLite (memo)

-- f(n) = max n (f(n/2) + f(n/3) + f(n/4))
f :: Integer -> Integer
f = memo f'
  where
    f' 0 = 0
    f' n = max n (f (n `div` 2) + f (n `div` 3) + f (n `div` 4))

main :: IO ()
main = do
  print (f 12)
  print (f 12380192300)