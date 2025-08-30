{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified GHC.Clock as GHCClock
import Text.Printf (printf)
import Control.Exception (evaluate)

import MemoTrieLite (memo)

-- Timing helpers (base only)
nowNs :: IO Integer
nowNs = toInteger <$> GHCClock.getMonotonicTimeNSec

toMs :: Integer -> Double
toMs ns = fromIntegral ns / 1.0e6

-- Force Bool inside IO boundary
forceBoolIO :: Bool -> IO Bool
forceBoolIO b = do
  let !r = b
  _ <- evaluate r
  pure r

-- Repeat calling f x n times. For memoized second run, calls are cheap.
loopNIO :: Int -> (a -> Bool) -> a -> IO Bool
loopNIO n f x = go n
  where
    go k
      | k <= 1    = forceBoolIO (f x)
      | otherwise = do
          !_ <- forceBoolIO (f x)
          go (k - 1)

-- Primality test (odd divisors only)
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | n `mod` 2 == 0 = False
  | otherwise = go 3
  where
    !limit = floor (sqrt (fromIntegral n :: Double))
    go !i
      | i > limit = True
      | n `mod` i == 0 = False
      | otherwise = go (i + 2)

isPrimeList :: [Integer] -> Bool
isPrimeList = all isPrime

memoIsPrimeList :: [Integer] -> Bool
memoIsPrimeList = memo isPrimeList

getPrimes :: IO [Integer]
getPrimes = pure
  [ 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47
  , 53,59,61,67,71,73,79,83,89,97
  , 101,103,107,109,113,127,131,137,139,149
  , 1981201020802099
  , 144403552893599
  , 29100036564239
  , 1012020412021
  , 166666666667
  , 10999999999
  ]

main :: IO ()
main = do
  input <- getPrimes
  let f = memoIsPrimeList

  -- Amplify work: increase this until you see separation.
  let iters = 1

  -- First window: compute many times (only first actually computes, rest still compute if memo is per-argument-list; here it's same list so they hit cache only after first)
  t1Start <- nowNs
  r1 <- loopNIO iters f input
  t1End <- nowNs
  let firstMs = toMs (t1End - t1Start)

  if not r1
    then error "Expected all primes to be True"
    else putStrLn "[haskell] first run OK"

  -- Second window: should be fully memoized and thus much faster
  t2Start <- nowNs
  _r2 <- loopNIO iters f input
  t2End <- nowNs
  let secondMs = toMs (t2End - t2Start)

  let speedup =
        if secondMs > 0 then firstMs / secondMs else read "Infinity" :: Double
      speedupStr =
        if isInfinite speedup
          then "∞x"
          else printf "%.1fx" speedup

  printf "[haskell:all-primes] firstMs=%.6fms secondMs=%.6fms speedup=%s\n"
         firstMs secondMs speedupStr