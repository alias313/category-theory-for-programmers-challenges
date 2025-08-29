{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MemoTrieLite
  ( HasTrie(..)
  , memo
  , bits
  , unBits
  ) where

-- Core class: a -> b represented as a trie a :->: b
class HasTrie a where
  infixr 2 :->:
  data (:->:) a b
  trie   :: (a -> b) -> (a :->: b)
  unTrie :: (a :->: b) -> (a -> b)

-- Generic memo combinator
memo :: HasTrie a => (a -> b) -> (a -> b)
memo = unTrie . trie

-- Bool
instance HasTrie Bool where
  data (:->:) Bool b = BoolTrie b b
  trie f = BoolTrie (f False) (f True)
  unTrie (BoolTrie bf _) False = bf
  unTrie (BoolTrie _ bt) True  = bt

-- Maybe
instance HasTrie a => HasTrie (Maybe a) where
  data (:->:) (Maybe a) b = MaybeTrie b (a :->: b)
  trie f = MaybeTrie (f Nothing) (trie (f . Just))
  unTrie (MaybeTrie b t) ma =
    case ma of
      Nothing -> b
      Just a  -> unTrie t a
-- Either
instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data (:->:) (Either a b) c = EitherTrie (a :->: c) (b :->: c)
  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))

  unTrie (EitherTrie ta tb) e =
    case e of
      Left a  -> unTrie ta a
      Right b -> unTrie tb b
-- Pairs
newtype PairTrie a b c = PairTrie (a :->: (b :->: c))
instance (HasTrie a, HasTrie b) => HasTrie (a, b) where
  data (:->:) (a, b) c = Tuple2Trie (PairTrie a b c)
  trie f =
    let t = trie $ \a -> trie $ \b -> f (a, b)
    in Tuple2Trie (PairTrie t)
  unTrie (Tuple2Trie (PairTrie t)) (a, b) = unTrie (unTrie t a) b

-- Lists
instance HasTrie a => HasTrie [a] where
  data (:->:) [a] b = ListTrie b (a :->: ([a] :->: b))
  trie f =
    let consTrie = trie $ \x -> trie $ \xs -> f (x:xs)
    in ListTrie (f []) consTrie
  unTrie (ListTrie b consTrie) = \xs -> case xs of
    []     -> b
    (y:ys) -> unTrie (unTrie consTrie y) ys

-- Integer via [Bool] (non-negative only)
newtype IntegerTrie b = IntegerTrie ([Bool] :->: b)
instance HasTrie Integer where
  data (:->:) Integer b = ITrie (IntegerTrie b)
  trie f = ITrie (IntegerTrie (trie (f . unBits)))
  unTrie (ITrie (IntegerTrie t)) = unTrie t . bits

-- Helpers for Integer <-> [Bool], little-endian
bits :: Integral a => a -> [Bool]
bits n
  | n < 0     = error "bits: negative not supported"
  | n == 0    = []
  | otherwise = (n `mod` 2 == 1) : bits (n `div` 2)

unBits :: Integral a => [Bool] -> a
unBits []           = 0
unBits (False : xs) = 2 * unBits xs
unBits (True  : xs) = 2 * unBits xs + 1