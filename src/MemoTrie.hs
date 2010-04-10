{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

{- Adapted from Conal Elliott's MemoTree package. ( http://haskell.org/haskellwiki/MemoTrie )

We don't just use that package, because it is affected by this bug:

  http://hackage.haskell.org/trac/ghc/ticket/2888

This implementation works around that bug by using functional dependencies and multi-param type classes instead of type families.

-}

module MemoTrie (Trie(..), memo, PairTrie(..), BoolTrie(..)) where

import qualified Data.Char as Char
import qualified Data.Bits as Bits

class Trie a t | a → t where
  trie :: (a → b) → (t b)
  untrie :: (t b) → (a → b)

memo :: Trie b t ⇒ (b → a) → (b → a)
memo = untrie . trie

data UnitTrie a = UnitTrie a
instance Trie () UnitTrie where
  trie f = UnitTrie (f ())
  untrie (UnitTrie a) = const a

data BoolTrie a = BoolTrie a a
instance Trie Bool BoolTrie where
  trie f = BoolTrie (f False) (f True)
  untrie (BoolTrie f t) b = if b then t else f

data EitherTrie ta tb x = EitherTrie (ta x) (tb x)
instance (Trie a ta, Trie b tb) ⇒ Trie (Either a b) (EitherTrie ta tb) where
  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
  untrie (EitherTrie s t) = either (untrie s) (untrie t)

data PairTrie ta tb x = PairTrie (ta (tb x))
instance (Trie a ta, Trie b tb) ⇒ Trie (a, b) (PairTrie ta tb) where
  trie f = PairTrie (trie (trie . curry f))
  untrie (PairTrie t) = uncurry (untrie . untrie t)

data ListTrie tx x = ListTrie x (tx (ListTrie tx x))
instance Trie x tx ⇒ Trie [x] (ListTrie tx) where
  trie f = ListTrie (f []) (trie (\y → trie $ f . (y:)))
  untrie (ListTrie p _) [] = p
  untrie (ListTrie _ q) (h:t) = untrie (untrie q h) t

bits :: Bits.Bits t ⇒ t → [Bool]
bits 0 = []
bits x = Bits.testBit x 0 : bits (Bits.shiftR x 1)

unbit :: Num t ⇒ Bool → t
unbit False = 0
unbit True = 1

unbits :: Bits.Bits t ⇒ [Bool] → t
unbits [] = 0
unbits (x:xs) = (Bits..|.) (unbit x) (Bits.shiftL (unbits xs) 1)

instance Trie Char (ListTrie BoolTrie) where
  trie f = trie (f . Char.chr . unbits)
  untrie t = untrie t . bits . Char.ord

{- Test:

    f :: String → Integer
    f = memo $ \l → case l of
      [] → 1
      (h:t) → (if Char.isAlpha h then 2 else 1) * slow_func t + slow_func t

    out = slow_func $ take 21 $ cycle "a9d55"

Replacing memo with id makes f much slower.

-}
