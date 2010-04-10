{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}

module MemberTest (MemberTest, (∈), (∉), (∋), (∌)) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

class MemberTest a t where (∈) :: a → t → Bool

-- We don't let ∉ be specialized independently, because it does not make sense to have a ∉ that is faster than ∈. After all, in that case, one should have defined ∈ as the negation of ∉ and have it be as fast.

(∉) :: MemberTest a t ⇒ a → t → Bool
(∉) x = not . (x ∈)

(∋), (∌) :: MemberTest a t ⇒ t → a → Bool
(∋) = flip (∈)
(∌) = flip (∉)

infix 4 ∈
infix 4 ∋
infix 4 ∉
infix 4 ∌

instance Eq a ⇒ MemberTest a [a] where (∈) = elem
instance Ord a ⇒ MemberTest a (Set.Set a) where (∈) = Set.member
instance Ord k ⇒ MemberTest k (Map.Map k v) where (∈) = Map.member
instance MemberTest Int IntSet.IntSet where (∈) = IntSet.member
instance MemberTest Int (IntMap.IntMap v) where (∈) = IntMap.member

{- Bad because it overlaps and is non-optimal:

import qualified Data.Foldable as Foldable

instance (Foldable.Foldable f, Eq a) => MemberTest a (f a) where (∈) = Foldable.elem
-}
