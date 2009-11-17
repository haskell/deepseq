-----------------------------------------------------------------------------
-- |
-- Module      :  Control.DeepSeq
-- Copyright   :  (c) The University of Glasgow 2001-2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Provides an overloaded function 'deepseq' for fully evaluating data
-- structures.
-- 

module Control.DeepSeq (
     DeepSeq(..), DeepSeqIntegral, DeepSeqOrd
  ) where

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex
import Data.Map
import Data.Set
import Data.IntMap
import Data.IntSet
import Data.Tree
import Data.Array

-- A class of types that can be fully evaluated.
class DeepSeq a where
    -- | Fully evaluates its argument.  The name 'deepseq' is used to
    -- illustrate the relationship to 'seq': where 'seq' is shallow in
    -- the sense that it only evaluates the top level of its argument,
    -- 'deepseq' traverses the entire data structure evaluating it
    -- completely.
    --
    -- 'deepseq' can be useful for forcing pending exceptions,
    -- eradicating space leaks, or forcing lazy I/O to happen.  It is
    -- also useful in conjunction with parallel Strategies (see the
    -- @parallel@ package).
    --
    -- There is no guarantee about the ordering of evaluation.  The
    -- implementation may evaluate the components of the structure in
    -- any order or in parallel.  To impose an actual order on
    -- evaluation, use 'pseq' from "Control.Parallel" in the
    -- @parallel@ package.
    --
    -- The default implementation of 'deepseq' is simply 'seq', which
    -- may be convenient when defining instances for data types with
    -- no unevaluated fields (e.g. enumerations).
    --
    deepseq :: a -> ()
    deepseq a = a `seq` ()

class (DeepSeq a, Integral a) => DeepSeqIntegral a
class (DeepSeq a, Ord a) => DeepSeqOrd a

instance DeepSeq Int 
instance DeepSeq Word
instance DeepSeq Integer
instance DeepSeq Float
instance DeepSeq Double

instance DeepSeq Char
instance DeepSeq Bool
instance DeepSeq ()

instance DeepSeq Int8
instance DeepSeq Int16
instance DeepSeq Int32
instance DeepSeq Int64

instance DeepSeq Word8
instance DeepSeq Word16
instance DeepSeq Word32
instance DeepSeq Word64

instance DeepSeqIntegral Int
instance DeepSeqOrd Int

--Rational and complex numbers.

instance (Integral a, DeepSeq a) => DeepSeq (Ratio a) where
  deepseq x = deepseq (numerator x, denominator x)

instance (RealFloat a, DeepSeq a) => DeepSeq (Complex a) where
  deepseq (x:+y) = deepseq x `seq` 
	         deepseq y `seq`
               ()

instance DeepSeq a => DeepSeq (Maybe a) where
    deepseq Nothing  = ()
    deepseq (Just x) = deepseq x

instance (DeepSeq a, DeepSeq b) => DeepSeq (Either a b) where
    deepseq (Left x)  = deepseq x
    deepseq (Right y) = deepseq y

instance (DeepSeq k, DeepSeq a) => DeepSeq (Data.Map.Map k a) where
    deepseq = deepseq . Data.Map.toList

instance DeepSeq a => DeepSeq (Data.Set.Set a) where
    deepseq = deepseq . Data.Set.toList

instance DeepSeq a => DeepSeq (Data.Tree.Tree a) where
    deepseq (Data.Tree.Node r f) = deepseq r `seq` deepseq f

instance DeepSeq a => DeepSeq (Data.IntMap.IntMap a) where
    deepseq = deepseq . Data.IntMap.toList

instance DeepSeq Data.IntSet.IntSet where
    deepseq = deepseq . Data.IntSet.toList

instance DeepSeq a => DeepSeq [a] where
    deepseq [] = ()
    deepseq (x:xs) = deepseq x `seq` deepseq xs

instance (Ix a, DeepSeq a, DeepSeq b) => DeepSeq (Array a b) where
    deepseq x = deepseq (bounds x, Data.Array.elems x)

instance (DeepSeq a, DeepSeq b) => DeepSeq (a,b) where
  deepseq (x,y) = deepseq x `seq` deepseq y

instance (DeepSeq a, DeepSeq b, DeepSeq c) => DeepSeq (a,b,c) where
  deepseq (x,y,z) = deepseq x `seq` deepseq y `seq` deepseq z 

instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d) => DeepSeq (a,b,c,d) where
  deepseq (x1,x2,x3,x4) = deepseq x1 `seq` 
		        deepseq x2 `seq` 
		        deepseq x3 `seq` 
		        deepseq x4 

instance (DeepSeq a1, DeepSeq a2, DeepSeq a3, DeepSeq a4, DeepSeq a5) => 
         DeepSeq (a1, a2, a3, a4, a5) where
  deepseq (x1, x2, x3, x4, x5) =
                  deepseq x1 `seq`
                  deepseq x2 `seq`
                  deepseq x3 `seq`
                  deepseq x4 `seq`
                  deepseq x5

instance (DeepSeq a1, DeepSeq a2, DeepSeq a3, DeepSeq a4, DeepSeq a5, DeepSeq a6) => 
         DeepSeq (a1, a2, a3, a4, a5, a6) where
  deepseq (x1, x2, x3, x4, x5, x6) =
                  deepseq x1 `seq`
                  deepseq x2 `seq`
                  deepseq x3 `seq`
                  deepseq x4 `seq`
                  deepseq x5 `seq`
                  deepseq x6

instance (DeepSeq a1, DeepSeq a2, DeepSeq a3, DeepSeq a4, DeepSeq a5, DeepSeq a6, DeepSeq a7) => 
         DeepSeq (a1, a2, a3, a4, a5, a6, a7) where
  deepseq (x1, x2, x3, x4, x5, x6, x7) =
                  deepseq x1 `seq`
                  deepseq x2 `seq`
                  deepseq x3 `seq`
                  deepseq x4 `seq`
                  deepseq x5 `seq`
                  deepseq x6 `seq`
                  deepseq x7

instance (DeepSeq a1, DeepSeq a2, DeepSeq a3, DeepSeq a4, DeepSeq a5, DeepSeq a6, DeepSeq a7, DeepSeq a8) => 
         DeepSeq (a1, a2, a3, a4, a5, a6, a7, a8) where
  deepseq (x1, x2, x3, x4, x5, x6, x7, x8) =
                  deepseq x1 `seq`
                  deepseq x2 `seq`
                  deepseq x3 `seq`
                  deepseq x4 `seq`
                  deepseq x5 `seq`
                  deepseq x6 `seq`
                  deepseq x7 `seq`
                  deepseq x8

instance (DeepSeq a1, DeepSeq a2, DeepSeq a3, DeepSeq a4, DeepSeq a5, DeepSeq a6, DeepSeq a7, DeepSeq a8, DeepSeq a9) => 
         DeepSeq (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  deepseq (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
                  deepseq x1 `seq`
                  deepseq x2 `seq`
                  deepseq x3 `seq`
                  deepseq x4 `seq`
                  deepseq x5 `seq`
                  deepseq x6 `seq`
                  deepseq x7 `seq`
                  deepseq x8 `seq`
                  deepseq x9
