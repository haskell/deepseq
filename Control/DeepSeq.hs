{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 811 && __GLASGOW_HASKELL__ < 901
-- For the Option instance (https://gitlab.haskell.org/ghc/ghc/issues/15028)
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

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
-- This module provides overloaded functions, such as 'deepseq' and
-- 'rnf', for fully evaluating data structures (that is, evaluating to
-- \"Normal Form\").
--
-- A typical use is to prevent resource leaks in lazy IO programs, by
-- forcing all characters from a file to be read. For example:
--
-- > import System.IO
-- > import Control.DeepSeq
-- > import Control.Exception (evaluate)
-- >
-- > readFile' :: FilePath -> IO String
-- > readFile' fn = do
-- >     h <- openFile fn ReadMode
-- >     s <- hGetContents h
-- >     evaluate (rnf s)
-- >     hClose h
-- >     return s
--
-- __Note__: The example above should rather be written in terms of
-- 'Control.Exception.bracket' to ensure releasing file-descriptors in
-- a timely matter (see the description of 'force' for an example).
--
-- 'deepseq' differs from 'seq' as it traverses data structures deeply,
-- for example, 'seq' will evaluate only to the first constructor in
-- the list:
--
-- > > [1,2,undefined] `seq` 3
-- > 3
--
-- While 'deepseq' will force evaluation of all the list elements:
--
-- > > [1,2,undefined] `deepseq` 3
-- > *** Exception: Prelude.undefined
--
-- Another common use is to ensure any exceptions hidden within lazy
-- fields of a data structure do not leak outside the scope of the
-- exception handler, or to force evaluation of a data structure in one
-- thread, before passing to another thread (preventing work moving to
-- the wrong threads).
--
-- @since 1.1.0.0
module Control.DeepSeq (
  -- * 'NFData' class
  NFData (rnf),

  -- * Helper functions
  deepseq,
  force,
  ($!!),
  (<$!!>),
  rwhnf,

  -- * Liftings of the 'NFData' class

  -- ** For unary constructors
  NFData1 (liftRnf),
  rnf1,

  -- ** For binary constructors
  NFData2 (liftRnf2),
  rnf2,
) where

import Control.Applicative
import Control.Concurrent (MVar, ThreadId)
import Control.Exception (MaskingState (..))
import Data.Array
import Data.Complex
import Data.Fixed
import Data.Functor.Compose
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.IORef
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid as Mon
import Data.Ord (Down (Down))
import Data.Proxy (Proxy (Proxy))
import Data.Ratio
import Data.STRef
import Data.Semigroup as Semi
import Data.Type.Equality ((:~:), (:~~:))
import Data.Typeable (TyCon, TypeRep, rnfTyCon, rnfTypeRep)
import Data.Unique (Unique)
import Data.Version
import Data.Void (Void, absurd)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Fingerprint.Type (Fingerprint (..))
import GHC.Generics
import GHC.Stack.Types (CallStack (..), SrcLoc (..))
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))
import System.Mem.StableName (StableName)
import qualified Type.Reflection as Reflection

#ifdef MIN_VERSION_ghc_prim
#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo (..))
#endif
#endif

#if MIN_VERSION_base(4,17,0)
import Data.Array.Byte (ByteArray(..), MutableByteArray(..))
#endif

-- | Hidden internal type-class
class GNFData arity f where
  grnf :: RnfArgs arity a -> f a -> ()

instance GNFData arity V1 where
  grnf _ x = case x of {}

data Zero

data One

data family RnfArgs arity a

data instance RnfArgs Zero a = RnfArgs0

newtype instance RnfArgs One a = RnfArgs1 (a -> ())

instance GNFData arity U1 where
  grnf _ U1 = ()

instance NFData a => GNFData arity (K1 i a) where
  grnf _ = rnf . unK1
  {-# INLINEABLE grnf #-}

instance GNFData arity a => GNFData arity (M1 i c a) where
  grnf args = grnf args . unM1
  {-# INLINEABLE grnf #-}

instance GNFData arity (URec a) where
  grnf _ = rwhnf -- Every URec data instance consists of a single data
  -- constructor containing a single strict field, so reducing
  -- any URec instance to WHNF suffices to reduce it to NF.
  {-# INLINEABLE grnf #-}

instance (GNFData arity a, GNFData arity b) => GNFData arity (a :*: b) where
  grnf args (x :*: y) = grnf args x `seq` grnf args y
  {-# INLINEABLE grnf #-}

instance (GNFData arity a, GNFData arity b) => GNFData arity (a :+: b) where
  grnf args (L1 x) = grnf args x
  grnf args (R1 x) = grnf args x
  {-# INLINEABLE grnf #-}

instance GNFData One Par1 where
  grnf (RnfArgs1 r) = r . unPar1

instance NFData1 f => GNFData One (Rec1 f) where
  grnf (RnfArgs1 r) = liftRnf r . unRec1

instance (NFData1 f, GNFData One g) => GNFData One (f :.: g) where
  grnf args = liftRnf (grnf args) . unComp1

infixr 0 $!!

infixr 0 `deepseq`

-- | 'deepseq': fully evaluates the first argument, before returning the
-- second.
--
-- The name 'deepseq' is used to illustrate the relationship to 'seq':
-- where 'seq' is shallow in the sense that it only evaluates the top
-- level of its argument, 'deepseq' traverses the entire data structure
-- evaluating it completely.
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
-- @since 1.1.0.0
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

-- | the deep analogue of '$!'.  In the expression @f $!! x@, @x@ is
-- fully evaluated before the function @f@ is applied to it.
--
-- @since 1.2.0.0
($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

-- | a variant of 'deepseq' that is useful in some circumstances:
--
-- > force x = x `deepseq` x
--
-- @force x@ fully evaluates @x@, and then returns it.  Note that
-- @force x@ only performs evaluation when the value of @force x@
-- itself is demanded, so essentially it turns shallow evaluation into
-- deep evaluation.
--
-- 'force' can be conveniently used in combination with @ViewPatterns@:
--
-- > {-# LANGUAGE BangPatterns, ViewPatterns #-}
-- > import Control.DeepSeq
-- >
-- > someFun :: ComplexData -> SomeResult
-- > someFun (force -> !arg) = {- 'arg' will be fully evaluated -}
--
-- Another useful application is to combine 'force' with
-- 'Control.Exception.evaluate' in order to force deep evaluation
-- relative to other 'IO' operations:
--
-- > import Control.Exception (evaluate)
-- > import Control.DeepSeq
-- >
-- > main = do
-- >   result <- evaluate $ force $ pureComputation
-- >   {- 'result' will be fully evaluated at this point -}
-- >   return ()
--
-- Finally, here's an exception safe variant of the @readFile'@ example:
--
-- > readFile' :: FilePath -> IO String
-- > readFile' fn = bracket (openFile fn ReadMode) hClose $ \h ->
-- >                        evaluate . force =<< hGetContents h
--
-- @since 1.2.0.0
force :: (NFData a) => a -> a
force x = x `deepseq` x

-- | Deeply strict version of 'Control.Applicative.<$>'.
--
-- @since 1.4.3.0
(<$!!>) :: (Monad m, NFData b) => (a -> b) -> m a -> m b
f <$!!> m = m >>= \x -> pure $!! f x

infixl 4 <$!!>

-- | Reduce to weak head normal form
--
-- Equivalent to @\\x -> 'seq' x ()@.
--
-- Useful for defining 'NFData' for types for which NF=WHNF holds.
--
-- > data T = C1 | C2 | C3
-- > instance NFData T where rnf = rwhnf
--
-- @since 1.4.3.0
rwhnf :: a -> ()
rwhnf = (`seq` ())
{-# INLINE rwhnf #-}

-- Note: the 'rwhnf' is defined point-free to help aggressive inlining

-- | A class of types that can be fully evaluated.
--
-- @since 1.1.0.0
class NFData a where
  -- | 'rnf' should reduce its argument to normal form (that is, fully
  -- evaluate all sub-components), and then return '()'.
  --
  -- === 'Generic' 'NFData' deriving
  --
  -- Starting with GHC 7.2, you can automatically derive instances
  -- for types possessing a 'Generic' instance.
  --
  -- Note: 'Generic1' can be auto-derived starting with GHC 7.4
  --
  -- > {-# LANGUAGE DeriveGeneric #-}
  -- >
  -- > import GHC.Generics (Generic, Generic1)
  -- > import Control.DeepSeq
  -- >
  -- > data Foo a = Foo a String
  -- >              deriving (Eq, Generic, Generic1)
  -- >
  -- > instance NFData a => NFData (Foo a)
  -- > instance NFData1 Foo
  -- >
  -- > data Colour = Red | Green | Blue
  -- >               deriving Generic
  -- >
  -- > instance NFData Colour
  --
  -- Starting with GHC 7.10, the example above can be written more
  -- concisely by enabling the new @DeriveAnyClass@ extension:
  --
  -- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
  -- >
  -- > import GHC.Generics (Generic)
  -- > import Control.DeepSeq
  -- >
  -- > data Foo a = Foo a String
  -- >              deriving (Eq, Generic, Generic1, NFData, NFData1)
  -- >
  -- > data Colour = Red | Green | Blue
  -- >               deriving (Generic, NFData)
  -- >
  --
  -- === Compatibility with previous @deepseq@ versions
  --
  -- Prior to version 1.4.0.0, the default implementation of the 'rnf'
  -- method was defined as
  --
  -- @'rnf' a = 'seq' a ()@
  --
  -- However, starting with @deepseq-1.4.0.0@, the default
  -- implementation is based on @DefaultSignatures@ allowing for
  -- more accurate auto-derived 'NFData' instances. If you need the
  -- previously used exact default 'rnf' method implementation
  -- semantics, use
  --
  -- > instance NFData Colour where rnf x = seq x ()
  --
  -- or alternatively
  --
  -- > instance NFData Colour where rnf = rwhnf
  --
  -- or
  --
  -- > {-# LANGUAGE BangPatterns #-}
  -- > instance NFData Colour where rnf !_ = ()
  rnf :: a -> ()
  default rnf :: (Generic a, GNFData Zero (Rep a)) => a -> ()
  rnf = grnf RnfArgs0 . from

-- | A class of functors that can be fully evaluated.
--
-- In `deepseq-1.5.0.0` this class was updated to include superclasses.
--
-- @since 1.4.3.0
class (forall a. NFData a => NFData (f a)) => NFData1 f where
  -- | 'liftRnf' should reduce its argument to normal form (that is, fully
  -- evaluate all sub-components), given an argument to reduce @a@ arguments,
  -- and then return '()'.
  --
  -- See 'rnf' for the generic deriving.
  liftRnf :: (a -> ()) -> f a -> ()
  default liftRnf :: (Generic1 f, GNFData One (Rep1 f)) => (a -> ()) -> f a -> ()
  liftRnf r = grnf (RnfArgs1 r) . from1

-- | Lift the standard 'rnf' function through the type constructor.
--
-- @since 1.4.3.0
rnf1 :: (NFData1 f, NFData a) => f a -> ()
rnf1 = liftRnf rnf

-- | A class of bifunctors that can be fully evaluated.
--
-- In `deepseq-1.5.0.0` this class was updated to include superclasses.
--
-- @since 1.4.3.0
class (forall a. NFData a => NFData1 (p a)) => NFData2 p where
  -- | 'liftRnf2' should reduce its argument to normal form (that
  -- is, fully evaluate all sub-components), given functions to
  -- reduce @a@ and @b@ arguments respectively, and then return '()'.
  --
  -- __Note__: Unlike for the unary 'liftRnf', there is currently no
  -- support for generically deriving 'liftRnf2'.
  liftRnf2 :: (a -> ()) -> (b -> ()) -> p a b -> ()

-- | Lift the standard 'rnf' function through the type constructor.
--
-- @since 1.4.3.0
rnf2 :: (NFData2 p, NFData a, NFData b) => p a b -> ()
rnf2 = liftRnf2 rnf rnf

instance NFData Int where rnf = rwhnf

instance NFData Word where rnf = rwhnf

instance NFData Integer where rnf = rwhnf

instance NFData Float where rnf = rwhnf

instance NFData Double where rnf = rwhnf

instance NFData Char where rnf = rwhnf

instance NFData Bool where rnf = rwhnf

instance NFData Ordering where rnf = rwhnf

instance NFData () where rnf = rwhnf

instance NFData Int8 where rnf = rwhnf

instance NFData Int16 where rnf = rwhnf

instance NFData Int32 where rnf = rwhnf

instance NFData Int64 where rnf = rwhnf

instance NFData Word8 where rnf = rwhnf

instance NFData Word16 where rnf = rwhnf

instance NFData Word32 where rnf = rwhnf

instance NFData Word64 where rnf = rwhnf

-- | @since 1.4.4.0
instance NFData MaskingState where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData (Proxy a) where rnf Proxy = ()

-- | @since 1.4.3.0
instance NFData1 Proxy where liftRnf _ Proxy = ()

-- | @since 1.4.3.0
instance NFData (a :~: b) where rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 ((:~:) a) where liftRnf _ = rwhnf

-- | @since 1.4.3.0
instance NFData2 (:~:) where liftRnf2 _ _ = rwhnf

-- | @since 1.4.3.0
instance NFData (a :~~: b) where rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 ((:~~:) a) where liftRnf _ = rwhnf

-- | @since 1.4.3.0
instance NFData2 (:~~:) where liftRnf2 _ _ = rwhnf

-- | @since 1.4.0.0
instance NFData a => NFData (Identity a) where
  rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Identity where
  liftRnf r = r . runIdentity

-- | Defined as @'rnf' = 'absurd'@.
--
-- @since 1.4.0.0
instance NFData Void where
  rnf = absurd

-- | @since 1.4.0.0
instance NFData Natural where rnf = rwhnf

-- | @since 1.3.0.0
instance NFData (Fixed a) where rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 Fixed where liftRnf _ = rwhnf

-- | This instance is for convenience and consistency with 'seq'.
--  This assumes that WHNF is equivalent to NF for functions.
--
--  @since 1.3.0.0
instance NFData (a -> b) where rnf = rwhnf

-- Rational and complex numbers.

-- | Available on @base >=4.9@
--
-- @since 1.4.3.0
instance NFData1 Ratio where
  liftRnf r x = r (numerator x) `seq` r (denominator x)

-- | @since 1.4.3.0
instance (NFData1 f, NFData1 g) => NFData1 (Compose f g) where
  liftRnf r = liftRnf (liftRnf r) . getCompose

-- | Note: in @deepseq-1.5.0.0@ this instance's superclasses were changed.
-- 
-- @since 1.4.3.0
instance (NFData (f (g a))) => NFData (Compose f g a) where
  rnf (Compose fga) = rnf fga

-- | @since 1.4.3.0
instance (NFData1 f, NFData1 g) => NFData1 (Functor.Sum f g) where
  liftRnf rnf0 (Functor.InL l) = liftRnf rnf0 l
  liftRnf rnf0 (Functor.InR r) = liftRnf rnf0 r

-- | Note: in @deepseq-1.5.0.0@ this instance's superclasses were changed.
-- 
-- @since 1.4.3.0
instance (NFData (f a), NFData (g a)) => NFData (Functor.Sum f g a) where
  rnf (Functor.InL fa) = rnf fa
  rnf (Functor.InR ga) = rnf ga

-- | @since 1.4.3.0
instance (NFData1 f, NFData1 g) => NFData1 (Functor.Product f g) where
  liftRnf rnf0 (Functor.Pair f g) = liftRnf rnf0 f `seq` liftRnf rnf0 g

-- | Note: in @deepseq-1.5.0.0@ this instance's superclasses were changed.
-- 
-- @since 1.4.3.0
instance (NFData (f a), NFData (g a)) => NFData (Functor.Product f g a) where
  rnf (Functor.Pair fa ga) = rnf fa `seq` rnf ga

instance NFData a => NFData (Ratio a) where
  rnf x = rnf (numerator x, denominator x)

instance (NFData a) => NFData (Complex a) where
  rnf (x :+ y) = rnf x `seq` rnf y `seq` ()

instance NFData a => NFData (Maybe a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Maybe where
  liftRnf _r Nothing = ()
  liftRnf r (Just x) = r x

instance (NFData a, NFData b) => NFData (Either a b) where rnf = rnf1

-- | @since 1.4.3.0
instance (NFData a) => NFData1 (Either a) where liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance NFData2 Either where
  liftRnf2 l _r (Left x) = l x
  liftRnf2 _l r (Right y) = r y

-- | @since 1.3.0.0
instance NFData Data.Version.Version where
  rnf (Data.Version.Version branch tags) = rnf branch `seq` rnf tags

instance NFData a => NFData [a] where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 [] where
  liftRnf f = foldr (\x r -> f x `seq` r) ()
  {-# INLINABLE liftRnf #-}

-- | @since 1.4.0.0
instance NFData a => NFData (ZipList a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 ZipList where
  liftRnf r = liftRnf r . getZipList

-- | @since 1.4.0.0
instance NFData a => NFData (Const a b) where
  rnf = rnf . getConst

-- | @since 1.4.3.0
instance NFData a => NFData1 (Const a) where
  liftRnf _ = rnf . getConst

-- | @since 1.4.3.0
instance NFData2 Const where
  liftRnf2 r _ = r . getConst

-- We should use MIN_VERSION array(0,5,1,1) but that's not possible.
-- There isn't an underscore to not break C preprocessor
instance (NFData a, NFData b) => NFData (Array a b) where
  rnf x = rnf (bounds x, Data.Array.elems x)

-- | @since 1.4.3.0
instance (NFData a) => NFData1 (Array a) where
  liftRnf r x = rnf (bounds x) `seq` liftRnf r (Data.Array.elems x)

-- | @since 1.4.3.0
instance NFData2 Array where
  liftRnf2 r r' x = liftRnf2 r r (bounds x) `seq` liftRnf r' (Data.Array.elems x)

-- | @since 1.4.0.0
instance NFData a => NFData (Down a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Down where
  liftRnf r (Down x) = r x

-- | @since 1.4.0.0
instance NFData a => NFData (Dual a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Dual where
  liftRnf r (Dual x) = r x

-- | @since 1.4.0.0
instance NFData a => NFData (Mon.First a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Mon.First where
  liftRnf r (Mon.First x) = liftRnf r x

-- | @since 1.4.0.0
instance NFData a => NFData (Mon.Last a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Mon.Last where
  liftRnf r (Mon.Last x) = liftRnf r x

-- | @since 1.4.0.0
instance NFData Any where rnf = rnf . getAny

-- | @since 1.4.0.0
instance NFData All where rnf = rnf . getAll

-- | @since 1.4.0.0
instance NFData a => NFData (Sum a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Sum where
  liftRnf r (Sum x) = r x

-- | @since 1.4.0.0
instance NFData a => NFData (Product a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Product where
  liftRnf r (Product x) = r x

-- | @since 1.4.0.0
instance NFData (StableName a) where
  rnf = rwhnf -- assumes `data StableName a = StableName (StableName# a)`

-- | @since 1.4.3.0
instance NFData1 StableName where
  liftRnf _ = rwhnf

-- | @since 1.4.0.0
instance NFData ThreadId where
  rnf = rwhnf -- assumes `data ThreadId = ThreadId ThreadId#`

-- | @since 1.4.0.0
instance NFData Unique where
  rnf = rwhnf -- assumes `newtype Unique = Unique Integer`

-- | __NOTE__: Prior to @deepseq-1.4.4.0@ this instance was only defined for @base-4.8.0.0@ and later.
--
-- @since 1.4.0.0
instance NFData TypeRep where
  rnf tyrep = rnfTypeRep tyrep

-- | __NOTE__: Prior to @deepseq-1.4.4.0@ this instance was only defined for @base-4.8.0.0@ and later.
--
-- @since 1.4.0.0
instance NFData TyCon where
  rnf tycon = rnfTyCon tycon

-- | @since 1.4.8.0
instance NFData (Reflection.TypeRep a) where
  rnf tr = Reflection.rnfTypeRep tr

-- | @since 1.4.8.0
instance NFData Reflection.Module where
  rnf modul = Reflection.rnfModule modul

-- | __NOTE__: Only strict in the reference and not the referenced value.
--
-- @since 1.4.2.0
instance NFData (IORef a) where
  rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 IORef where
  liftRnf _ = rwhnf

-- | __NOTE__: Only strict in the reference and not the referenced value.
--
-- @since 1.4.2.0
instance NFData (STRef s a) where
  rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 (STRef s) where
  liftRnf _ = rwhnf

-- | @since 1.4.3.0
instance NFData2 STRef where
  liftRnf2 _ _ = rwhnf

-- | __NOTE__: Only strict in the reference and not the referenced value.
--
-- @since 1.4.2.0
instance NFData (MVar a) where
  rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 MVar where
  liftRnf _ = rwhnf

----------------------------------------------------------------------------
-- GHC Specifics

-- | @since 1.4.0.0
instance NFData Fingerprint where
  rnf (Fingerprint _ _) = ()

----------------------------------------------------------------------------
-- Foreign.Ptr

-- | @since 1.4.2.0
instance NFData (Ptr a) where
  rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 Ptr where
  liftRnf _ = rwhnf

-- | @since 1.4.2.0
instance NFData (FunPtr a) where
  rnf = rwhnf

-- | @since 1.4.3.0
instance NFData1 FunPtr where
  liftRnf _ = rwhnf

----------------------------------------------------------------------------
-- Foreign.C.Types

-- | @since 1.4.0.0
instance NFData CChar where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CSChar where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUChar where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CShort where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUShort where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CInt where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUInt where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CLong where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CULong where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CPtrdiff where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CSize where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CWchar where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CSigAtomic where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CLLong where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CULLong where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CIntPtr where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUIntPtr where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CIntMax where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUIntMax where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CClock where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CTime where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CUSeconds where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CSUSeconds where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CFloat where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CDouble where rnf = rwhnf

-- NOTE: The types `CFile`, `CFPos`, and `CJmpBuf` below are not
-- newtype wrappers rather defined as field-less single-constructor
-- types.

-- | @since 1.4.0.0
instance NFData CFile where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CFpos where rnf = rwhnf

-- | @since 1.4.0.0
instance NFData CJmpBuf where rnf = rwhnf

-- | @since 1.4.3.0
instance NFData CBool where rnf = rwhnf

----------------------------------------------------------------------------
-- System.Exit

-- | @since 1.4.2.0
instance NFData ExitCode where
  rnf (ExitFailure n) = rnf n
  rnf ExitSuccess = ()

----------------------------------------------------------------------------
-- instances previously provided by semigroups package

-- | @since 1.4.2.0
instance NFData a => NFData (NonEmpty a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 NonEmpty where
  liftRnf r (x :| xs) = r x `seq` liftRnf r xs

-- | @since 1.4.2.0
instance NFData a => NFData (Min a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Min where
  liftRnf r (Min a) = r a

-- | @since 1.4.2.0
instance NFData a => NFData (Max a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Max where
  liftRnf r (Max a) = r a

-- | @since 1.4.2.0
instance (NFData a, NFData b) => NFData (Arg a b) where rnf = rnf2

-- | @since 1.4.3.0
instance (NFData a) => NFData1 (Arg a) where liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance NFData2 Arg where
  liftRnf2 r r' (Arg a b) = r a `seq` r' b `seq` ()

-- | @since 1.4.2.0
instance NFData a => NFData (Semi.First a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Semi.First where
  liftRnf r (Semi.First a) = r a

-- | @since 1.4.2.0
instance NFData a => NFData (Semi.Last a) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 Semi.Last where
  liftRnf r (Semi.Last a) = r a

-- | @since 1.4.2.0
instance NFData m => NFData (WrappedMonoid m) where rnf = rnf1

-- | @since 1.4.3.0
instance NFData1 WrappedMonoid where
  liftRnf r (WrapMonoid a) = r a

#if __GLASGOW_HASKELL__ < 901
-- |@since 1.4.2.0
instance NFData a => NFData (Option a) where rnf = rnf1
-- |@since 1.4.3.0
instance NFData1 Option where
  liftRnf r (Option a) = liftRnf r a
#endif

----------------------------------------------------------------------------
-- GHC.Stack

-- | @since 1.4.2.0
instance NFData SrcLoc where
  rnf (SrcLoc a b c d e f g) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g

-- | @since 1.4.2.0
instance NFData CallStack where
  rnf EmptyCallStack = ()
  rnf (PushCallStack a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (FreezeCallStack a) = rnf a

----------------------------------------------------------------------------
-- Tuples

#ifdef MIN_VERSION_ghc_prim
#if MIN_VERSION_ghc_prim(0,7,0)
-- |@since 1.4.6.0
instance NFData a => NFData (Solo a) where
#if MIN_VERSION_ghc_prim(0,10,0)
  rnf (MkSolo a) = rnf a
#else
  rnf (Solo a) = rnf a
#endif
-- |@since 1.4.6.0
instance NFData1 Solo where
#if MIN_VERSION_ghc_prim(0,10,0)
  liftRnf r (MkSolo a) = r a
#else
  liftRnf r (Solo a) = r a
#endif
#endif
#endif

instance (NFData a, NFData b) => NFData (a, b) where rnf = rnf2

-- | @since 1.4.3.0
instance (NFData a) => NFData1 ((,) a) where liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance NFData2 (,) where
  liftRnf2 r r' (x, y) = r x `seq` r' y

-- Code below is generated, see generate-nfdata-tuple.hs
instance
  (NFData a1, NFData a2, NFData a3) =>
  NFData (a1, a2, a3)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2) =>
  NFData1 ((,,) a1 a2)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1) =>
  NFData2 ((,,) a1)
  where
  liftRnf2 r r' (x1, x2, x3) =
    rnf x1 `seq` r x2 `seq` r' x3

instance
  (NFData a1, NFData a2, NFData a3, NFData a4) =>
  NFData (a1, a2, a3, a4)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3) =>
  NFData1 ((,,,) a1 a2 a3)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2) =>
  NFData2 ((,,,) a1 a2)
  where
  liftRnf2 r r' (x1, x2, x3, x4) =
    rnf x1 `seq` rnf x2 `seq` r x3 `seq` r' x4

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) =>
  NFData (a1, a2, a3, a4, a5)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4) =>
  NFData1 ((,,,,) a1 a2 a3 a4)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3) =>
  NFData2 ((,,,,) a1 a2 a3)
  where
  liftRnf2 r r' (x1, x2, x3, x4, x5) =
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` r x4 `seq` r' x5

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) =>
  NFData (a1, a2, a3, a4, a5, a6)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) =>
  NFData1 ((,,,,,) a1 a2 a3 a4 a5)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4) =>
  NFData2 ((,,,,,) a1 a2 a3 a4)
  where
  liftRnf2 r r' (x1, x2, x3, x4, x5, x6) =
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` r x5 `seq` r' x6

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) =>
  NFData (a1, a2, a3, a4, a5, a6, a7)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) =>
  NFData1 ((,,,,,,) a1 a2 a3 a4 a5 a6)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) =>
  NFData2 ((,,,,,,) a1 a2 a3 a4 a5)
  where
  liftRnf2 r r' (x1, x2, x3, x4, x5, x6, x7) =
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` r x6 `seq` r' x7

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8) =>
  NFData (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) =>
  NFData1 ((,,,,,,,) a1 a2 a3 a4 a5 a6 a7)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) =>
  NFData2 ((,,,,,,,) a1 a2 a3 a4 a5 a6)
  where
  liftRnf2 r r' (x1, x2, x3, x4, x5, x6, x7, x8) =
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` r x7 `seq` r' x8

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9) =>
  NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  rnf = rnf2

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8) =>
  NFData1 ((,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8)
  where
  liftRnf = liftRnf2 rnf

-- | @since 1.4.3.0
instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) =>
  NFData2 ((,,,,,,,,) a1 a2 a3 a4 a5 a6 a7)
  where
  liftRnf2 r r' (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` r x8 `seq` r' x9

----------------------------------------------------------------------------
-- ByteArray

#if MIN_VERSION_base(4,17,0)
-- |@since 1.4.7.0
instance NFData ByteArray where
  rnf (ByteArray _) = ()

-- |@since 1.4.8.0
instance NFData (MutableByteArray s) where
  rnf (MutableByteArray _) = ()
#endif
