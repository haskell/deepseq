# Changelog for [`deepseq` package](http://hackage.haskell.org/package/deepseq)

## 1.4.1.0  *TBA*

  * Drop redundant constraints from a few `NFData` instances (if
    possible for a given `base` version)

## 1.4.0.0  *Dec 2014*

  * Bundled with GHC 7.10.1
  * Switch to Generics based `DefaultSignature` `rnf` method
    implementation (based on code from `deepseq-generics`)

    **Compatibility Note**: if you need the exact default-method
    semantics of `deepseq` prior to 1.4, replace occurences of

        instance NFData XYZ

    by

        instance NFData XYZ where rnf x = seq x ()

  * New `NFData` instances for `base` types:

     - `Control.Applicative.Const`
     - `Control.Applicative.ZipList`
     - `Control.Concurrent.ThreadId`
     - `Data.Functor.Identity.Identity`
     - `Data.Monoid.{Dual,First,Last,Any,All,Sum,Product}`
     - `Data.Ord.Down`
     - `Data.Proxy.Proxy`
     - `Data.Typeable.Internal.TyCon`
     - `Data.Typeable.Internal.TypeRep`
     - `Data.Unique.Unique`
     - `Data.Void.Void`
     - `GHC.Fingerprint.Type.Fingerprint`
     - `Numeric.Natural.Natural`
     - `System.Mem.StableName.StableName`
     - `Foreign.C.Types.C*`

## 1.3.0.2  *Nov 2013*

  * Bundled with GHC 7.8.1
  * Update package description to Cabal 1.10 format
  * Add support for GHC 7.8
  * Drop support for GHCs older than GHC 7.0.1
  * Add `/since: .../` annotations to Haddock comments
  * Add changelog

## 1.3.0.1  *Sep 2012*

  * No changes

## 1.3.0.0  *Feb 2012*

  * Add instances for `Fixed`, `a->b` and `Version`

## 1.2.0.1  *Sep 2011*

  * Disable SafeHaskell for GHC 7.2

## 1.2.0.0  *Sep 2011*

  * New function `force`
  * New operator `$!!`
  * Add SafeHaskell support
  * Dropped dependency on containers

## 1.1.0.2  *Nov 2010*

  * Improve Haddock documentation

## 1.1.0.1  *Oct 2010*

  * Enable support for containers-0.4.x

## 1.1.0.0  *Nov 2009*

  * Major rewrite

## 1.0.0.0  *Nov 2009*

  * Initial release
