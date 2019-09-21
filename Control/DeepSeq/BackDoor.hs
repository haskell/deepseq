{-# LANGUAGE CPP #-}

-- | Hack to keep Control.DeepSeq SAFE-inferred
--
-- This module only re-export reasonably safe entities from non-safe
-- modules when there is no safe alternative

{-# LANGUAGE Trustworthy #-}

module Control.DeepSeq.BackDoor
    ( module X
    ) where

#if !(MIN_VERSION_base(4,6,0))
-- not safe, but Down is in Data.Ord starting with base-4.6
import GHC.Exts as X ( Down(Down) )
#endif

#if MIN_VERSION_base(4,9,0)
-- Data.Type.Equality present from base-4.7, trustworthy starting with base-4.9
#elif MIN_VERSION_base(4,7,0)
import Data.Type.Equality as X ( (:~:) )
#endif

#if MIN_VERSION_base(4,0,0)
-- present since at least base-4.0, maybe sooner?
import GHC.Conc as X ( TVar(TVar) )
#endif
