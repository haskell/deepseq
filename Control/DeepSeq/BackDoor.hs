{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- | Hack to keep Control.DeepSeq SAFE-inferred
module Control.DeepSeq.BackDoor
    ( module X
    ) where

#if MIN_VERSION_base(4,6,0)
-- SAFE
import Data.Ord as X ( Down(Down) )
#else
-- not SAFE
import GHC.Exts as X ( Down(Down) )
#endif

#if MIN_VERSION_base(4,7,0)
-- Data.Type.Equality wasn't SAFE before base-4.10
import Data.Type.Equality as X ( (:~:) )
#endif
