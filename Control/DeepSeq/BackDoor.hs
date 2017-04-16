{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,6,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- | Hack to keep Control.DeepSeq SAFE-inferred
module Control.DeepSeq.BackDoor
    ( module X
    ) where

#if MIN_VERSION_base(4,6,0)
import Data.Ord as X ( Down(Down) )
#else
import GHC.Exts as X ( Down(Down) )
#endif
