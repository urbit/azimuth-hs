{-# LANGUAGE DataKinds #-}

module Urbit.Ob.Extended (
    patpToPoint
  , pointToPatp
  ) where

import qualified Data.Solidity.Prim
import qualified Urbit.Ob

-- | Convert a @p value to an Azimuth point.
--
--   Note that moon, comet, and higher-byte @p values will silently overflow on
--   conversion to a 32-bit Azimuth point!
patpToPoint :: Urbit.Ob.Patp -> Data.Solidity.Prim.UIntN 32
patpToPoint = fromIntegral . Urbit.Ob.fromPatp

-- | Convert an Azimuth point to a @p value.
pointToPatp :: Data.Solidity.Prim.UIntN 32 -> Urbit.Ob.Patp
pointToPatp = Urbit.Ob.patp . fromIntegral

