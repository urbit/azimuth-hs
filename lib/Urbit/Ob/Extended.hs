{-# LANGUAGE DataKinds #-}

module Urbit.Ob.Extended (
    patpToPoint
  , patpToGalaxy
  , pointToPatp
  ) where

import qualified Data.Solidity.Prim
import qualified Urbit.Ob as Ob

data ObError = InvalidClass Ob.Class

-- | Convert a @p value to an Azimuth point.
--
--   Note that moon, comet, and higher-byte @p values will silently overflow on
--   conversion to a 32-bit Azimuth point!
patpToPoint :: Ob.Patp -> Data.Solidity.Prim.UIntN 32
patpToPoint = fromIntegral . Ob.fromPatp

-- | Convert a galaxy-class @p value to an Azimuth point.
patpToGalaxy :: Ob.Patp -> Either ObError (Data.Solidity.Prim.UIntN 8)
patpToGalaxy patp = case clan of
    Ob.Galaxy -> pure (fromIntegral (Ob.fromPatp patp))
    _         -> Left (InvalidClass clan)
  where
    clan = Ob.clan patp

-- | Convert an Azimuth point to a @p value.
pointToPatp :: Data.Solidity.Prim.UIntN 32 -> Ob.Patp
pointToPatp = Ob.patp . fromIntegral

