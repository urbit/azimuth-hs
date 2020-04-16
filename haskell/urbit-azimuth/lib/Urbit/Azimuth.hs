module Urbit.Azimuth
    (

    ) where

-- import Numeric.Natural (Natural)

import qualified Urbit.Ob
import qualified Network.Ethereum.Api.Types as Ethereum.Types
import qualified Urbit.Azimuth.Contract as Contract

newtype Address = Address Natural
    deriving stock (Show, Read, Eq, Ord, Num, Real, Integral)

data Point = Point
    { encryptionKey     :: Int
    , authenticationKey :: Int
    , hasSponsor        :: Int
    , active            :: Int
    , escapeRequested   :: Int
    , sponsor           :: Int
    , escapeTo          :: Int
    , cryptoSuite       :: Int
    , keyRevision       :: Int
    , continuityNum     :: Int
    } deriving stock (Show, Eq)

-- data EthPoint = EthPoint
--     { epOwn :: (EthAddr, EthAddr, EthAddr, EthAddr)
--     , epNet :: Maybe (Life, Pass, ContNum, (Bool, Ship), Maybe Ship)
--     , epKid :: Maybe (EthAddr, HoonSet Ship)
--     }
--   deriving (Eq, Show)

getPoint point = do
    ( encryptionKey
    , authenticationKey
    , hasSponsor
    , active
    , escapeRequested
    , sponsor
    , escapeTo
    , cryptoSuite
    , keyRevision
    , continuityNum
    ) <- Contract.points point

    pure Point{..}
