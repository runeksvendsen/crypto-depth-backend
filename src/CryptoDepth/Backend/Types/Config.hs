{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module CryptoDepth.Backend.Types.Config
( Config(..)
, PoolConfig(..)
, mkConfig
)
where

import Protolude                            (toS)
import qualified Data.Pool                  as Pool
import Database.PostgreSQL.Simple           (Connection, connectPostgreSQL, close)
import Data.Time.Clock                      (NominalDiffTime)


data Config
   = Config
   { cfgPool            :: Pool.Pool Connection
   }

-- | See documentation for 'createPool'
data PoolConfig
    = PoolConfig
    { dbConnStr         :: String
    , numStripes        :: Int              -- ^ Number of striped
    , numConnPerStripe  :: Int              -- ^ Number of connections per stripe
    , keepOpenTimeout   :: NominalDiffTime  -- ^ How long to keep idle connections open
    }

mkConfig :: PoolConfig -> IO Config
mkConfig PoolConfig{..} = Config <$>
    Pool.createPool
        (connectPostgreSQL $ toS dbConnStr)
        close
        numStripes
        keepOpenTimeout
        numConnPerStripe
