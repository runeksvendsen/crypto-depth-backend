{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module CryptoDepth.Backend.Types.AppM
( AppM
, runAppM
, Config
, PoolConfig(..)
, mkConfig
, runDb
)
where

import Protolude                            (toS)
import Control.Monad.Reader                 ( ReaderT, MonadReader, MonadIO
                                            , liftIO, runReaderT, asks
                                            )
import Control.Monad.Except                 (ExceptT, MonadError)
import qualified Data.Pool                  as Pool
import Database.PostgreSQL.Simple           (Connection, connectPostgreSQL, close)
import Servant.Server                       (Handler, ServantErr)
import Data.Time.Clock                      (NominalDiffTime)
import qualified Database.Beam              as Beam
import Database.Beam.Postgres               (Pg)


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

newtype AppM a = AppM
    { getAppM :: ReaderT Config Handler a }
        deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadError ServantErr
        , MonadReader Config
        )

runAppM :: Config -> AppM a -> Handler a
runAppM cfg appM =
    runReaderT (getAppM appM) cfg

runDb :: Pg a -> AppM a
runDb pg = do
    pool <- asks cfgPool
    liftIO . Pool.withResource pool $ \conn ->
        Beam.withDatabase conn pg
