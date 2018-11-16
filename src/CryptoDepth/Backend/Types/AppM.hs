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
import CryptoDepth.Backend.Types.Config     as Config
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
