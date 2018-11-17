{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module CryptoDepth.Backend
( runApp
, app
, PoolConfig(..)
) where

import CryptoDepth.Backend.Internal.Prelude
import CryptoDepth.Backend.Types.AppM                       (PoolConfig(..))
import CryptoDepth.Backend.Types.Config                     (Config(cfgPool))
import CryptoDepth.Backend.Api
import qualified CryptoDepth.Backend.Handler.Numeraires
import qualified CryptoDepth.Backend.Handler.PathSums
import qualified CryptoDepth.Backend.Handler.Paths
import qualified CryptoDepth.Db.Internal.Migrate.Run        as Db

import qualified Network.Wai.Handler.Warp                   as Warp
import Servant
import qualified Data.Pool                                  as Pool


runApp :: Int -> PoolConfig -> IO ()
runApp port poolCfg = do
    cfg <- mkConfig poolCfg
    -- Assert that the database contains the expected schema
    Pool.withResource (cfgPool cfg) Db.assertTables
    Warp.run port (app cfg)

app :: Config -> Application
app cfg = serve api $ hoistServer api (runAppM cfg) server

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = CryptoDepth.Backend.Handler.Numeraires.handler
    :<|> CryptoDepth.Backend.Handler.PathSums.handler
    :<|> CryptoDepth.Backend.Handler.Paths.handler
