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
import CryptoDepth.Backend.Api
import qualified CryptoDepth.Backend.Handler.Numeraires
import qualified CryptoDepth.Backend.Handler.PathSums
import qualified CryptoDepth.Backend.Handler.Paths

import qualified Network.Wai.Handler.Warp                   as Warp
import Servant


runApp :: Int -> PoolConfig -> IO ()
runApp port poolCfg = do
    cfg <- mkConfig poolCfg
    Warp.run port (app cfg)

app :: Config -> Application
app cfg = serve api $ hoistServer api (runAppM cfg) server

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = CryptoDepth.Backend.Handler.Numeraires.handler
    :<|> CryptoDepth.Backend.Handler.PathSums.handler
    :<|> CryptoDepth.Backend.Handler.Paths.handler
