{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.Backend.Handler.Numeraires where

import CryptoDepth.Backend.Types.AppM
import Servant.Server
import qualified Data.Text as T


handler :: AppM [T.Text]
handler =
    return ["USD", "EUR", "GBP", "JPY"]
    -- TODO: don't duplicate this here
