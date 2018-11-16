module CryptoDepth.Backend.Internal.Util where

import CryptoDepth.Backend.Internal.Prelude
import Servant.Server

throw400 :: String -> AppM a
throw400 errText = throwError $ err400 { errReasonPhrase = errText }
