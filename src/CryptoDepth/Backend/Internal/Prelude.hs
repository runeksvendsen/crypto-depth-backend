module CryptoDepth.Backend.Internal.Prelude
( toS
, module AppM
, throwError
)
where

import CryptoDepth.Backend.Types.AppM   as AppM
import Protolude                        (toS)
import Control.Monad.Error              (throwError)
