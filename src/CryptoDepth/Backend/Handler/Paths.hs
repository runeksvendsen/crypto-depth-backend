{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CryptoDepth.Backend.Handler.Paths where

import CryptoDepth.Backend.Types
import CryptoDepth.Backend.Types.AppM
import CryptoDepth.Backend.Internal.Util
import CryptoDepth.Backend.Types.SomeSlippage

import CryptoDepth.Db.Internal.Query.Paths      (PathInfo(..))
import qualified CryptoDepth.Db.Query           as Db

import Servant.Server
import qualified Data.Text                      as T
import Data.Pagination
import Data.Tagged                              (Tagged(unTagged))
import Data.Functor.Identity                    (Identity)


handler :: T.Text -> Word -> Sym -> AppM BuySellPaths
handler numeraire slippage symbol = do
    SomeSlippage (Proxy :: Proxy slippage) (Proxy :: Proxy numeraire)
        :: SomeSlippage Identity <- either throw400 return
            (fromWord slippage =<< fromText numeraire)
    result <- runDb $ Db.newestBuySellPaths symbol
    return $ convert (result :: ReturnType slippage numeraire)

type ReturnType slippage numeraire =
    ( [PathInfo numeraire slippage]
    , [PathInfo numeraire slippage]
    )

convert ::
    ( [PathInfo numeraire slippage]
    , [PathInfo numeraire slippage]
    )
    -> BuySellPaths
convert (buyL, sellL) =
    BuySellPaths (map piToSymPath buyL) (map piToSymPath sellL)
  where
    piToSymPath (PathInfo qty path) =
        SymPath path (fromIntegral $ unTagged qty)
