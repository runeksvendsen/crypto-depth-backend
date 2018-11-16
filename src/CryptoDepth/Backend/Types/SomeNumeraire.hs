{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CryptoDepth.Backend.Types.SomeNumeraire
( module CryptoDepth.Backend.Types.SomeNumeraire
, Proxy(Proxy)
)
where

import CryptoDepth.Db.Internal.Query.Common     (PathTable, OneDiv)
import GHC.TypeLits
import Data.Proxy
import qualified Data.Text                      as T
import Database.Beam.Postgres                   (Postgres)


data SomeNumeraire = forall numeraire.
    ( PathTable numeraire Postgres
    , KnownSymbol numeraire
    )
    => SomeNumeraire (Proxy numeraire)

fromText :: T.Text -> Either String SomeNumeraire
fromText "USD"   = Right $ SomeNumeraire (Proxy :: Proxy "USD")
fromText "EUR"   = Right $ SomeNumeraire (Proxy :: Proxy "EUR")
fromText "GBP"   = Right $ SomeNumeraire (Proxy :: Proxy "GBP")
fromText "JPY"   = Right $ SomeNumeraire (Proxy :: Proxy "JPY")
fromText curr    = Left $ "Unsupported numeraire: " ++ show curr
