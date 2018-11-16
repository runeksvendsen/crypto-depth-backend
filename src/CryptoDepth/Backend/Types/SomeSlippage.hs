{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module CryptoDepth.Backend.Types.SomeSlippage
( module CryptoDepth.Backend.Types.SomeSlippage
, module SomeNumeraire
)
where

import CryptoDepth.Backend.Types.SomeNumeraire  as SomeNumeraire
import CryptoDepth.Db.Internal.Table.Path       (PathQuantity)
import GHC.TypeLits
import Data.Proxy
import CryptoDepth.Db.Internal.Query.Common     (OneDiv, PathTable)

import Database.Beam                            (QueryInaccessible)
import Database.Beam.Query.Internal             (QNested, QExpr, WithRewrittenThread)
import Database.Beam.Postgres.Syntax            (PgExpressionSyntax)
import Database.Beam.Postgres                   (Postgres)


data SomeSlippage f = forall slippage numeraire.
    ( PathQuantity slippage numeraire f
    , PathTable numeraire Postgres
    , KnownSymbol numeraire
    )
    => SomeSlippage (Proxy slippage) (Proxy numeraire)

fromWord :: Word -> SomeNumeraire -> Either String (SomeSlippage f)
fromWord slip (SomeNumeraire numrP)
    | slip == 20   = mkSomeSlippage (Proxy @(OneDiv 20))
    | slip == 100  = mkSomeSlippage (Proxy @(OneDiv 100))
    | slip == 200  = mkSomeSlippage (Proxy @(OneDiv 200))
    | slip == 1000 = mkSomeSlippage (Proxy @(OneDiv 1000))
    | otherwise    = Left $ "Unsupported slippage: " ++ show slip
  where
    mkSomeSlippage slipP =
        Right $ SomeSlippage slipP numrP

