{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CryptoDepth.Backend.Handler.PathSums where

import CryptoDepth.Backend.Internal.Prelude
import CryptoDepth.Backend.Internal.Util
import CryptoDepth.Backend.Types.SomeSlippage
import CryptoDepth.Backend.Types

import qualified CryptoDepth.Db.Query           as Db

import Servant.Server
import qualified Data.Text                      as T
import qualified Data.Pagination                as Page
import Data.Tagged                              (Tagged(unTagged))
-- LOL

import Database.Beam                            (QueryInaccessible)
import Database.Beam.Query.Internal             (QNested, QExpr, WithRewrittenThread)
import Database.Beam.Postgres.Syntax            (PgExpressionSyntax)


-- |
handler
    :: T.Text   -- ^ Numeraire
    -> Word     -- ^ Slippage denominator (e.g. 100 -> 1/100 = 1%)
    -> Sym      -- ^ Cryptocurrency symbol
    -> Word     -- ^ Number of items per page
    -> Word     -- ^ Page number
    -> AppM [SymSum]
handler numeraire slippage symbol pageSize pageNumber = do
    pagination <- mkPagination
    SomeSlippage (Proxy :: Proxy slippage) (Proxy :: Proxy numeraire)
        :: SomeSlippage QueryType <- either throw400 return
            (fromWord slippage =<< fromText numeraire)
    let paginateQuery = Page.paginate
            pagination
            (fromIntegral pageSize)
            Db.newestBuySellPathSumsPaginate
    result <- runDb $ Page.paginatedItems <$> paginateQuery
    return $ map convert (result :: [ReturnType slippage numeraire])
  where
    paginationE = Page.mkPagination
            (fromIntegral pageSize) (fromIntegral pageNumber)
    mkPagination = either (throw400 . show) return paginationE

type QueryType = QExpr PgExpressionSyntax
    (QNested (QNested (QNested (QNested QueryInaccessible))))

type ReturnType slippage numeraire =
    ( Sym
    , ( Tagged slippage (Db.Amount numeraire)
      , Tagged slippage (Db.Amount numeraire)
      )
    )

convert
    :: ( Sym
       , ( Tagged slippage (Db.Amount numeraire)
         , Tagged slippage (Db.Amount numeraire)
         )
       )
    -> SymSum
convert (sym, (buyQty, sellQty)) = SymSum
    { ssSymbol  = sym
    , ssBuyQty  = fromIntegral $ unTagged buyQty
    , ssSellQty = fromIntegral $ unTagged sellQty
    }
