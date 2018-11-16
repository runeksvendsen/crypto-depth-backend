module Main where

import qualified CryptoDepth.Backend    as Lib
import           CryptoDepth.Backend    (PoolConfig(..))
import           System.Environment     (lookupEnv)
import           Data.Maybe             (fromMaybe)


main :: IO ()
main = do
    dbUrl <- dbUrlFromEnvVar
    Lib.runApp 8080
        PoolConfig
            { dbConnStr         = dbUrl
            , numStripes        = 2
            , numConnPerStripe  = 10
            , keepOpenTimeout   = 60
            }
  where
    dbUrlFromEnvVar =
        fromMaybe (error errMsg) <$> lookupEnv dbEnvVar
    dbEnvVar = "DB_URL"
    errMsg = "ERROR: " ++ show dbEnvVar ++ " doesn't contain database URL"
