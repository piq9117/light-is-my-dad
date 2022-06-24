module Main where

import Server (AppEnv (..), initDbConnection, runApp)
import System.Environment (getEnv)

main :: IO ()
main = do
  connString <- getEnv "DATABASE_URL"
  connPool <- initDbConnection connString
  let appEnv =
        AppEnv
          { connectionPool = connPool
          }
  runApp appEnv 8081
