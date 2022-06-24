{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Exception.Safe (throw)
import Control.Monad.Catch (MonadThrow)
import Api (AppApi)
import Control.Monad.Except (MonadError)
import DB (ManageDB (..), runDBImpl)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool, PoolConfig (..), newPool)
import Database.Beam.Postgres (Connection)
import qualified Database.Beam.Postgres as Postgres
  ( Connection,
    close,
    connectPostgreSQL,
  )
import Login (ManageLogin (..), loginHandlerImpl)
import Network.Wai.Handler.Warp (Port, run)
import Servant (Context)
import Servant.API (type (:<|>) ((:<|>)))
import Servant.Auth.Server
  ( CookieSettings,
    JWTSettings,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
    JWT
  )
import Servant.Server
  ( Application,
    Context (..),
    Handler (..),
    Server,
    ServerError,
    ServerT,
    hoistServerWithContext,
    serveWithContext,
  )
import User.Handler
  ( ManageUser (..),
    getUserByIdImpl,
    getUsersImpl,
    postUserImpl,
    getUserHandler,
    getUserByIdHandler,
  )

newtype App a = App
  { unApp :: ReaderT AppEnv (ExceptT ServerError IO) a
  }

deriving instance Functor App

deriving instance Applicative App

deriving instance Monad App

deriving instance MonadIO App

deriving instance MonadReader AppEnv App

deriving instance MonadError ServerError App

deriving instance MonadThrow App

class Monad m => ManageServer m where
  startServer :: AppEnv -> Port -> m ()

data AppEnv = AppEnv
  { connectionPool :: Pool Connection
  }

appApi :: Proxy (AppApi '[JWT])
appApi = Proxy

proxyContext :: Proxy '[JWTSettings, CookieSettings]
proxyContext = Proxy

appServer :: CookieSettings -> JWTSettings -> ServerT (AppApi auths) App
appServer cs jwts =
  let userApi =
        getUserHandler
          :<|> getUserByIdHandler
          :<|> postUser

   in userApi :<|> (loginHandler cs jwts)

toHandler :: AppEnv -> App a -> Handler a
toHandler env application = Handler $ flip runReaderT env $ unApp application

appToServer :: CookieSettings -> JWTSettings -> AppEnv -> Server (AppApi '[JWT])
appToServer cookieSettings jwtSettings env =
  hoistServerWithContext
    appApi
    proxyContext
    (toHandler env)
    (appServer cookieSettings jwtSettings)

app
  :: Context '[CookieSettings, JWTSettings] ->
  CookieSettings ->
  JWTSettings ->
  AppEnv ->
  Application
app context cookieSettings jwtSettings env =
  serveWithContext appApi context (appToServer cookieSettings jwtSettings env)

runApp :: AppEnv -> Port -> IO ()
runApp env port = do
  res <- runExceptT $ flip runReaderT env $ unApp $ startServer env port
  either throw pure res

instance ManageUser App where
  getUsers = getUsersImpl
  getUserById = getUserByIdImpl
  postUser = postUserImpl

instance ManageServer App where
  startServer env port = do
    key <- liftIO generateKey
    putStrLn $ "Server running on port: " <> (show port)

    let jwtCfg = defaultJWTSettings key
    let ctx = defaultCookieSettings :. jwtCfg :. EmptyContext

    liftIO $ run port (app ctx defaultCookieSettings jwtCfg env)

instance ManageDB App where
  runDB query = do
    AppEnv {connectionPool} <- ask
    runDBImpl connectionPool query

instance ManageLogin App where
  loginHandler = loginHandlerImpl

initDbConnection :: MonadIO m => String -> m (Pool Postgres.Connection)
initDbConnection connectionString =
  liftIO $
    newPool $
      PoolConfig
        { createResource = Postgres.connectPostgreSQL $ BS.pack connectionString,
          freeResource = Postgres.close,
          poolCacheTTL = 60,
          poolMaxResources = 10
        }
