{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# language OverloadedRecordDot #-}
{-# language FlexibleContexts #-}

module Login where

import qualified Data.UUID as UUID
import Data.Time.Clock (getCurrentTime, addUTCTime, nominalDay)
import User.Types (GetUser(..))
import Servant.Server (err404, ServerError(..), err403, err422)
import Control.Monad.Except (MonadError(..))
import DB (ManageDB)
import qualified User.Model as DB
import User.Queries (selectUserByEmail)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.=),
  )
import Servant.API (JSON, Post, ReqBody, (:>))
import Servant.Auth.Server
  ( CookieSettings,
    FromJWT,
    JWTSettings,
    ToJWT,
    makeJWT
  )

type LoginApi =
  "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] LoginResponse

class Monad m => ManageLogin m where
  loginHandler :: CookieSettings -> JWTSettings -> LoginRequest -> m LoginResponse

data LoginRequest = LoginRequest
  { email :: Text,
    password :: Text
  }
  deriving (Eq, Show)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \l ->
    LoginRequest
      <$> l .: "email"
      <*> l .: "password"

data LoginResponse = LoginResponse
  { username :: Text,
    token :: Text,
    id_ :: Text,
    age :: Int32
  }

instance ToJSON LoginResponse where
  toJSON LoginResponse {..} =
    object
      [ "username" .= username,
        "token" .= token,
        "id" .= id_,
        "age" .= age
      ]

instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \l ->
    LoginResponse
      <$> l .: "username"
      <*> l .: "token"
      <*> l .: "id"
      <*> l .: "age"

instance ToJWT LoginResponse

instance FromJWT LoginResponse

loginHandlerImpl ::
  (ManageDB m, MonadError ServerError m, MonadIO m) =>
  CookieSettings ->
  JWTSettings ->
  LoginRequest ->
  m LoginResponse
loginHandlerImpl _ jwtSettings loginRequest = do
  user <- whenNothingM (selectUserByEmail loginRequest.email) $
    throwError err404

  now <- liftIO getCurrentTime

  if DB.verifyPassword loginRequest.password (DB.userPassword user)
    then throwError err403
    else  do
      let twoDays = Just (addUTCTime (nominalDay + nominalDay) now)

      eJwtToken <- liftIO $ makeJWT (toGetUser user) jwtSettings twoDays
      case eJwtToken of
        Left _ -> throwError err422
        Right jwtToken -> pure $
          LoginResponse
            { username = DB.userName user,
              id_ = UUID.toText $ DB.userId user,
              token = decodeUtf8 jwtToken,
              age = DB.userAge user
            }
  where
    toGetUser :: DB.User -> GetUser
    toGetUser DB.User{..} =
      GetUser
        { name = userName,
          age = userAge,
          email = userEmail,
          id_ = userId
        }

