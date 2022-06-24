{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot #-}

module User.Types where

import Data.Aeson
  (ToJSON (..), object, (.=), FromJSON(..), withObject, (.:))
import Data.UUID (UUID)
import Servant.Auth.Server (ToJWT, FromJWT)

data GetUser = GetUser
  { name :: Text,
    age :: Int32,
    email :: Text,
    id_ :: UUID
  }

instance ToJSON GetUser where
  toJSON GetUser {..} =
    object
      [ "name" .= name,
        "age" .= age,
        "email" .= email,
        "id" .= id_
      ]

instance FromJSON GetUser where
  parseJSON = withObject "GetUser" $ \g ->
    GetUser
      <$> g .: "name"
      <*> g .: "age"
      <*> g .: "email"
      <*> g .: "id"

instance ToJWT GetUser
instance FromJWT GetUser

data CreateUser = CreateUser
  { name :: Text,
    age :: Int32,
    email :: Text,
    password :: Text
  }

instance FromJSON CreateUser where
  parseJSON = withObject "CreateUser" $ \u ->
    CreateUser
      <$> u .: "name"
      <*> u .: "age"
      <*> u .: "email"
      <*> u .: "password"
