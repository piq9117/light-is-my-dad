{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User.Api where

import Data.UUID (UUID)
import Servant.API (Capture, Get, JSON, NoContent, Post, ReqBody, (:<|>), (:>))
import User.Types (CreateUser, GetUser)
import Servant.Auth.Server (Auth)

type UserApi auths =
  "users"
    :> ( Auth auths GetUser :> Get '[JSON] [GetUser]
           :<|> Auth auths GetUser :> Capture "userId" UUID :> Get '[JSON] (Maybe GetUser)
           :<|> ReqBody '[JSON] CreateUser :> Post '[JSON] NoContent
       )
