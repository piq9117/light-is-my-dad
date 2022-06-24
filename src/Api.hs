{-# LANGUAGE TypeOperators #-}

module Api where

import Login (LoginApi)
import Servant.API ((:<|>))
import User.Api (UserApi)

type AppApi auths =  UserApi auths
  :<|> LoginApi
