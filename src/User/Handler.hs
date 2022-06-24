{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User.Handler
  ( ManageUser(..),
    getUserByIdImpl,
    getUsersImpl,
    postUserImpl,
    getUserHandler,
    getUserByIdHandler
  ) where

import qualified Models as DB
import Control.Monad.Except (MonadError (..))
import DB (ManageDB (..))
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.UUID (UUID)
import Servant.API (NoContent (..))
import Servant.Server (ServerError, err422, err401)
import User.Queries (insertUser, selectUserById, selectUsers)
import User.Types (CreateUser, GetUser (..))
import Servant.Auth.Server (AuthResult(..), throwAll, ThrowAll)

class Monad m => ManageUser m where
  getUsers :: m [GetUser]
  getUserById :: UUID -> m (Maybe GetUser)
  postUser :: CreateUser -> m NoContent

getUserHandler ::
  (ManageUser m, ThrowAll (m [GetUser])) =>
  AuthResult GetUser ->
  m [GetUser]
getUserHandler (Authenticated _) = getUsers
getUserHandler _ = throwAll err401

getUsersImpl :: ManageDB m => m [GetUser]
getUsersImpl =  do
  users <- selectUsers
  pure $ (\DB.User {..} -> GetUser userName userAge userEmail userId) <$> users

getUserByIdHandler ::
  (ManageUser m, ThrowAll (m (Maybe GetUser))) =>
  AuthResult GetUser ->
  UUID ->
  m (Maybe GetUser)
getUserByIdHandler (Authenticated _) userId = getUserById userId
getUserByIdHandler _ _ = throwAll err401

getUserByIdImpl :: ManageDB m => UUID -> m (Maybe GetUser)
getUserByIdImpl userIdInput = do
  user <- selectUserById userIdInput
  pure $ (\DB.User {..} -> GetUser userName userAge userEmail userId) <$> user

postUserImpl ::
  (MonadIO m, ManageDB m, MonadError ServerError m) =>
  CreateUser ->
  m NoContent
postUserImpl newUser = do
  now <- liftIO getCurrentTime
  mResult <- insertUser newUser (utctDay now)
  case mResult of
    Nothing -> throwError err422
    Just _ -> pure NoContent
