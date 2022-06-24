{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User.Queries where

import DB (ManageDB (..))
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Database.Beam
  ( all_,
    default_,
    guard_,
    insert,
    insertExpressions,
    runInsert,
    runSelectReturningList,
    runSelectReturningOne,
    select,
    val_,
    (==.),
  )
import qualified Models as DB
import User.Types (CreateUser (..))

selectUsers :: ManageDB m => m [DB.User]
selectUsers =
  runDB $
    runSelectReturningList $
      select $ do
        u <- all_ $ DB.appDbUsers DB.appDb
        pure u

selectUserById :: ManageDB m => UUID -> m (Maybe DB.User)
selectUserById userId =
  runDB $
    runSelectReturningOne $
      select $ do
        u <- all_ $ DB.appDbUsers DB.appDb
        guard_ $ DB.userId u ==. val_ userId
        pure u

insertUser :: (MonadIO m, ManageDB m) => CreateUser -> Day -> m (Maybe ())
insertUser CreateUser {..} registrationDate = do
  mPassword <- DB.mkPassword password
  case mPassword of
    Nothing -> pure Nothing
    Just encodedPassword -> do
      runDB $
        runInsert $
          insert (DB.appDbUsers DB.appDb) $
            insertExpressions
              [ DB.User
                  { DB.userId = default_,
                    DB.userName = val_ name,
                    DB.userAge = val_ age,
                    DB.userEmail = val_ email,
                    DB.userRegistrationDate = val_ registrationDate,
                    DB.userPassword = val_ encodedPassword
                  }
              ]
      pure $ Just ()

selectUserByEmail :: ManageDB m => Text -> m (Maybe DB.User)
selectUserByEmail email =
  runDB $
    runSelectReturningOne $
      select $ do
        u <- all_ $ DB.appDbUsers DB.appDb
        guard_ $ DB.userEmail u ==. val_ email
        pure u
