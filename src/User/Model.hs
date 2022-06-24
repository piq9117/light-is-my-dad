{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module User.Model where

import Crypto.BCrypt
  ( fastBcryptHashingPolicy,
    hashPasswordUsingPolicy,
    validatePassword,
  )
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Database.Beam
  ( Beamable,
    Columnar,
    DatabaseEntity,
    EntityModification,
    FromBackendRow (..),
    PrimaryKey,
    Table (..),
    TableEntity,
    modifyTableFields,
    setEntityName,
    tableModification,
  )
import Database.Beam.Backend (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax, defaultPgValueSyntax)

data UserT f = User
  { userId :: Columnar f UUID,
    userName :: Columnar f Text,
    userAge :: Columnar f Int32,
    userEmail :: Columnar f Text,
    userPassword :: Columnar f Password,
    userRegistrationDate :: Columnar f Day
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type UserId = PrimaryKey UserT Identity

type User = UserT Identity

instance Table UserT where
  newtype PrimaryKey UserT f = UserId
    { unUserKey :: Columnar f UUID
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey = UserId . userId

userSettings :: EntityModification (DatabaseEntity be db) be (TableEntity UserT)
userSettings =
  setEntityName "users"
    <> modifyTableFields
      tableModification
        { userId = "id",
          userName = "name",
          userAge = "age",
          userEmail = "email",
          userRegistrationDate = "registration_date"
        }

newtype Password = Password
  { unPassword :: Text
  }
  deriving (Eq, Show)

instance FromBackendRow Postgres Password where
  fromBackendRow = do
    txtPassword <- fromBackendRow
    pure $ Password txtPassword

instance HasSqlValueSyntax PgValueSyntax Password where
  sqlValueSyntax = defaultPgValueSyntax <<< coerce @Password @Text

mkPassword :: MonadIO m => Text -> m (Maybe Password)
mkPassword txtPassword = do
  passwordBS <-
    liftIO $
      hashPasswordUsingPolicy
        fastBcryptHashingPolicy
        (encodeUtf8 txtPassword)
  pure $ (Password <<< decodeUtf8) <$> passwordBS

verifyPassword :: Text -> Password -> Bool
verifyPassword passwordTxt password =
  validatePassword
    (encodeUtf8 passwordTxt)
    (encodeUtf8 $ coerce @Password @Text password)
