{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models
  ( module User.Model,
    module Models,
    AppDb(..)
  )
where

import Database.Beam
  ( Database,
    DatabaseSettings,
    TableEntity,
    dbModification,
    defaultDbSettings,
    withDbModification,
  )
import User.Model

data AppDb f = AppDb
  { appDbUsers :: f (TableEntity UserT)
  }
  deriving stock (Generic)

instance Database be AppDb

appDb :: DatabaseSettings be AppDb
appDb =
  defaultDbSettings
    `withDbModification` dbModification
      { appDbUsers = userSettings
      }
