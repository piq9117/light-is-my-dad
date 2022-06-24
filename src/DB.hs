module DB where

import Control.Monad.Catch (onException)
import Data.Pool (Pool, withResource)
import qualified Database.Beam.Postgres as Postgres
import qualified Database.PostgreSQL.Simple as Simple

class Monad m => ManageDB m where
  runDB :: Postgres.Pg a -> m a

runDBImpl :: MonadIO m => Pool Simple.Connection -> Postgres.Pg a -> m a
runDBImpl connectionPool query =
  liftIO $
    withResource connectionPool $ \connection -> do
      let transaction = do
            Simple.begin connection
            Postgres.runBeamPostgresDebug putStrLn connection query
      result <- transaction `onException` Simple.rollback connection
      Simple.commit connection
      pure result
