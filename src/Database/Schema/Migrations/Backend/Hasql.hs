{-# LANGUAGE OverloadedStrings #-}
module Database.Schema.Migrations.Backend.Hasql
    ( hasqlBackend
    )
where

import           Hasql.Connection
import           Hasql.Decoders
import qualified Hasql.Encoders                       as H.Encode
import           Hasql.Session
import           Hasql.Statement

import           Database.Schema.Migrations.Backend   (Backend (..),
                                                       rootMigrationName)
import           Database.Schema.Migrations.Migration (Migration (..),
                                                       newMigration)

import           Data.ByteString                      (ByteString)
import           Data.Time.Clock                      (getCurrentTime)
import           System.Exit                          (ExitCode (ExitFailure),
                                                       exitWith)

migrationTableName :: ByteString
migrationTableName = "installed_migrations"

createSql :: ByteString
createSql = "CREATE TABLE " <> migrationTableName <> " (migration_id TEXT)"

revertSql :: ByteString
revertSql = "DROP TABLE " <> migrationTableName

-- |General Backend constructor for all Hasql connection implementations.
hasqlBackend :: Connection -> Backend
hasqlBackend conn =
    Backend { isBootstrapped = do
                exists <- run (statement () $ Statement "SELECT migration_id FROM installed_migrations WHERE FALSE" H.Encode.noParams noResult False) conn
                return $ case exists of
                  Left _  -> False
                  Right _ -> True
            , getBootstrapMigration = do
                ts <- getCurrentTime
                return $ (newMigration rootMigrationName)
                    { mApply = createSql
                    , mRevert = Just revertSql
                    , mDesc = Just "Migration table installation"
                    , mTimestamp = Just ts
                    }

            , applyMigration = \m -> do
                _ <- run (sql "BEGIN") conn
                reportAction <- run (sql $ mApply m) conn
                case reportAction of
                  Left e  -> do
                    _ <- run (sql "ABORT") conn
                    reportSqlError e
                  Right i -> return i
                register <- run (sql $ "INSERT INTO " <> migrationTableName <>
                          " (migration_id) VALUES ('" <> mId m <> "')") conn
                case register of
                  Left e  -> do
                    _ <- run (sql "ABORT") conn
                    reportSqlError e
                  Right i -> do
                    _ <- run (sql "COMMIT") conn
                    return i
                return ()

            , revertMigration = \m -> do
                  case mRevert m of
                    Nothing -> return ()
                    Just revQ -> do
                      action <- run (sql revQ) conn
                      case action of
                        Left e  -> reportSqlError e
                        Right i -> return i
                  -- Remove migration from installed_migrations in either case.
                  deleteAction <- run (sql $ "DELETE FROM " <> migrationTableName <>
                            " WHERE migration_id = '" <> mId m <> "'") conn
                  case deleteAction of
                    Left e  -> reportSqlError e
                    Right i -> return i
                  return ()

            , getMigrations = do
                selectNames <- run (statement () $ Statement "SELECT migration_id FROM installed_migrations" H.Encode.noParams (rowList $ column $ nonNullable bytea) False) conn
                results <- case selectNames of
                  Left e      -> reportSqlError e
                  Right names -> return names
                return results

            , disconnectBackend = release conn
            }

reportSqlError :: SessionError -> IO a
reportSqlError e = do
  putStrLn $ "\n" <> "A database error occurred: " <> show e
  exitWith (ExitFailure 1)
