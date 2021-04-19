{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStoreSettings(..)
    , migrationFromFile
    , migrationFromPath
    , filesystemStore
    )
where

import Prelude

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension
                       , takeFileName, takeBaseName )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Data.Typeable ( Typeable )
import Data.Time () -- for UTCTime Show instance

import Control.Monad ( filterM )
import Control.Exception ( IOException, Exception(..), throw, catch )

import Data.Yaml.YamlLight

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , emptyMigration
    )
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store
import Database.Schema.Migrations.Fields


data FilesystemStoreSettings = FSStore { storePath :: FilePath }

data FilesystemStoreError = FilesystemStoreError String
                            deriving (Show, Typeable)

instance Exception FilesystemStoreError

throwFS :: String -> a
throwFS = throw . FilesystemStoreError

filenameExtension :: String
filenameExtension = ".yml"

filesystemStore :: FilesystemStoreSettings -> MigrationStore
filesystemStore s =
    MigrationStore { fullMigrationName = fsFullMigrationName s

                   , loadMigration = \theId -> migrationFromFile s theId

                   , getMigrations = do
                       contents <- getDirectoryContents $ storePath s
                       let migrationFilenames = [ f | f <- contents, isMigrationFilename (BSC.pack f) ]
                           fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
                       existing <- filterM (\(_, full) -> doesFileExist full) fullPaths
                       return [ BSC.pack $ dropExtension short | (short, _) <- existing ]

                   , saveMigration = \m -> do
                       filename <- fsFullMigrationName s $ mId m
                       writeFile (BSC.unpack filename) $ (BSC.unpack $ serializeMigration m)
                   }

fsFullMigrationName :: FilesystemStoreSettings -> ByteString -> IO ByteString
fsFullMigrationName s name = return $ BSC.pack $ storePath s </> BSC.unpack name ++ filenameExtension

isMigrationFilename :: ByteString -> Bool
isMigrationFilename path = takeExtension (BSC.unpack path) == filenameExtension

-- |Given a store and migration name, read and parse the associated
-- migration and return the migration if successful.  Otherwise return
-- a parsing error message.
migrationFromFile :: FilesystemStoreSettings -> ByteString -> IO (Either String Migration)
migrationFromFile store name =
    fsFullMigrationName store name >>= migrationFromPath

-- |Given a filesystem path, read and parse the file as a migration
-- return the 'Migration' if successful.  Otherwise return a parsing
-- error message.
migrationFromPath :: ByteString -> IO (Either String Migration)
migrationFromPath pathStr = do
  let name = takeBaseName $ takeFileName path
  (Right <$> process name) `catch` (\(FilesystemStoreError s) -> return $ Left $ "Could not parse migration " ++ path ++ ":" ++ s)

  where
    path = BSC.unpack pathStr
    process name = do
      yaml <- parseYamlFile path `catch` (\(e::IOException) -> throwFS $ show e)

      -- Convert yaml structure into basic key/value map
      let fields = getFields yaml
          missing = missingFields fields

      case length missing of
        0 -> do
          let newM = emptyMigration (BSC.pack name)
          case migrationFromFields newM fields of
            Nothing -> throwFS $ "Error in " ++ (show path) ++ ": unrecognized field found"
            Just m -> return m
        _ -> throwFS $ "Error in " ++ (show path) ++ ": missing required field(s): " ++ (show missing)
