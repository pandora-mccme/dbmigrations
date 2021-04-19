{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Yaml
where

import Prelude

import Data.ByteString (ByteString)

import Data.Time () -- for UTCTime Show instance


import Data.Yaml.YamlLight

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , emptyMigration
    )
import Database.Schema.Migrations.Store
import Database.Schema.Migrations.Fields
import Data.List (find)

data YamlFile = YamlFile {yamlName :: ByteString, yamlContent :: ByteString}

storeFromYamls :: [YamlFile] -> MigrationStore
storeFromYamls yamls =
    MigrationStore { fullMigrationName = pure

                   , loadMigration = \migrationName -> case find ((== migrationName) . yamlName) yamls of
                      Nothing -> pure $ Left $ "No such migration given: " ++ show migrationName
                      Just yaml -> migrationFromYaml yaml

                   , getMigrations = pure $ map yamlName yamls

                   , saveMigration = error "I can't save migrations."
                   }


migrationFromYaml :: YamlFile -> IO (Either String Migration)
migrationFromYaml YamlFile{yamlName = name, yamlContent = bs} = do
      yaml <- parseYamlBytes bs
      -- Convert yaml structure into basic key/value map  
      let fields = getFields yaml
          missing = missingFields fields

      pure $ case length missing of
        0 -> do
          let newM = emptyMigration name
          case migrationFromFields newM fields of
            Nothing -> Left $ "Error in " ++ show name ++ ": unrecognized field found"
            Just m -> return m
        _ -> Left $ "Error in " ++ show name ++ ": missing required field(s): " ++ show missing
