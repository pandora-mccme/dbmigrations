module Database.Schema.Migrations.Fields where

import Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance
import qualified Data.Map as Map


import Data.Yaml.YamlLight

import Database.Schema.Migrations.Migration
    ( Migration(..)
    
    )

type FieldProcessor = ByteString -> Migration -> Maybe Migration


getFields :: YamlLight -> [(String, ByteString)]
getFields (YMap mp) = map toPair $ Map.assocs mp
    where
      toPair (YStr k, YStr v) = (BSC.unpack k, v)
      toPair (k, v) = error $ "Error in YAML input; expected string key and string value, got " ++ (show (k, v))
getFields _ = error "Error in YAML input; expected mapping"

missingFields :: [(String, ByteString)] -> [String]
missingFields fs =
    [ k | k <- requiredFields, k `notElem` inputStrings ]
    where
      inputStrings = map fst fs

-- |Given a migration and a list of parsed migration fields, update
-- the migration from the field values for recognized fields.
migrationFromFields :: Migration -> [(String, ByteString)] -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

requiredFields :: [String]
requiredFields = [ "Apply"
                 , "Depends"
                 ]

fieldProcessors :: [(String, FieldProcessor)]
fieldProcessors = [ ("Created", setTimestamp )
                  , ("Description", setDescription )
                  , ("Apply", setApply )
                  , ("Revert", setRevert )
                  , ("Depends", setDepends )
                  ]

setTimestamp :: FieldProcessor
setTimestamp value m = do
  ts <- case readTimestamp (BSC.unpack value) of
          [(t, _)] -> return t
          _ -> fail "expected one valid parse"
  return $ m { mTimestamp = Just ts }

readTimestamp :: String -> [(UTCTime, String)]
readTimestamp = reads

setDescription :: FieldProcessor
setDescription desc m = Just $ m { mDesc = Just desc }

setApply :: FieldProcessor
setApply apply m = Just $ m { mApply = apply }

setRevert :: FieldProcessor
setRevert revert m = Just $ m { mRevert = Just revert }

setDepends :: FieldProcessor
setDepends depString m = Just $ m { mDeps = BSC.words depString }
