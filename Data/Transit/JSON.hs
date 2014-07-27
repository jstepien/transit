{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances #-}

module Data.Transit.JSON (
  JSON(JSON)
) where

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import Data.Transit.Internal

data JSON = JSON

data AsKey = AsKey | AsVal

instance Repr JSON ByteString where
  encode JSON val = J.encode (AsVal, val)
  decode JSON = J.decode

str :: Text -> J.Value
str = J.String

kvsToJSON :: [(Value, Value)] -> J.Value
kvsToJSON = J.Array . V.fromList . (str "^ " :) . flatten . map jsonPair
  where
    jsonPair (k, v) = (J.toJSON (AsKey, k), J.toJSON (AsVal, v))
    flatten [] = []
    flatten ((k, v) : kvs) = k : v : flatten kvs

instance J.ToJSON (AsKey, Value) where
  toJSON (AsVal, Bool val) = J.Bool val
  toJSON (AsVal, Null) = J.Null
  toJSON (AsKey, Null) = str "~_"
  toJSON (AsKey, Bool False) = str "~?f"
  toJSON (AsKey, Bool True) = str "~?t"
  toJSON (_, String val) = J.toJSON val
  toJSON (_, Array val) = J.Array $ V.fromList $ map (J.toJSON . (,) AsVal) val
  toJSON (_, Dict val) = kvsToJSON val

instance J.FromJSON Value where
  parseJSON (J.Bool b) = return $ Bool b
  parseJSON (J.Array vec) = Array `fmap` mapM J.parseJSON (V.toList vec)
