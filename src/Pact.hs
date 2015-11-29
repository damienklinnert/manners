{-# LANGUAGE OverloadedStrings #-}
module Pact
 ( Request(..)
 , Response(..)
 , Interaction(..)
 , InteractionWrapper(..)
 , ServiceDescription(..)
 , ContractDescription(..)
 , Diff
 , diffRequests
 ) where

import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Types as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Control.Monad (liftM)

hasNullValue :: AT.Pair -> Bool
hasNullValue (_, Null) = True
hasNullValue _ = False

data Request = Request
 { requestMethod :: String
 , requestPath :: String
 , requestQuery :: Maybe String
 , requestHeaders :: Maybe Object
 , requestBody :: Maybe Value
 } deriving (Show, Eq)

instance FromJSON Request where
  parseJSON (Object v) = Request <$> v .: "method" <*> v .: "path" <*> liftM normalizedQuery (v .:? "query") <*> v .:? "headers" <*> v .:? "body"
    where
      normalizedQuery :: Maybe Value -> Maybe String
      normalizedQuery Nothing = Nothing
      normalizedQuery (Just (String x)) = Just $ T.unpack x
      normalizedQuery (Just (Object x)) = Just $ CS.unpack $ H.renderSimpleQuery False $ mapToSimpleQuery x
      normalizedQuery _ = error "Could not normalize query for request"
      mapToSimpleQuery :: HM.HashMap T.Text Value -> [(BS.ByteString, BS.ByteString)]
      mapToSimpleQuery m = concat $ map flattenQuery $ HM.toList m
      flattenQuery :: (T.Text, Value) -> [(BS.ByteString, BS.ByteString)]
      flattenQuery (k, (Array vs)) = map (byteStringPairs k) $ V.toList vs
      flattenQuery (k, s@(String _)) = [byteStringPairs k s]
      flattenQuery _ = error "Could not flatten query for request"
      byteStringPairs :: T.Text -> Value -> (BS.ByteString, BS.ByteString)
      byteStringPairs key (String val) = (E.encodeUtf8 key, E.encodeUtf8 val)
      byteStringPairs _ _ = error "Could not convert to byte string pairs"
  parseJSON _ = error "Could not parse Pact.Request"

instance ToJSON Request where
  toJSON (Request method path query headers body) = object $ filter (not . hasNullValue)
   [ "method" .= method
   , "path" .= path
   , "query" .= query
   , "headers" .= headers
   , "body" .= body
   ]

data Response = Response
 { responseStatus :: Maybe Int
 , responseHeaders :: Maybe Object
 , responseBody :: Maybe Value
 } deriving (Show, Eq)

instance FromJSON Response where
  parseJSON (Object v) = Response <$> v .:? "status" <*> v .:? "headers" <*> v .:?"body"
  parseJSON _ = error "Could not parse Pact.Response"

instance ToJSON Response where
  toJSON (Response status headers body) = object $ filter (not . hasNullValue)
   [ "status" .= status
   , "headers" .= headers
   , "body" .= body
   ]

data Interaction = Interaction
 { interactionDescription :: String
 , interactionState :: Maybe String
 , interactionRequest :: Request
 , interactionResponse :: Response
 } deriving (Show, Eq)

instance FromJSON Interaction where
  parseJSON (Object v) = Interaction <$> v .: "description" <*> v .:? "provider_state" <*> v .: "request" <*> v .: "response"
  parseJSON _ = error "Could not parse Pact.Interaction"

instance ToJSON Interaction where
  toJSON (Interaction desc state req res) = object $ filter (not . hasNullValue)
   [ "description" .= desc
   , "provider_state" .= state
   , "request" .= req
   , "response" .= res
   ]

data InteractionWrapper = InteractionWrapper { wrapperInteractions :: [Pact.Interaction] } deriving (Show)
instance FromJSON InteractionWrapper where
  parseJSON (Object v) = InteractionWrapper <$> v .: "interactions"
  parseJSON _ = error "can't parse InteractionWrapper"
instance ToJSON InteractionWrapper where
  toJSON (InteractionWrapper w) = object ["interactions" .= w]

data ServiceDescription = ServiceDescription { serviceName :: String } deriving (Show, Eq)

instance FromJSON ServiceDescription where
  parseJSON (Object v) = ServiceDescription <$> v .: "name"
  parseJSON _ = error "can't parse ServiceDescription"

instance ToJSON ServiceDescription where
  toJSON (ServiceDescription name) = object ["name" .= name]

data ContractDescription = ContractDescription
 { contractConsumer :: ServiceDescription
 , contractProvider :: ServiceDescription
 , contractInteractions :: [Pact.Interaction]
 } deriving (Show, Eq)

instance FromJSON ContractDescription where
  parseJSON (Object v) = ContractDescription <$> v .: "consumer" <*> v .: "provider" <*> v .:? "interactions" .!= []
  parseJSON _ = error "cant't parse ContractDescription"

instance ToJSON ContractDescription where
  toJSON (ContractDescription consumer provider interactions) = object
   [ "consumer" .= consumer
   , "provider" .= provider
   , "interactions" .= interactions
   ]

type Diff = Bool

diffRequests :: Request -> Request -> Diff
diffRequests expected actual = foldl1 (&&)
 [ verifyMethod (requestMethod expected) (requestMethod actual)
 , verifyPath (requestPath expected) (requestPath actual)
 , verifyQuery (requestQuery expected) (requestQuery actual)
 , verifyHeaders (requestHeaders expected) (requestHeaders actual)
 , verifyBody (requestBody expected) (requestBody actual)
 ]

verifyMethod :: String -> String -> Diff
verifyMethod expected actual = uppercase expected == uppercase actual
  where uppercase = map toUpper

verifyPath :: String -> String -> Diff
verifyPath = (==)

verifyQuery :: Maybe String -> Maybe String -> Diff
verifyQuery Nothing _ = True
verifyQuery (Just _) Nothing = False
verifyQuery (Just expected) (Just actual) = toQ expected == toQ actual
  where toQ s = L.sortOn fst $ H.parseSimpleQuery (CS.pack s)

verifyHeaders :: Maybe Object -> Maybe Object -> Diff
verifyHeaders Nothing _ = True
verifyHeaders (Just _) Nothing = False
verifyHeaders (Just expected) (Just actual) = sanitize expected == (HM.intersection (sanitize actual) (sanitize expected))
  where sanitize obj = HM.fromList $ map (\(k,v) -> (T.toLower k, fixValue v)) $ HM.toList obj
        fixValue (String v) = T.filter (/= ' ') v
        fixValue _ = error "Unexpected value for verifyHeaders.sanitize"

verifyBody :: Maybe Value -> Maybe Value -> Diff
verifyBody Nothing _ = True
verifyBody (Just _) Nothing = False
verifyBody (Just expected) (Just actual) = expectedObj == (HM.intersection actualObj expectedObj)
  where expectedObj = HM.fromList [("value" :: String, expected :: Value)]
        actualObj = HM.fromList [("value" :: String, actual :: Value)]
