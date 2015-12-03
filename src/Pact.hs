{-# LANGUAGE OverloadedStrings #-}
module Pact
 ( Query(..), queryString
 , Headers(..)
 , Request(..)
 , Response(..)
 , Interaction(..)
 , InteractionWrapper(..)
 , ServiceDescription(..)
 , ContractDescription(..)
 , Diff
 , diffRequests
 , diffResponses
 , convertHeadersToJson
 , convertHeadersFromJson
 ) where

import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Function (on)
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
import qualified Network.HTTP.Types.Header as HTH
import qualified Data.CaseInsensitive as CI
import Data.Monoid (All(..))

hasNullValue :: AT.Pair -> Bool
hasNullValue (_, Null) = True
hasNullValue _ = False

newtype Headers = Headers [(String, String)]
  deriving (Eq, Show)

instance Monoid Headers where
  mempty = Headers []
  mappend (Headers as) (Headers bs) = Headers (L.union as bs)

instance FromJSON Headers where
  parseJSON x = Headers . HM.toList <$> parseJSON x

instance ToJSON Headers where
  toJSON (Headers hs) = toJSON $ HM.fromList hs

newtype Query = Query [(BS.ByteString, BS.ByteString)]
  deriving (Eq, Show)

instance Monoid Query where
  mempty = Query []
  mappend (Query as) (Query bs) = Query (as ++ bs)

instance ToJSON Query where
  toJSON = String . T.pack . queryString False

queryString :: Bool -> Query -> String
queryString qm (Query qs) = CS.unpack $ H.renderSimpleQuery qm qs

normalizedQuery :: Value -> Either String Query
normalizedQuery (String x) = Right . Query . H.parseSimpleQuery . CS.pack $ T.unpack x
normalizedQuery (Object x) = Query <$> mapToSimpleQuery x
normalizedQuery _ = Left "Could not normalize query for request"

mapToSimpleQuery :: HM.HashMap T.Text Value -> Either String [(BS.ByteString, BS.ByteString)]
mapToSimpleQuery m = L.sortBy (compare `on` fst) . concat <$> mapM flattenQuery (HM.toList m)

flattenQuery :: (T.Text, Value) -> Either String [(BS.ByteString, BS.ByteString)]
flattenQuery (k, (Array vs)) = mapM (byteStringPairs k) $ V.toList vs
flattenQuery (k, s@(String _)) = pure <$> byteStringPairs k s
flattenQuery _ = Left "Could not flatten query for request"

byteStringPairs :: T.Text -> Value -> Either String (BS.ByteString, BS.ByteString)
byteStringPairs key (String val) = Right (E.encodeUtf8 key, E.encodeUtf8 val)
byteStringPairs _ _ = Left "Could not convert to byte string pairs"

data Request = Request
 { requestMethod :: String
 , requestPath :: String
 , requestQuery :: Query
 , requestHeaders :: Headers
 , requestBody :: Maybe Value
 } deriving (Show, Eq)

instance FromJSON Request where
  parseJSON (Object v) = Request <$> v .: "method" <*> v .: "path" <*> query <*> headers <*> v .:? "body"
    where
      headers = fmap (fromMaybe mempty) $ v .:? "headers"
      query = do
        maybeQ <- v .:? "query"
        case maybeQ of
          Nothing -> pure mempty
          Just q -> either fail pure $ normalizedQuery q
  parseJSON _ = fail "Could not parse Pact.Request"

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
 , responseHeaders :: Headers
 , responseBody :: Maybe Value
 } deriving (Show, Eq)

instance FromJSON Response where
  parseJSON (Object v) = Response <$> v .:? "status" <*> headers <*> v .:?"body"
    where headers = fmap (fromMaybe mempty) $ v .:? "headers"
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
diffRequests expected actual = getAll $ foldMap All
 [ verifyMethod (requestMethod expected) (requestMethod actual)
 , verifyPath (requestPath expected) (requestPath actual)
 , verifyQuery (requestQuery expected) (requestQuery actual)
 , verifyHeaders (requestHeaders expected) (requestHeaders actual)
 , verifyBodyStrict (requestBody expected) (requestBody actual)
 ]

diffResponses :: Response -> Response -> Diff
diffResponses expected actual = foldl1 (&&)
 [ verifyStatus (responseStatus expected) (responseStatus actual)
 , verifyHeaders (responseHeaders expected) (responseHeaders actual)
 , verifyBody (responseBody expected) (responseBody actual)
 ]

verifyMethod :: String -> String -> Diff
verifyMethod expected actual = uppercase expected == uppercase actual
  where uppercase = map toUpper

verifyPath :: String -> String -> Diff
verifyPath = (==)

verifyQuery :: Query -> Query -> Diff
verifyQuery (Query expected) (Query actual) = toQ expected == toQ actual
  where toQ s = L.sortOn fst s

verifyHeaders :: Headers -> Headers -> Diff
verifyHeaders (Headers expected) (Headers actual) = sanitize expected == (L.intersect (sanitize actual) (sanitize expected))
  where sanitize obj = map (\(k,v) -> (toLower <$> k, fixValue v)) obj
        fixValue = filter (/= ' ')

verifyBodyStrict :: Maybe Value -> Maybe Value -> Diff
verifyBodyStrict Nothing _ = True
verifyBodyStrict (Just _) Nothing = False
verifyBodyStrict (Just expected) (Just actual) = expectedObj == actualObj
  where expectedObj = HM.fromList [("value" :: String, expected :: Value)]
        actualObj = HM.fromList [("value" :: String, actual :: Value)]

verifyBody :: Maybe Value -> Maybe Value -> Diff
verifyBody Nothing _ = True
verifyBody (Just _) Nothing = False
verifyBody (Just expected) (Just actual) = expectedObj == intersect actualObj expectedObj
  where expectedObj = Object $ HM.fromList [("value" :: T.Text, expected :: Value)]
        actualObj = Object $ HM.fromList [("value" :: T.Text, actual :: Value)]
        intersect :: Value -> Value -> Value
        intersect (Object vals) (Object filts) =
         Object
           $ HM.mapWithKey (\k _ -> intersect ((HM.!) vals k) ((HM.!) filts k))
           $ HM.intersection vals filts
        intersect x _ = x

verifyStatus :: Maybe Int -> Maybe Int -> Diff
verifyStatus Nothing _ = True
verifyStatus (Just _) Nothing = False
verifyStatus(Just expected) (Just actual) = expected == actual

convertHeadersToJson :: [HTH.Header] -> Headers
convertHeadersToJson headers = Headers $ map toTextPair headers
  where toTextPair (k, v) = (T.unpack . E.decodeUtf8 $ CI.foldedCase k, T.unpack $ E.decodeUtf8 v)

convertHeadersFromJson :: Headers -> [HTH.Header]
convertHeadersFromJson (Headers hs) = map fromTextPair $ hs
  where
    fromTextPair (k, v) = (CI.mk . E.encodeUtf8 $ T.pack k, E.encodeUtf8 $ T.pack v)
