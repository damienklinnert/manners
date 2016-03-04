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
 , validateRequest
 , validateResponse
 , ValidationError(..)
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
  toJSON (Headers []) = Null
  toJSON (Headers hs) = toJSON $ HM.fromList hs

newtype Query = Query [(BS.ByteString, BS.ByteString)]
  deriving (Eq, Show)

instance Monoid Query where
  mempty = Query []
  mappend (Query as) (Query bs) = Query (as ++ bs)

instance ToJSON Query where
  toJSON (Query []) = Null
  toJSON x = String . T.pack $ queryString False x

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

data ValidationError = MethodValidationError String String
                     | PathValidationError String String
                     | QueryValidationError Query Query
                     | StatusValidationError Int Int
                     | HeaderValidationError Headers Headers
                     | BodyValidationError Value Value
                     deriving (Eq, Show)

instance ToJSON ValidationError where
  toJSON (MethodValidationError e a) = object ["type" .= ("MethodValidationError" :: String), "expected" .= e, "actual" .= a]
  toJSON (PathValidationError e a) = object ["type" .= ("PathValidationError" :: String), "expected" .= e, "actual" .= a]
  toJSON (QueryValidationError e a) = object ["type" .= ("QueryValidationError" :: String), "expected" .= e, "actual" .= a]
  toJSON (StatusValidationError e a) = object ["type" .= ("StatusValidationError" :: String), "expected" .= e, "actual" .= a]
  toJSON (HeaderValidationError e a) = object ["type" .= ("HeaderValidationError" :: String), "expected" .= e, "actual" .= a]
  toJSON (BodyValidationError e a) = object ["type" .= ("BodyValidationError" :: String), "expected" .= e, "actual" .= a]

validateRequest :: Request -> Request -> [ValidationError]
validateRequest expected actual = concat
 [ validateMethod (requestMethod expected) (requestMethod actual)
 , validatePath (requestPath expected) (requestPath actual)
 , validateQuery (requestQuery expected) (requestQuery actual)
 , validateHeaders (requestHeaders expected) (requestHeaders actual)
 , validateBodyStrict (requestBody expected) (requestBody actual)
 ]

validateResponse :: Response -> Response -> [ValidationError]
validateResponse expected actual = concat
 [ validateStatus (responseStatus expected) (responseStatus actual)
 , validateHeaders (responseHeaders expected) (responseHeaders actual)
 , validateBody (responseBody expected) (responseBody actual)
 ]

validateMethod :: String -> String -> [ValidationError]
validateMethod expected actual
  | upperE == upperA = []
  | otherwise        = [MethodValidationError upperE upperA]
  where upperE = map toUpper expected
        upperA = map toUpper actual

validatePath :: String -> String -> [ValidationError]
validatePath expected actual
  | expected == actual = []
  | otherwise          = [PathValidationError expected actual]

validateQuery :: Query -> Query -> [ValidationError]
validateQuery (Query expected) (Query actual)
  | saneE == saneA = []
  | otherwise      = [QueryValidationError (Query saneE) (Query saneA)]
  where toQ s = L.sortOn fst s
        saneE = toQ expected
        saneA = toQ actual

validateHeaders :: Headers -> Headers -> [ValidationError]
validateHeaders (Headers expected) (Headers actual)
  | saneE == (L.intersect (saneA) (saneE)) = []
  | otherwise                              = [HeaderValidationError (Headers saneE) (Headers saneA)]
  where sanitize obj = L.sortOn fst $ map (\(k,v) -> (toLower <$> k, fixValue v)) obj
        fixValue = filter (/= ' ')
        saneE = sanitize expected
        saneA = sanitize actual

validateBodyStrict :: Maybe Value -> Maybe Value -> [ValidationError]
validateBodyStrict Nothing _ = []
validateBodyStrict (Just expected) Nothing = [BodyValidationError expected Null]
validateBodyStrict (Just expected) (Just actual)
  | expectedObj == actualObj = []
  | otherwise                = [BodyValidationError expected actual]
  where expectedObj = HM.fromList [("value" :: String, expected :: Value)]
        actualObj   = HM.fromList [("value" :: String, actual   :: Value)]

validateBody :: Maybe Value -> Maybe Value -> [ValidationError]
validateBody Nothing _ = []
validateBody (Just expected) Nothing = [BodyValidationError expected Null]
validateBody (Just expected) (Just actual)
  | expectedObj == intersect actualObj expectedObj = []
  | otherwise                                      = [BodyValidationError expected actual]
  where expectedObj = Object $ HM.fromList [("value" :: T.Text, expected :: Value)]
        actualObj = Object $ HM.fromList [("value" :: T.Text, actual :: Value)]
        intersect :: Value -> Value -> Value
        intersect (Object vals) (Object filts) =
         Object
           $ HM.mapWithKey (\k _ -> intersect ((HM.!) vals k) ((HM.!) filts k))
           $ HM.intersection vals filts
        intersect x _ = x

validateStatus :: Maybe Int -> Maybe Int -> [ValidationError]
validateStatus Nothing _ = []
validateStatus (Just _) Nothing = error "Can't validate status without an actual value"
validateStatus(Just expected) (Just actual)
  | expected == actual = []
  | otherwise          = [StatusValidationError expected actual]

convertHeadersToJson :: [HTH.Header] -> Headers
convertHeadersToJson headers = Headers $ map toTextPair headers
  where toTextPair (k, v) = (CS.unpack $ CI.foldedCase k, CS.unpack v)

convertHeadersFromJson :: Headers -> [HTH.Header]
convertHeadersFromJson (Headers hs) = map fromTextPair $ hs
  where
    fromTextPair (k, v) = (CI.mk . E.encodeUtf8 $ T.pack k, E.encodeUtf8 $ T.pack v)
