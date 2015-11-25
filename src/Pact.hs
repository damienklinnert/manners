{-# LANGUAGE OverloadedStrings #-}
module Pact
 ( Request(..),
   Response(..),
   Interaction(..),
   Diff,
   diffRequests
 ) where

import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
      mapToSimpleQuery :: HM.HashMap T.Text Value -> [(BS.ByteString, BS.ByteString)]
      mapToSimpleQuery m = concat $ map flattenQuery $ HM.toList m
      flattenQuery :: (T.Text, Value) -> [(BS.ByteString, BS.ByteString)]
      flattenQuery (k, (Array vs)) = map (byteStringPairs k) $ V.toList vs
      flattenQuery (k, s@(String _)) = [byteStringPairs k s]
      byteStringPairs :: T.Text -> Value -> (BS.ByteString, BS.ByteString)
      byteStringPairs key (String val) = (E.encodeUtf8 key, E.encodeUtf8 val)

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
 } deriving (Show)

instance FromJSON Response where
  parseJSON (Object v) = Response <$> v .:? "status" <*> v .:? "headers" <*> v .:?"body"

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
 } deriving (Show)

instance FromJSON Interaction where
  parseJSON (Object v) = Interaction <$> v .: "description" <*> v .:? "provider_state" <*> v .: "request" <*> v .: "response"

instance ToJSON Interaction where
  toJSON (Interaction desc state req res) = object $ filter (not . hasNullValue)
   [ "description" .= desc
   , "provider_state" .= state
   , "request" .= req
   , "response" .= res
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

verifyBody :: Maybe Value -> Maybe Value -> Diff
verifyBody Nothing _ = True
verifyBody (Just expected) Nothing = False
verifyBody (Just expected) (Just actual) = expectedObj == (HM.intersection actualObj expectedObj)
  where expectedObj = HM.fromList [("value" :: String, expected :: Value)]
        actualObj = HM.fromList [("value" :: String, actual :: Value)]

data TestCase = TestCase
 { match :: Bool
 , comment :: String
 , expected :: Request
 , actual :: Request
 } deriving (Show, Eq)

instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$> v .: "match" <*> v .: "comment" <*> v .: "expected" <*> v .: "actual"

checkCase :: String -> IO ()
checkCase file = do
  methodDifferent <- BL.readFile file
  let (Just x) = decode methodDifferent :: Maybe TestCase
  let exp = (expected x)
  let act = (actual x)
  let matches = diffRequests exp act
  putStrLn $ (if matches == (match x) then "WIN " else "FAIL ") ++ file

testBase = "resources/pact_specification_v1.1/testcases/request"
tests =
 [ "/method/different method.json"
 , "/method/matches.json"
 , "/method/method is different case.json"

 , "/path/empty path found when forward slash expected.json"
 , "/path/forward slash found when empty path expected.json"
 , "/path/incorrect path.json"
 , "/path/matches.json"
 , "/path/missing trailing slash in path.json"
 , "/path/unexpected trailing slash in path.json"

 , "/query/different order.json"
 , "/query/different params.json"
 , "/query/matches.json"
 , "/query/missing params.json"
 , "/query/same parameter different values.json"
 , "/query/same parameter multiple times in different order.json"
 , "/query/same parameter multiple times.json"
 , "/query/trailing ampersand.json"
 , "/query/unexpected param.json"

 , "/headers/empty headers.json"
 , "/headers/header name is different case.json"
 , "/headers/header value is different case.json"
 , "/headers/matches.json"
 , "/headers/order of comma separated header values different.json"
 , "/headers/unexpected header found.json"
 , "/headers/whitespace after comma different.json"

 , "/body/array in different order.json"
 , "/body/different value found at index.json"
 , "/body/different value found at key.json"
 , "/body/matches.json"
 , "/body/missing index.json"
 , "/body/missing key.json"
 , "/body/not null found at key when null expected.json"
 , "/body/not null found in array when null expected.json"
 , "/body/null found at key where not null expected.json"
 , "/body/null found in array when not null expected.json"
 , "/body/number found at key when string expected.json"
 , "/body/number found in array when string expected.json"
 , "/body/string found at key when number expected.json"
 , "/body/string found in array when number expected.json"
 , "/body/unexpected index with not null value.json"
 , "/body/unexpected index with null value.json"
 , "/body/unexpected key with not null value.json"
 , "/body/unexpected key with null value.json"
 ]

main :: IO ()
main = mapM_ (checkCase . (testBase ++)) tests