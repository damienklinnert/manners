{-# LANGUAGE OverloadedStrings #-}
module PactSpec where

import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Aeson
import Pact

data RequestTestCase = RequestTestCase
 { requestMatch :: Bool
 , requestComment :: String
 , requestExpected :: Request
 , requestActual :: Request
 } deriving (Show, Eq)

instance FromJSON RequestTestCase where
  parseJSON (Object v) = RequestTestCase <$> v .: "match" <*> v .: "comment" <*> v .: "expected" <*> v .: "actual"
  parseJSON _ = error "could not parse RequestTestCase"

data ResponseTestCase = ResponseTestCase
 { responseMatch :: Bool
 , responseComment :: String
 , responseExpected :: Response
 , responseActual :: Response
 } deriving (Show, Eq)

instance FromJSON ResponseTestCase where
  parseJSON (Object v) = ResponseTestCase <$> v .: "match" <*> v .: "comment" <*> v .: "expected" <*> v .: "actual"
  parseJSON _ = error "could not parse ResponseTestCase"

testBase :: String
testBase = "resources/pact_specification_v1.1/testcases"

requestTestBase :: String
requestTestBase = testBase ++ "/request"

responseTestBase :: String
responseTestBase = testBase ++ "/response"

requestTests :: [String]
requestTests =
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

responseTests :: [String]
responseTests =
 [ "/status/different status.json"
 , "/status/matches.json"

 , "/headers/empty headers.json"
 , "/headers/header name is different case.json"
 , "/headers/header value is different case.json"
 , "/headers/matches.json"
 , "/headers/order of comma separated header values different.json"
 , "/headers/unexpected header found.json"
 , "/headers/whitespace after comma different.json"

 , "/body/array in different order.json"
 , "/body/deeply nested objects.json"
 , "/body/different value found at index.json"
 , "/body/different value found at key.json"
 , "/body/keys out of order match.json"
 , "/body/matches.json"
 , "/body/missing index.json"
 , "/body/missing key.json"
 , "/body/not null found at key when null expected.json"
 , "/body/not null found in array when null expected.json"
 , "/body/null found at key where not null expected.json"
 , "/body/null found in array when not null expected.json"
 , "/body/number found at key when string expected.json"
 , "/body/number found in array when string expected.json"
 , "/body/objects in array first matches.json"
 , "/body/objects in array no matches.json"
 , "/body/objects in array second matches.json"
 , "/body/property name is different case.json"
 , "/body/string found at key when number expected.json"
 , "/body/string found in array when number expected.json"
 , "/body/unexpected index with not null value.json"
 , "/body/unexpected index with null value.json"
 , "/body/unexpected key with not null value.json"
 , "/body/unexpected key with null value.json"
 ]

checkRequestTestCase :: String -> IO ()
checkRequestTestCase file = do
  content <- BL.readFile file
  let (Just x) = decode content :: Maybe RequestTestCase
  let expectedVal = (requestExpected x)
  let actualVal = (requestActual x)
  let success = null (validateRequest expectedVal actualVal) == (requestMatch x)
  putStrLn $ (if success then "SUCCESS " else "FAIL ") ++ file ++ " - " ++ (requestComment x)
  when (not success) exitFailure

checkResponseTestCase :: String -> IO ()
checkResponseTestCase file = do
  content <- BL.readFile file
  let (Just x) = decode content :: Maybe ResponseTestCase
  let expectedVal = (responseExpected x)
  let actualVal = (responseActual x)
  print expectedVal
  print actualVal
  let success = null (validateResponse expectedVal actualVal) == (responseMatch x)
  putStrLn $ (if success then "SUCCESS " else "FAIL ") ++ file ++ " - " ++ (responseComment x)
  when (not success) exitFailure

main :: IO ()
main = do
  mapM_ (checkRequestTestCase . (requestTestBase ++)) requestTests
  mapM_ (checkResponseTestCase . (responseTestBase ++)) responseTests