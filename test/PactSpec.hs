{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Aeson
import Pact


data TestCase = TestCase
 { match :: Bool
 , comment :: String
 , expected :: Request
 , actual :: Request
 } deriving (Show, Eq)

instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$> v .: "match" <*> v .: "comment" <*> v .: "expected" <*> v .: "actual"
  parseJSON _ = error "could not parse TestCase"

checkCase :: String -> IO ()
checkCase file = do
  methodDifferent <- BL.readFile file
  let (Just x) = decode methodDifferent :: Maybe TestCase
  let expectedVal = (expected x)
  let actualVal = (actual x)
  let success = (diffRequests expectedVal actualVal) == (match x)
  putStrLn $ (if success then "SUCCESS " else "FAIL ") ++ file ++ " - " ++ (comment x)
  when (not success) exitFailure

testBase :: String
testBase = "resources/pact_specification_v1.1/testcases/request"

tests :: [String]
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