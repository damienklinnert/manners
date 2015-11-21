{-# LANGUAGE OverloadedStrings #-}
module Pact where

import Data.Char (toUpper)
import qualified Data.ByteString.Lazy as B
import Data.Aeson

data TestCase = TestCase
 { match :: Bool
 , comment :: String
 , expected :: RequestDesc
 , actual :: RequestDesc
 } deriving (Show, Eq)

instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$> v .: "match" <*> v .: "comment" <*> v .: "expected" <*> v .: "actual"

checkCase :: String -> IO ()
checkCase file = do
  methodDifferent <- B.readFile file
  let (Just x) = decode methodDifferent :: Maybe TestCase
  let exp = (expected x)
  let act = (actual x)
  let matches = verifyRequest exp act
  putStrLn $ (if matches == (match x) then "WIN " else "FAIL ") ++ file

data RequestDesc = RequestDesc
 { method :: String
 , path :: String
 , query :: String
 , headers :: Value
 } deriving (Show, Eq)

instance FromJSON RequestDesc where
  parseJSON (Object v) = RequestDesc <$> v .: "method" <*> v .: "path" <*> v .: "query" <*> v .: "headers"

type Diff = Bool

main :: IO ()
main = do
  checkCase "resources/pact_specification_v1.1/testcases/request/method/different method.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/method/matches.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/method/method is different case.json"

  checkCase "resources/pact_specification_v1.1/testcases/request/path/empty path found when forward slash expected.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/path/forward slash found when empty path expected.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/path/incorrect path.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/path/matches.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/path/missing trailing slash in path.json"
  checkCase "resources/pact_specification_v1.1/testcases/request/path/unexpected trailing slash in path.json"

  return ()

verifyRequest :: RequestDesc -> RequestDesc -> Diff
verifyRequest expected actual = foldl1 (&&) [
    verifyMethod (method expected) (method actual),
    verifyPath (path expected) (path actual)
  ]

verifyMethod :: String -> String -> Diff
verifyMethod expected actual = uppercase expected == uppercase actual
  where uppercase = map toUpper

verifyPath :: String -> String -> Diff
verifyPath expected actual = expected == actual