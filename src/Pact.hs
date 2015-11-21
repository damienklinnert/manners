{-# LANGUAGE OverloadedStrings #-}
module Pact where

import Data.Char (toUpper)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Types as H
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
  methodDifferent <- BL.readFile file
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
 ]

main :: IO ()
main = mapM_ (checkCase . (testBase ++)) tests

verifyRequest :: RequestDesc -> RequestDesc -> Diff
verifyRequest expected actual = foldl1 (&&)
 [ verifyMethod (method expected) (method actual)
 , verifyPath (path expected) (path actual)
 , verifyQuery (query expected) (query actual)
 ]

verifyMethod :: String -> String -> Diff
verifyMethod expected actual = uppercase expected == uppercase actual
  where uppercase = map toUpper

verifyPath :: String -> String -> Diff
verifyPath = (==)

verifyQuery :: String -> String -> Diff
verifyQuery expected actual = toQ expected == toQ actual
  where toQ s = L.sortOn fst $ H.parseSimpleQuery (CS.pack s)