{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ServiceSpec where

import Data.Aeson
import Control.Lens
import Network.Wreq
import Test.Hspec
import Control.Exception (bracket)
import Control.Concurrent
import Text.Heredoc
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Pact as Pact
import qualified Service as Service

adminOpts :: Options
adminOpts = defaults & header "X-Pact-Mock-Service" .~ ["True"] & checkStatus .~ Just (\_ _ _ -> Nothing)

allowServerErrorOpts :: Options
allowServerErrorOpts = defaults & checkStatus .~ Just (\_ _ _ -> Nothing)

withFakeProvider :: IO () -> IO ()
withFakeProvider action = bracket (startFakeProvider 2345) stopFakeProvider (const action)

startFakeProvider :: Int -> IO ThreadId
startFakeProvider p = forkIO $ Service.runProviderService p

stopFakeProvider :: ThreadId -> IO ()
stopFakeProvider t = killThread t

main :: IO ()
main = hspec $ around_ withFakeProvider $ do
  describe "Functional Tests" $ do
    it "smoke test" $ do
      -- setup an interaction
      let b1 = encode $ Pact.Interaction
                          "a sample interaction"
                          (Just "stateless")
                          (Pact.Request "get" "/sample" Nothing Nothing Nothing)
                          (Pact.Response (Just 201) Nothing Nothing)
      r1 <- postWith adminOpts "http://localhost:2345/interactions" b1
      (r1 ^. responseStatus . statusCode) `shouldBe` 200

      -- do the interaction
      r2 <- get "http://localhost:2345/sample"
      (r2 ^. responseStatus . statusCode) `shouldBe` 201

      -- ensure that verify works
      r3 <- getWith adminOpts "http://localhost:2345/interactions/verification"
      (r3 ^. responseStatus . statusCode) `shouldBe` 200

      -- do unspecified request
      r4 <- getWith allowServerErrorOpts "http://localhost:2345/no-sample"
      (r4 ^. responseStatus . statusCode) `shouldBe` 500

      -- ensure that verify fails
      r5 <- getWith adminOpts "http://localhost:2345/interactions/verification"
      (r5 ^. responseStatus . statusCode) `shouldBe` 500

      -- delete all interactions
      r6 <- deleteWith adminOpts "http://localhost:2345/interactions"
      (r6 ^. responseStatus . statusCode) `shouldBe` 200

      -- verify should work again now
      r7 <- getWith adminOpts "http://localhost:2345/interactions/verification"
      (r7 ^. responseStatus . statusCode) `shouldBe` 200

      -- set interations to another one
      let b8 = encode $ Pact.InteractionWrapper
                         [ Pact.Interaction
                           "another interaction"
                           (Just "some_state")
                           (Pact.Request "GET" "/sample/call" Nothing Nothing Nothing)
                           (Pact.Response (Just 400) (Just $ HM.fromList [("x-header", "here"), ("content-type", "application/json")]) (Just "bodycontent"))
                         ]
      r8 <- putWith adminOpts "http://localhost:2345/interactions" b8
      (r8 ^. responseStatus . statusCode) `shouldBe` 200

      -- verification should fail now
      r9 <- getWith adminOpts "http://localhost:2345/interactions/verification"
      (r9 ^. responseStatus . statusCode) `shouldBe` 500

      -- do the proper interaction now
      r10 <- getWith allowServerErrorOpts "http://localhost:2345/sample/call"
      (r10 ^. responseStatus . statusCode) `shouldBe` 400
      (r10 ^. responseHeader "x-header") `shouldBe` "here"
      (r10 ^. responseHeader "content-type") `shouldBe` "application/json"
      (r10 ^. responseBody) `shouldBe` "\"bodycontent\""

      -- now verify should work again
      r11 <- getWith adminOpts "http://localhost:2345/interactions/verification"
      (r11 ^. responseStatus . statusCode) `shouldBe` 200

      -- persist contract
      let b12 = encode $ Pact.ContractDescription (Pact.ServiceDescription "a") (Pact.ServiceDescription "b") []
      r12 <- postWith adminOpts "http://localhost:2345/pact" b12
      (r12 ^. responseStatus . statusCode) `shouldBe` 200

      -- check that the contract was written properly
      modelContents <- BL.readFile "test/resources/a-b.json"
      generatedContents <- BL.readFile "pact/a-b.json"
      let (Just model) = decode modelContents :: Maybe Pact.ContractDescription
      let (Just generated) = decode generatedContents :: Maybe Pact.ContractDescription
      model `shouldBe` generated

    it "another smoke test" $ do
      let b1 = encode $ Pact.InteractionWrapper
                         [ Pact.Interaction
                           "a request for hello"
                           Nothing
                           (Pact.Request "get" "/sayHello" Nothing Nothing Nothing)
                           (Pact.Response
                             (Just 200)
                             (Just $ HM.fromList [("Content-Type", "application/json")])
                             (Just $ Object $ HM.fromList [("reply", String "Hello")]))
                         ]
      r1 <- putWith adminOpts "http://localhost:2345/interactions" b1
      (r1 ^. responseStatus . statusCode) `shouldBe` 200

      r2 <- get "http://localhost:2345/sayHello"
      (r2 ^. responseStatus . statusCode) `shouldBe` 200
      (decode (r2 ^. responseBody)) `shouldBe` (Just $ Object $ HM.fromList [("reply", String "Hello")])

      let b3 = encode $ Pact.ContractDescription
                           (Pact.ServiceDescription "Hello Consumer")
                           (Pact.ServiceDescription "Hello Provider")
                           []
      r3 <- postWith adminOpts "http://localhost:2345/pact" b3
      (r3 ^. responseStatus . statusCode) `shouldBe` 200

      let b4 = BLC.pack [str|{ "interactions": [{
                            |  "description": "a request friends",
                            |  "request": {
                            |    "method": "get",
                            |    "path": "/friends",
                            |    "query": { "age": "30", "children": ["Mary Jane", "James"] },
                            |    "headers": { "Accept": "application/json" }
                            |  },
                            |  "response": {
                            |    "status": 200,
                            |    "headers": { "Content-Type": "application/json" },
                            |    "body": { "friends": [{"name": "Sue"}] }
                            |  }
                            |}]}
                            |]
      r4 <- putWith adminOpts "http://localhost:2345/interactions" b4
      (r4 ^. responseStatus . statusCode) `shouldBe` 200

      let r5opts = (allowServerErrorOpts &
                      param "age" .~ ["30"] &
                      param "children" .~ ["Mary Jane", "James"] &
                      header "Accept" .~ ["application/json"])
      r5 <- getWith r5opts "http://localhost:2345/friends"
      (r5 ^. responseStatus . statusCode) `shouldBe` 200
      ((decode $ r5 ^. responseBody) :: Maybe Object) `shouldBe` (decode $ BLC.pack [str|{ "friends": [{"name": "Sue"}] }|])

      r6 <- postWith adminOpts "http://localhost:2345/pact" b3
      (r6 ^. responseStatus . statusCode) `shouldBe` 200

      let b7 = BLC.pack [str|{ "interactions": [{
                                  |  "description": "a request to unfriend",
                                  |  "provider_state": "I am friends with Fred",
                                  |  "request": {
                                  |    "method": "put",
                                  |    "path": "/unfriendMe"
                                  |  },
                                  |  "response": {
                                  |    "status": 200,
                                  |    "headers": { "Content-Type": "application/json" },
                                  |    "body": { "reply": "Bye" }
                                  |  }
                                  |}]}
                                  |]
      r7 <- putWith adminOpts "http://localhost:2345/interactions" b7
      (r7 ^. responseStatus . statusCode) `shouldBe` 200

      r8 <- put "http://localhost:2345/unfriendMe" BL.empty
      (r8 ^. responseStatus . statusCode) `shouldBe` 200
      ((decode $ r8 ^. responseBody) :: Maybe Object) `shouldBe` (decode $ BLC.pack [str|{ "reply": "Bye" }|])

      r9 <- postWith adminOpts "http://localhost:2345/pact" b3
      (r9 ^. responseStatus . statusCode) `shouldBe` 200

      let b10 = BLC.pack [str|{ "interactions": [{
                                        |  "description": "a request to unfriend",
                                        |  "provider_state": "I have no friends",
                                        |  "request": {
                                        |    "method": "put",
                                        |    "path": "/unfriendMe"
                                        |  },
                                        |  "response": {
                                        |    "status": 404
                                        |  }
                                        |}]}
                                        |]
      r10 <- putWith adminOpts "http://localhost:2345/interactions" b10
      (r10 ^. responseStatus . statusCode) `shouldBe` 200

      r11 <- putWith allowServerErrorOpts "http://localhost:2345/unfriendMe" BL.empty
      (r11 ^. responseStatus . statusCode) `shouldBe` 404

      r12 <- postWith adminOpts "http://localhost:2345/pact" b3
      (r12 ^. responseStatus . statusCode) `shouldBe` 200

      modelContents <- BL.readFile "test/resources/Hello Consumer-Hello Provider.json"
      generatedContents <- BL.readFile "pact/Hello Consumer-Hello Provider.json"
      let (Just model) = decode modelContents :: Maybe Pact.ContractDescription
      let (Just generated) = decode generatedContents :: Maybe Pact.ContractDescription
      model `shouldBe` generated