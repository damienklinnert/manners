{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ServiceSpec where

import Data.Aeson
import Control.Lens
import Network.Wreq
import Test.Hspec
import Control.Exception (bracket)
import Control.Concurrent

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
      let b1 = encode $ Pact.Interaction "a sample interaction" (Just "stateless") (Pact.Request "get" "/sample" Nothing Nothing Nothing) (Pact.Response (Just 201) Nothing Nothing)
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