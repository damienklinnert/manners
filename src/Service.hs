{-# LANGUAGE OverloadedStrings #-}
module Service
 ( runProviderService
 , Port
 ) where

import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Control.Concurrent.MVar as M
import Data.Aeson as Aeson
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as Warp

import qualified Pact as Pact
import qualified Provider as Provider

type Port = Int

runProviderService :: Port -> IO ()
runProviderService p = do
  putStrLn $ "Listening on port " ++ (show p)
  providerState <- M.newMVar $ Provider.initialFakeProvider
  Warp.run p (providerService providerState)

providerService :: M.MVar Provider.FakeProvider -> W.Application
providerService providerState request respond =
  case route of

    ("POST", ["interactions"], True) -> do
      body <- W.strictRequestBody request
      let (Just interaction) = Aeson.decode body :: Maybe Pact.Interaction
      putStrLn (show interaction)
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Add interaction"

    ("PUT", ["interactions"], True) -> do
      body <- W.strictRequestBody request
      let (Just interactionWrapper) = Aeson.decode body :: Maybe InteractionWrapper
      let interactions = wrapperInteractions interactionWrapper
      putStrLn (show interactions)
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Sets all interactions"

    ("DELETE", ["interactions"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Delete registered interactions"

    ("GET", ["interactions", "verification"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Verify set-up interactions"

    ("POST", ["pact"], True) -> do
      body <- W.strictRequestBody request
      let (Just contactDesc) = Aeson.decode body :: Maybe ContractDescription
      putStrLn (show contactDesc)
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Persist verified interactions as contract"

    _ -> do
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Default Handler"

  where route = (W.requestMethod request, W.pathInfo request, isAdminRequest)
        isAdminRequest =
          case L.find hasAdminHeader (W.requestHeaders request)
            of (Just _) -> True
               _ -> False
        hasAdminHeader (h, v) = h == "X-Pact-Mock-Service" && v == "True"


data InteractionWrapper = InteractionWrapper { wrapperInteractions :: [Pact.Interaction] } deriving (Show)
instance Aeson.FromJSON InteractionWrapper where
  parseJSON (Object v) = InteractionWrapper <$> v .: "interactions"

data ServiceDescription = ServiceDescription { serviceName :: String } deriving (Show)
instance Aeson.FromJSON ServiceDescription where
  parseJSON (Object v) = ServiceDescription <$> v .: "name"

data ContractDescription = ContractDescription
 { contractConsumer :: ServiceDescription
 , contractProvider :: ServiceDescription
 } deriving (Show)
instance Aeson.FromJSON ContractDescription where
  parseJSON (Object v) = ContractDescription <$> v .: "consumer" <*> v .: "provider"