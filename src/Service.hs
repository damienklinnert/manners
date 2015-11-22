{-# LANGUAGE OverloadedStrings #-}
module Service
 ( runProviderService
 , Port
 ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Control.Concurrent.MVar as M
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as D

import qualified Pact as Pact
import qualified Provider as Provider

type Port = Int

runProviderService :: Port -> IO ()
runProviderService p = do
  putStrLn $ "Listening on port " ++ (show p)
  fakeProviderState <- M.newMVar $ Provider.initialFakeProvider
  Warp.run p (providerService fakeProviderState)

providerService :: M.MVar Provider.FakeProvider -> W.Application
providerService fakeProviderState request respond =
  case route of

    ("POST", ["interactions"], True) -> do
      body <- W.strictRequestBody request
      let (Just interaction) = Aeson.decode body :: Maybe Pact.Interaction
      putStrLn (show interaction)
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.addInteraction fakeProvider interaction )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Add interaction"

    ("PUT", ["interactions"], True) -> do
      body <- W.strictRequestBody request
      let (Just interactionWrapper) = Aeson.decode body :: Maybe InteractionWrapper
      let interactions = wrapperInteractions interactionWrapper
      putStrLn (show interactions)
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.setInteractions fakeProvider interactions )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Sets all interactions"

    ("DELETE", ["interactions"], True) -> do
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.resetInteractions fakeProvider )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Delete registered interactions"

    ("GET", ["interactions", "verification"], True) -> do
      fakeProvider <- M.readMVar fakeProviderState
      putStrLn (show $ Provider.verifyInteractions fakeProvider)
      putStrLn (show fakeProvider)
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Verify set-up interactions"

    ("POST", ["pact"], True) -> do
      body <- W.strictRequestBody request
      let (Just contractDesc) = Aeson.decode body :: Maybe ContractDescription
      putStrLn (show contractDesc)
      fakeProvider <- M.readMVar fakeProviderState
      let verifiedInteractions = Provider.verifiedInteractions fakeProvider
      let contract = contractDesc { contractInteractions = verifiedInteractions }
      putStrLn (show contract)
      let marshalledContract = encodePretty contract
      let fileName = "pact/" ++ (serviceName . contractConsumer $ contract) ++ "-" ++ (serviceName . contractProvider $ contract) ++ ".json"
      D.createDirectoryIfMissing True "pact"
      BL.writeFile fileName marshalledContract
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Persist verified interactions as contract"

    _ -> do
      body <- W.strictRequestBody request
      let inMethod = C.unpack $ W.requestMethod request
      let inPath = filter (/='?') $ C.unpack $ W.rawPathInfo request
      let inQuery = filter (/='?') $ C.unpack $ W.rawQueryString request
      let inHeaders = HM.empty -- @TODO populate headers (HM.fromList $ W.requestHeaders request)
      let inBody = decode body
      let inputRequest = Pact.Request inMethod inPath inQuery inHeaders inBody

      putStrLn (show inputRequest)

      fakeProvider <- M.readMVar fakeProviderState

      -- @TODO Find matching interaction

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
instance Aeson.ToJSON ServiceDescription where
  toJSON (ServiceDescription name) = object ["name" .= name]

data ContractDescription = ContractDescription
 { contractConsumer :: ServiceDescription
 , contractProvider :: ServiceDescription
 , contractInteractions :: [Pact.Interaction]
 } deriving (Show)
instance Aeson.FromJSON ContractDescription where
  parseJSON (Object v) = ContractDescription <$> v .: "consumer" <*> v .: "provider" <*> v .:? "interactions" .!= []
instance Aeson.ToJSON ContractDescription where
  toJSON (ContractDescription consumer provider interactions) = object
   [ "consumer" .= consumer
   , "provider" .= provider
   , "interactions" .= interactions
   ]