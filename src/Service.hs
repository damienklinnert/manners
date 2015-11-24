{-# LANGUAGE OverloadedStrings #-}
module Service
 ( runProviderService
 , Port
 ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as E
import qualified Control.Concurrent.MVar as M
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as D
import qualified Data.CaseInsensitive as CI

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
      putStrLn "Setup interaction"
      body <- W.strictRequestBody request
      let (Just interaction) = Aeson.decode body :: Maybe Pact.Interaction
      putStrLn (show interaction)
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.addInteraction fakeProvider interaction )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Add interaction"

    ("PUT", ["interactions"], True) -> do
      putStrLn "Set interactions"
      body <- W.strictRequestBody request
      let (Just interactionWrapper) = Aeson.decode body :: Maybe InteractionWrapper
      let interactions = wrapperInteractions interactionWrapper
      putStrLn (show interactions)
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.setInteractions fakeProvider interactions )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Sets all interactions"

    ("DELETE", ["interactions"], True) -> do
      putStrLn "Reset interactions"
      M.modifyMVar_ fakeProviderState (\fakeProvider -> return $ Provider.resetInteractions fakeProvider )
      respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Delete registered interactions"

    ("GET", ["interactions", "verification"], True) -> do
      putStrLn "Verify interactions"
      fakeProvider <- M.readMVar fakeProviderState
      let isSuccessful = Provider.verifyInteractions fakeProvider
      putStrLn (show $ isSuccessful)
      let responseCode = if isSuccessful then H.status200 else H.status500
      respond $ W.responseLBS responseCode [("Content-Type", "text/plain")] "Verify set-up interactions"

    ("POST", ["pact"], True) -> do
      putStrLn "Write pact"
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
      putStrLn "Default handler"
      body <- W.strictRequestBody request
      let inMethod = C.unpack $ W.requestMethod request
      let inPath = filter (/='?') $ C.unpack $ W.rawPathInfo request
      let inQuery = filter (/='?') $ C.unpack $ W.rawQueryString request
      let inHeaders = convertHeadersToJson $ W.requestHeaders request
      let inBody = decode body
      let inputRequest = Pact.Request inMethod inPath inQuery inHeaders inBody

      putStrLn (show inputRequest)

      fakeProvider <- M.readMVar fakeProviderState
      let maybeInteraction = Provider.findInteractionForRequest fakeProvider inputRequest
      let fakeProvider' = case maybeInteraction of (Just interaction) -> Provider.addInteractionMatch fakeProvider interaction
                                                   Nothing            -> Provider.addMismatchedRequest fakeProvider inputRequest
      M.modifyMVar_ fakeProviderState (\_ -> return fakeProvider')

      respond $ case maybeInteraction of
        (Just interaction) -> let response          = Pact.interactionResponse interaction
                                  status            = toEnum $ case Pact.responseStatus response of
                                                        (Just statusCode) -> statusCode
                                                        Nothing           -> 200
                                  headers           = case Pact.responseHeaders response of
                                                        (Just headers) -> convertHeadersFromJson headers
                                                        Nothing        -> []
                                  body              = case Pact.responseBody response of
                                                        (Just body)    -> encode body
                                                        Nothing        -> ""
                              in W.responseLBS status headers body
        Nothing -> W.responseLBS H.status500 [] ""

  where route = (W.requestMethod request, W.pathInfo request, isAdminRequest)
        isAdminRequest =
          case L.find hasAdminHeader (W.requestHeaders request)
            of (Just _) -> True
               _ -> False
        hasAdminHeader (h, v) = CI.mk h == CI.mk "X-Pact-Mock-Service" && CI.mk v == CI.mk "True"


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

convertHeadersToJson :: [H.Header] -> Object
convertHeadersToJson headers = HM.fromList $ map toTextPair headers
  where toTextPair (k, v) = (E.decodeUtf8 $ CI.foldedCase k, String $ E.decodeUtf8 v)

convertHeadersFromJson :: Object -> [H.Header]
convertHeadersFromJson headers = map fromTextPair $ HM.toList headers
  where fromTextPair (k, (String v)) = (CI.mk $ E.encodeUtf8 k, E.encodeUtf8 v)