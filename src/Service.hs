{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Service
 ( runProviderService
 , Port
 ) where

import System.IO (stdout, hFlush)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as EP
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified System.Directory as D
import qualified Data.CaseInsensitive as CI

import qualified Pact as Pact
import qualified Provider as Provider

keyOrder :: [T.Text]
keyOrder =
 [ "consumer", "provider", "interactions"
 , "description", "provider_state", "request", "response"
 , "status"
 , "method", "path", "query", "headers", "body"
 ]

encodePrettyCfg :: EP.Config
encodePrettyCfg = EP.Config { EP.confIndent = 4, EP.confCompare = EP.keyOrder keyOrder }

type Port = Int

runProviderService :: Port -> IO ()
runProviderService p = do
  putStrLn $ "manners: listening on port " ++ (show p)
  hFlush stdout
  fakeProviderState <- Provider.newFakeProviderState
  Warp.run p (RequestLogger.logStdoutDev (providerService fakeProviderState))

providerService :: Provider.FakeProviderState -> W.Application
providerService fakeProviderState request respond =
  case route of

    ("POST", ["interactions"], True) -> do
      body <- W.strictRequestBody request

      let maybeInteraction = Aeson.decode body :: Maybe Pact.Interaction
      case maybeInteraction of
        Just interaction -> do
          interactions <- Provider.run fakeProviderState $ Provider.addInteraction interaction
          respond . responseData $ object ["interactions" .= interactions]
        Nothing ->
          respond $ responseError APIErrorInvalidInput

    ("PUT", ["interactions"], True) -> do
      body <- W.strictRequestBody request

      let maybeInteractionWrapper = Aeson.decode body :: Maybe Pact.InteractionWrapper
      case maybeInteractionWrapper of
        Just interactionWrapper -> do
          let interactions = Pact.wrapperInteractions interactionWrapper
          Provider.run fakeProviderState (Provider.setInteractions interactions)
          respond . responseData $ object ["interactions" .= interactions]

        Nothing ->
          respond $ responseError APIErrorInvalidInput

    ("DELETE", ["interactions"], True) -> do
      Provider.run fakeProviderState $
        Provider.resetInteractions
      respond . responseData $ object ["interactions" .= ()]

    ("GET", ["interactions", "verification"], True) -> do
      (isSuccessful, mismatchedRequests, matchedInteractions, activeInteractions) <- Provider.run fakeProviderState $
        Provider.verifyInteractions
      respond $ if isSuccessful then responseData Null else responseError $ APIErrorVerifyFailed mismatchedRequests matchedInteractions activeInteractions

    ("POST", ["pact"], True) -> do
      body <- W.strictRequestBody request

      let maybeContractDesc = Aeson.decode body :: Maybe Pact.ContractDescription
      case maybeContractDesc of
        Just contractDesc -> do
          verifiedInteractions <- Provider.run fakeProviderState $ Provider.getVerifiedInteractions
          let contract = contractDesc { Pact.contractInteractions = reverse verifiedInteractions }
          let marshalledContract = EP.encodePretty' encodePrettyCfg contract
          let fileName = "pact/" ++ (Pact.serviceName . Pact.contractConsumer $ contract) ++ "-" ++ (Pact.serviceName . Pact.contractProvider $ contract) ++ ".json"
          D.createDirectoryIfMissing True "pact"
          BL.writeFile fileName marshalledContract
          respond $ responseData (object ["contractPath" .= fileName, "contract" .= contract])
        Nothing ->
          respond $ responseError APIErrorInvalidInput

    _ -> do
      encodedBody <- W.strictRequestBody request
      let inMethod = C.unpack $ W.requestMethod request
      let inPath = filter (/='?') $ C.unpack $ W.rawPathInfo request
      let inQuery = Pact.Query $ H.parseSimpleQuery $ W.rawQueryString request
      let inHeaders = Pact.convertHeadersToJson $ W.requestHeaders request
      let inBody = Aeson.decode encodedBody
      let inputRequest = Pact.Request inMethod inPath inQuery inHeaders inBody

      eitherInteraction <- Provider.run fakeProviderState $ Provider.recordRequest inputRequest

      respond $ case eitherInteraction of
        (Right interaction) -> let response          = Pact.interactionResponse interaction
                                   resStatus         = toEnum $ case Pact.responseStatus response of
                                                         (Just statusCode) -> statusCode
                                                         Nothing           -> 200
                                   resHeaders        = Pact.convertHeadersFromJson $ Pact.responseHeaders response
                                   resBody           = case Pact.responseBody response of
                                                         (Just body)    -> encode body
                                                         Nothing        -> ""
                               in W.responseLBS resStatus resHeaders resBody
        (Left []) -> responseError APIErrorNoInteractionsConfigured
        (Left failures) -> responseError $ APIErrorNoInteractionMatch inputRequest failures

  where route = (W.requestMethod request, W.pathInfo request, isAdminRequest)
        isAdminRequest =
          case L.find hasAdminHeader (W.requestHeaders request)
            of (Just _) -> True
               _ -> False
        hasAdminHeader (h, v) = CI.mk h == CI.mk "X-Pact-Mock-Service" && CI.mk v == CI.mk "True"

responseData :: forall a. (ToJSON a) => a -> W.Response
responseData dat = W.responseLBS H.status200 [("Content-Type", "application/json")] $ encodeAPI (APIResponseSuccess dat :: APIResponse a ())

responseError :: APIError -> W.Response
responseError err = W.responseLBS H.status500 [("Content-Type", "application/json")] $ encodeAPI (APIResponseFailure err :: APIResponse () APIError)

encodeAPI :: (ToJSON a, ToJSON b) => APIResponse a b -> BL.ByteString
encodeAPI resp = EP.encodePretty' encodeAPICfg resp
  where
    encodeAPICfg :: EP.Config
    encodeAPICfg = EP.Config { EP.confIndent = 4, EP.confCompare = EP.keyOrder cmp }
      where cmp = [ "name", "description", "incommingRequest", "interactions", "interaction", "failedValidations" ] ++ keyOrder

data APIResponse a b = APIResponseSuccess a | APIResponseFailure b
instance (ToJSON a, ToJSON b) => ToJSON (APIResponse a b) where
  toJSON (APIResponseSuccess d) = object ["data" .= d]
  toJSON (APIResponseFailure e) = object ["error" .= e]

data APIError = APIErrorInvalidInput
              | APIErrorVerifyFailed [Pact.Request] [Pact.Interaction] [Pact.Interaction]
              | APIErrorNoInteractionsConfigured
              | APIErrorNoInteractionMatch Pact.Request [(Pact.Interaction, [Pact.ValidationError])]

instance ToJSON APIError where
  toJSON APIErrorInvalidInput = object
    [ "name" .= ("APIErrorInvalidInput" :: String)
    , "description" .= ("The request is invalid or malformed" :: String)
    ]
  toJSON (APIErrorVerifyFailed mismatchedRequests matchedInteractions activeInteractions) = object
    [ "name" .= ("APIErrorVerifyFailed" :: String)
    , "description" .= ("Verification failed" :: String)
    , "mismatchedRequests" .= mismatchedRequests
    , "matchedInteractions" .= matchedInteractions
    , "activeInteractions" .= activeInteractions
    ]
  toJSON APIErrorNoInteractionsConfigured = object
    [ "name" .= ("APIErrorNoInteractionsConfigured" :: String)
    , "description" .= ("No interactions are configured yet" :: String)
    ]
  toJSON (APIErrorNoInteractionMatch req failures) = object
    [ "name" .= ("APIErrorNoInteractionMatch" :: String)
    , "description" .= ("No matching interaction found" :: String)
    , "incommingRequest" .= req
    , "interactions" .= map formatFailure failures
    ]
    where
      formatFailure (interaction, errors) = object ["interaction" .= interaction, "failedValidations" .= errors]
