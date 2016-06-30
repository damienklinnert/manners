{-# LANGUAGE OverloadedStrings #-}
module Runner
 ( runContract
 , Path
 ) where

import qualified System.Exit as S
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Pact as P
import qualified Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import qualified Control.Monad as C
import qualified Data.Maybe as M
import qualified Network.Wreq as W
import Control.Lens

type Path = String
type BaseUrl = String

runContract :: Path -> BaseUrl -> IO ()
runContract path baseUrl = do
  contents <- BL.readFile path
  let (Just contract) = A.decode contents :: Maybe P.ContractDescription
  Pretty.putDoc $ mconcat
    [ Pretty.indent 2 $ Pretty.contractStart path contract
    , Pretty.line
    , Pretty.line
    ]

  let interactions = P.contractInteractions contract
  C.forM_ interactions $ \i -> do
    Pretty.putDoc $ mconcat
      [ Pretty.indent 4 $ Pretty.verifyStart i
      , Pretty.line
      , Pretty.line
      ]

    setupState $ P.interactionState i

    let request = P.interactionRequest i
    let expectedResponse = P.interactionResponse i

    actualResponse <- performRequest baseUrl request

    let errors = P.validateResponse expectedResponse actualResponse
    C.when (not $ null errors) $ do
      Pretty.putDoc $ mconcat
        [ Pretty.indent 8 $ Pretty.validationErrors errors
        , Pretty.line
        , Pretty.line
        ]
      S.exitFailure

    teardownState $ P.interactionState i

setupState :: Maybe String -> IO ()
setupState (Just state) = W.post "http://localhost:2345/post" (getPayload "setup" state) >> pure ()
setupState Nothing      = W.post "http://localhost:2345/post" (getPayload "setup" "__MANNERS_DEFAULT_STATE__") >> pure ()

teardownState :: Maybe String -> IO ()
teardownState (Just state) = W.post "http://localhost:2345/post" (getPayload "teardown" state) >> pure ()
teardownState Nothing      = W.post "http://localhost:2345/post" (getPayload "teardown" "__MANNERS_DEFAULT_STATE__") >> pure ()

getPayload :: T.Text -> String -> BL.ByteString
getPayload hook state = A.encode $ A.object [(A..=) "type" (A.String hook), (A..=) "state" (A.String (T.pack state))]

performRequest :: BaseUrl -> P.Request -> IO P.Response
performRequest baseUrl req = performMethod method url opts body
  where
    method = CI.foldedCase . CI.mk $ P.requestMethod req
    headers = P.convertHeadersFromJson $ P.requestHeaders req
    url = baseUrl ++ P.requestPath req ++ P.queryString True (P.requestQuery req)
    opts = (W.defaults & W.checkStatus .~ (Just (\_ _ _-> Nothing)) & W.headers .~ headers)
    body = (M.fromMaybe "" ((P.requestBody req) >>= (\x -> return $ A.encode x)))

performMethod :: String -> String -> W.Options -> BL.ByteString -> IO P.Response
performMethod "get" url options _ = serialiseResponse <$> W.getWith options url
performMethod "post" url options body = serialiseResponse <$> W.postWith options url body
performMethod "put" url options body = serialiseResponse <$> W.putWith options url body
performMethod "delete" url options _ = serialiseResponse <$> W.deleteWith options url
performMethod "head" url options _ = serialiseResponseEmptyBody <$> W.headWith options url
performMethod "options" url options _ = serialiseResponseEmptyBody <$> W.optionsWith options url
performMethod _ _ _ _ = error "method not supported"

serialiseResponse :: W.Response BL.ByteString -> P.Response
serialiseResponse wRes = P.Response (Just actualStatus) actualHeaders (A.decode actualBody)
  where
    actualStatus = (wRes ^. W.responseStatus . W.statusCode)
    actualHeaders = P.convertHeadersToJson (wRes ^. W.responseHeaders)
    actualBody = (wRes ^. W.responseBody)

serialiseResponseEmptyBody :: W.Response () -> P.Response
serialiseResponseEmptyBody wRes = P.Response (Just actualStatus) actualHeaders Nothing
   where
     actualStatus = (wRes ^. W.responseStatus . W.statusCode)
     actualHeaders = P.convertHeadersToJson (wRes ^. W.responseHeaders)
