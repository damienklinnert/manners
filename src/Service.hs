{-# LANGUAGE OverloadedStrings #-}
module Service
 ( runProviderService
 , Port
 ) where

import qualified Data.List as L
import qualified Control.Concurrent.MVar as M
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Handler.Warp as Warp

import qualified Pact as Pact
import qualified Provider as Provider

type Port = Int

runProviderService :: Port -> IO ()
runProviderService p = do
  putStrLn $ "Listening on port " ++ (show p)
  Warp.run p providerService

providerService :: W.Application
providerService request respond =
  case route of

    ("POST", ["interactions"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Adds a new interaction"

    ("PUT", ["interactions"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Sets all interactions"

    ("DELETE", ["interactions"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Delete registered interactions"

    ("GET", ["interactions", "verification"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Verify set-up interactions"

    ("POST", ["pact"], True) -> respond $
      W.responseLBS H.status200 [("Content-Type", "text/plain")] "Persist verified interactions as contract"

    _ -> respond $ W.responseLBS H.status200 [("Content-Type", "text/plain")] "Default Handler"

  where route = (W.requestMethod request, W.pathInfo request, isAdminRequest)
        isAdminRequest =
          case L.find hasAdminHeader (W.requestHeaders request)
            of (Just _) -> True
               _ -> False
        hasAdminHeader (h, v) = h == "X-Pact-Mock-Service" && v == "True"