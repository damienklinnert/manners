module Provider
 ( FakeProvider()
 , initialFakeProvider
 , activeInteractions
 , verifiedInteractions
 , addInteraction
 , setInteractions
 , resetInteractions
 , findInteractionForRequest
 , addInteractionMatch
 , addMismatchedRequest
 , verifyInteractions
 ) where

import qualified Data.List as L
import qualified Pact as P

data FakeProvider = FakeProvider
 { activeInteractions :: [P.Interaction]
 , matchedInteractions :: [P.Interaction]
 , verifiedInteractions :: [P.Interaction]
 , mismatchedRequests :: [P.Request]
 } deriving (Show)

initialFakeProvider :: FakeProvider
initialFakeProvider = FakeProvider [] [] [] []

addInteraction :: FakeProvider -> P.Interaction -> FakeProvider
addInteraction p i = p { activeInteractions = (i : activeInteractions p) }

setInteractions :: FakeProvider -> [P.Interaction] -> FakeProvider
setInteractions p is = p { activeInteractions = is }

resetInteractions :: FakeProvider -> FakeProvider
resetInteractions p = p { activeInteractions = [], mismatchedRequests = [] }

findInteractionForRequest :: FakeProvider -> P.Request -> Maybe P.Interaction
findInteractionForRequest p req = L.find (\i -> P.diffRequests (P.interactionRequest i) req) $ activeInteractions p

addInteractionMatch :: FakeProvider -> P.Interaction -> FakeProvider
addInteractionMatch p i = p
 { matchedInteractions = (i : matchedInteractions p)
 , verifiedInteractions = (i : verifiedInteractions p)
 }

addMismatchedRequest :: FakeProvider -> P.Request -> FakeProvider
addMismatchedRequest p i = p { mismatchedRequests = (i : mismatchedRequests p) }

verifyInteractions :: FakeProvider -> Bool
verifyInteractions p = (length $ mismatchedRequests p) == 0 && (length $ matchedInteractions p) == (length $ activeInteractions p)