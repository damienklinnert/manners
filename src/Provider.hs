module Provider
 ( FakeProvider()
 , initialFakeProvider
 , activeInteractions
 , verifiedInteractions
 , addInteraction
 , setInteractions
 , resetInteractions
 , addInteractionMatch
 , addInteractionMismatch
 , verifyInteractions
 ) where

import qualified Pact as P

data FakeProvider = FakeProvider
 { activeInteractions :: [P.Interaction]
 , matchedInteractions :: [P.Interaction]
 , verifiedInteractions :: [P.Interaction]
 , failedInteractions :: [P.Interaction]
 } deriving (Show)

initialFakeProvider :: FakeProvider
initialFakeProvider = FakeProvider [] [] [] []

addInteraction :: FakeProvider -> P.Interaction -> FakeProvider
addInteraction p i = p { activeInteractions = (i : activeInteractions p) }

setInteractions :: FakeProvider -> [P.Interaction] -> FakeProvider
setInteractions p is = p { activeInteractions = is }

resetInteractions :: FakeProvider -> FakeProvider
resetInteractions p = p { activeInteractions = [], failedInteractions = [] }

addInteractionMatch :: FakeProvider -> P.Interaction -> FakeProvider
addInteractionMatch p i = p
 { matchedInteractions = (i : matchedInteractions p)
 , verifiedInteractions = (i : verifiedInteractions p)
 }

addInteractionMismatch :: FakeProvider -> P.Interaction -> FakeProvider
addInteractionMismatch p i = p { failedInteractions = (i : failedInteractions p) }

verifyInteractions :: FakeProvider -> Bool
verifyInteractions p = (length $ failedInteractions p) == 0 && (length $ matchedInteractions p) == (length $ activeInteractions p)