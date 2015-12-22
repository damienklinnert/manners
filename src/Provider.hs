module Provider
 ( FakeProvider()
 , activeInteractions
 , verifiedInteractions
 , addInteraction
 , setInteractions
 , resetInteractions
 , findInteractionForRequest
 , recordRequest
 , verifyInteractions
 , getVerifiedInteractions
 , FakeProviderState
 , newFakeProviderState
 , run
 ) where

import Control.Concurrent.STM
import Control.Monad.State

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

addInteraction :: P.Interaction -> State FakeProvider ([P.Interaction])
addInteraction i = do
  modify $ \p -> p { activeInteractions = (i : activeInteractions p) }
  gets $ \p -> activeInteractions p

setInteractions :: [P.Interaction] -> State FakeProvider ()
setInteractions is = modify $ \p -> p { activeInteractions = is }

resetInteractions :: State FakeProvider ()
resetInteractions = modify $ \p -> p { activeInteractions = [], mismatchedRequests = [], matchedInteractions = [] }

findInteractionForRequest :: P.Request -> State FakeProvider ([(P.Interaction, [P.ValidationError])])
findInteractionForRequest req = gets $ \p -> map toTuple $ activeInteractions p
  where
    toTuple :: P.Interaction -> (P.Interaction, [P.ValidationError])
    toTuple interaction = (interaction, P.validateRequest (P.interactionRequest interaction) req)

addInteractionMatch :: P.Interaction -> State FakeProvider ()
addInteractionMatch i = modify $ \p -> p
 { matchedInteractions = (i : matchedInteractions p)
 , verifiedInteractions = (i : verifiedInteractions p)
 }

addMismatchedRequest :: P.Request -> State FakeProvider ()
addMismatchedRequest i = modify $ \p -> p { mismatchedRequests = (i : mismatchedRequests p) }

recordRequest :: P.Request -> State FakeProvider (Either [(P.Interaction, [P.ValidationError])] P.Interaction)
recordRequest req = do
  interactions <- findInteractionForRequest req
  let (successful, failed) = L.partition (\(_, errors) -> errors == []) interactions
  case successful of
    -- we only care about the first match and ignore later matches
    ((interaction, _):_) -> do
      addInteractionMatch interaction
      pure (Right interaction)
    [] -> do
      addMismatchedRequest req
      pure (Left failed)

verifyInteractions :: State FakeProvider (Bool, [P.Request], [P.Interaction], [P.Interaction])
verifyInteractions = gets $ \p ->
  let isSuccess = (length $ mismatchedRequests p) == 0 && (length $ matchedInteractions p) == (length $ activeInteractions p)
  in (isSuccess, mismatchedRequests p, matchedInteractions p, activeInteractions p) where

getVerifiedInteractions :: State FakeProvider [P.Interaction]
getVerifiedInteractions = gets verifiedInteractions

-- Transactions

type FakeProviderState = TVar FakeProvider
type FakeProviderTX a = FakeProviderState -> STM (a, FakeProvider)

newFakeProviderState :: IO FakeProviderState
newFakeProviderState = newTVarIO initialFakeProvider

liftTX :: State FakeProvider a -> FakeProviderTX a
liftTX m v = do
  (a, s) <- runState m <$> readTVar v
  writeTVar v s
  return (a, s)  

runTX :: FakeProviderState -> FakeProviderTX a -> IO a
runTX fps tx = atomically (tx fps) >>= \(a, _) -> pure a

run :: FakeProviderState -> State FakeProvider a -> IO a
run fps = runTX fps . liftTX
