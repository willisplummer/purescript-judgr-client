module Component.Judgeable (State, Query(..), ui) where

import Prelude

import Data.Argonaut.Core as J
import Simple.JSON as JSON
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH

data Input a
  = Noop a

type State =
  { loading :: Boolean
  , result :: Maybe String
  }

data Query a
  =  Initialize a

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: true, result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st = HH.div_ [ HH.h1_ [ HH.text "JUDGE" ] ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/judgeables",
        method = Left GET,
        responseFormat = AXRF.json,
        withCredentials = true
      })
      H.modify_ (_ { loading = false, result = hush $ J.stringify <$> response.body })
      pure next
