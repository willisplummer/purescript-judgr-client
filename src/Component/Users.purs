module Component.Users (State, Query(..), ui) where

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
  =  GetJudgeable a

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st = HH.div_ [ HH.h1_ [ HH.text "USERS" ] ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    GetJudgeable next -> do
      pure next
