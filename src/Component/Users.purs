module Component.Users (State, Query(..), Response(..), User(..), ui) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader as AXRH
import Simple.JSON as JSON

data Input a
  = Noop a

type User =
  { name :: String
  , email :: String
  , id :: Int
  }

type Response = Array User

type State =
  { loading :: Boolean
  , result :: Maybe Response
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
  initialState = { loading: false, result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ [
      HH.h1_ [ HH.text "USERS" ],
      HH.div_
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ userComponents ]
            ]
      ]
      where
      userComponents :: Array (H.ComponentHTML Query)
      userComponents =
        case st.result of
        Just r -> map (\u -> HH.div_ [ HH.text u.name ] ) r
        Nothing -> []

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/users",
        method = Left GET,
        responseFormat = AXRF.string,
        withCredentials = true
      })

      case lmap transformError response.body >>= JSON.readJSON of
        Right (r :: Response) -> do
          H.modify_ (_ {
            loading = false,
            result = Just r
          })
        Left e -> do
          H.modify_ (_ {
            loading = false,
            result = Nothing
          })

      pure next

transformError (AXRF.ResponseFormatError e _) = singleton e
