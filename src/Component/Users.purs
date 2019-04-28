module Component.Users (State, Query(..), UserResponse(..), User(..), Follow(..), FollowResponse(..), ui) where

import Prelude

import Data.Array (find, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Int (toStringAs, decimal)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader as AXRH
import Simple.JSON as JSON

data Input a
  = Noop a

type Follow =
  { id :: Int
  , followerId :: Int
  , followedId :: Int
  }

type FollowResponse = Array Follow

type User =
  { name :: String
  , email :: String
  , id :: Int
  }

type UserResponse = Array User

type State =
  { loading :: Boolean
  , users :: Maybe UserResponse
  , follows :: Maybe FollowResponse
  }

data Query a
  =  Initialize a
  |  Follow Int a
  |  Unfollow Int a

data Slot = Slot

checkFollows :: User -> FollowResponse -> Boolean
checkFollows u fArr = isJust $ find (\f -> f.followedId == u.id) fArr

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
  initialState = { loading: false, users: Nothing, follows: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ [
      HH.h1_ [ HH.text "USERS" ],
      HH.div_
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.div_ userComponents
            ]
      ]
      where
      userComponents :: Array (H.ComponentHTML Query)
      userComponents = map (\u ->
        HH.div_ 
          [
            HH.text u.name
          , (
            if (checkFollows u (fromMaybe [] st.follows))
            then
              HH.button
              [ HP.disabled st.loading
              , HE.onClick (HE.input_ (Unfollow u.id))
              ]
              [ HH.text (if st.loading then "Working..." else "Unfollow") ]
            else
              HH.button
              [ HP.disabled st.loading
              , HE.onClick (HE.input_ (Follow u.id))
              ]
              [ HH.text (if st.loading then "Working..." else "Follow") ])
          ]) (fromMaybe [] st.users)

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      userResponse <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/users",
        method = Left GET,
        responseFormat = AXRF.string,
        withCredentials = true
      })

      followResponse <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/follows",
        method = Left GET,
        responseFormat = AXRF.string,
        withCredentials = true
      })

      H.modify_ (_ {
        loading = false,
        users = hush $ JSON.readJSON =<< lmap transformError userResponse.body,
        follows = hush $ JSON.readJSON =<< lmap transformError followResponse.body
      })

      pure next
    Follow id next -> do
      H.modify_ (_ { loading = true })

      response <- H.liftAff $ AX.request (AX.defaultRequest {
                headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"],
        url = "http://localhost:8080/users/" <> (toStringAs decimal id) <> "/follow",
        method = Left POST,
        responseFormat = AXRF.string,
        content = Nothing,
        withCredentials = true
      })

      case lmap transformError response.body >>= JSON.readJSON of
        Right (r :: Follow) -> do
          H.modify_ (\s -> s { loading = false, follows = Just ((fromMaybe [] s.follows) <> [r]) })
        Left e -> do
          H.modify_ (_ { loading = false })

      pure next
    Unfollow id next -> do
      H.modify_ (_ { loading = true })

      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"],
        url = "http://localhost:8080/users/" <> (toStringAs decimal id) <> "/unfollow",
        method = Left POST,
        responseFormat = AXRF.string,
        content = Nothing,
        withCredentials = true
      })

      -- TODO: Actually check the response before updating state
      H.modify_ (\s -> s {
        loading = false,
        follows = Just (filter (\f -> f.followedId /= id) (fromMaybe [] s.follows))
      })

      pure next

transformError (AXRF.ResponseFormatError e _) = singleton e
