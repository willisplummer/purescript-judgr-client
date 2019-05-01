module Component.Users (State, Query(..), UserResponse(..), User(..), Follow(..), FollowResponse(..), ui) where

import Prelude

import Data.Array (find, filter)
import Data.Bifunctor (lmap)
import Data.HTTP.Method (Method(..))
import Data.Int (toStringAs, decimal)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.List.NonEmpty (singleton)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader as AXRH
import Affjax.StatusCode as AXSC
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

data State
  = Loading
  | Failed
  | Loaded {
      users :: UserResponse
    , follows :: FollowResponse
    }

data Query a
  =  Initialize a
  |  Follow Int a
  |  Unfollow Int a

data Slot = Slot

checkFollows :: User -> FollowResponse -> Boolean
checkFollows u fArr = isJust $ find (\f -> f.followedId == u.id) fArr

userComponents :: UserResponse -> FollowResponse -> Array (H.ComponentHTML Query)
userComponents users follows = map (\u ->
  HH.div_ 
    [
      HH.text u.name
    , (
      if (checkFollows u follows)
      then
        HH.button
        [ HP.disabled false
        , HE.onClick (HE.input_ (Unfollow u.id))
        ]
        [ HH.text (if false then "Working..." else "Unfollow") ]
      else
        HH.button
        [ HP.disabled false
        , HE.onClick (HE.input_ (Follow u.id))
        ]
        [ HH.text (if false then "Working..." else "Follow") ])
    ]) users

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
  initialState = Loading

  render :: State -> H.ComponentHTML Query
  render st =
      HH.div_ [
        HH.h1_ [ HH.text "USERS" ],
        case st of
          Loading -> HH.div_ [ HH.text "loading"]
          Failed -> HH.div_ [ HH.text "failed" ]
          Loaded r ->
            HH.div_ $ userComponents r.users r.follows
      ]



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

      case lmap transformError userResponse.body >>= JSON.readJSON of
        Right (users :: UserResponse) ->
          case lmap transformError followResponse.body >>= JSON.readJSON of
            Right (follows :: FollowResponse) ->
              H.modify_ (\s -> Loaded { users: users, follows: follows })
            Left e ->
              H.modify_ (\s -> Failed)
        Left e -> do
          H.modify_ (\s -> Failed)

      pure next
    Follow id next -> do
      state <- H.get
      case state of
        Loaded records -> do
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
              H.modify_ (\s -> Loaded (records { follows = (records.follows <> [r]) }))
            Left e -> do
              H.modify_ (\s -> Failed)
        _ -> pure unit
      pure next

    Unfollow id next -> do
      state <- H.get
      case state of
        Loaded records -> do
          response <- H.liftAff $ AX.request (AX.defaultRequest {
            headers = [AXRH.RequestHeader "Accept" "application/json",
            AXRH.RequestHeader "Content-Type" "application/json"],
            url = "http://localhost:8080/users/" <> (toStringAs decimal id) <> "/unfollow",
            method = Left POST,
            responseFormat = AXRF.string,
            content = Nothing,
            withCredentials = true
          })

          if response.status == AXSC.StatusCode 204
            then
              H.modify_ (\s -> Loaded records {
                follows = filter (\f -> f.followedId /= id) records.follows
              })
            else
              pure unit
        _ -> pure unit
      pure next

transformError (AXRF.ResponseFormatError e _) = singleton e
