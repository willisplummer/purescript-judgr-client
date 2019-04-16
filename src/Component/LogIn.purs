module Component.LogIn where

import Prelude

import Data.Argonaut.Core as J
import Simple.JSON as JSON
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (setHref)

type SignupPostBody = {
  username :: String,
  email :: String,
  password :: String
}

type State =
  { loading :: Boolean
  , username :: String
  , email :: String
  , password :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | SetPassword String a
  | SetEmail String a
  | MakeRequest a
  | MakeAuthdRequest a

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
  initialState = { loading: false, username: "", email: "", password: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_
      [ HH.h1_ [ HH.text "Sign Up For Judgr" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter email:" ]
          , HH.input
              [ HP.value st.email
              , HE.onValueInput (HE.input SetEmail)
              ]
          ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter password:" ]
          , HH.input
              [ HP.value st.password
              , HE.onValueInput (HE.input SetPassword)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Register" ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeAuthdRequest)
          ]
          [ HH.text "authd request" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      , HH.a [ HP.href "#link-a" ] [ HH.text "Link A" ] 
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    SetUsername username next -> do
      H.modify_ (_ { username = username, result = Nothing })
      pure next
    SetEmail email next -> do
      H.modify_ (_ { email = email, result = Nothing })
      pure next
    SetPassword password next -> do
      H.modify_ (_ { password = password, result = Nothing })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      password <- H.gets _.password
      email <- H.gets _.email
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"],
        url = "http://localhost:8080/signup",
        method = Left POST,
        responseFormat = AXRF.json,
        content = Just (AXRB.string (JSON.writeJSON { username: username, email: email, password: password })),
        withCredentials = true
      })
      liftEffect $ setHref' "/loggedin"
      H.modify_ (_ { loading = false, result = hush $ J.stringify <$> response.body })
      pure next
    MakeAuthdRequest next -> do
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/users",
        method = Left GET,
        responseFormat = AXRF.json,
        withCredentials = true
      })
      H.modify_ (_ { loading = false, result = hush $ J.stringify <$> response.body })
      pure next
    where
      setHref' :: String -> Effect Unit
      setHref' href = do
        loc <- location =<< window
        setHref href loc
