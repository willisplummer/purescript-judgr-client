module Component.Judgeable (State, Judgeable(..), JudgeableResponse, Judgement(..), JudgementResponse, Query(..), ui) where

import Prelude

import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Int (toStringAs, decimal)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader as AXRH
import Affjax.RequestBody as AXRB
import Simple.JSON as JSON

type Judgeable =
  { id :: Int
  , name :: String
  , imageUrl :: String
  }

type JudgeableResponse = Array Judgeable

type Judgement =
  { id :: Int
  , userId :: Int
  , judgeableId :: Int
  , isGood :: Boolean
  }

type JudgementResponse = Array Judgement

data Input a
  = Noop a

type State =
  { loading :: Boolean
  , judgements :: Maybe JudgementResponse
  , judgeables :: Maybe JudgeableResponse
  }

data Query a
  = Initialize a
  | Judge Int Boolean a

data Slot = Slot

getJudgement :: Judgeable -> JudgementResponse -> Maybe Judgement
getJudgement judgeable judgementArr = find (\f -> f.judgeableId == judgeable.id) judgementArr

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
  initialState = { loading: true, judgements: Nothing, judgeables: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ [
      HH.h1_ [ HH.text "JUDGE" ],
      HH.div_
        case st.judgeables of
          Nothing ->
            [ HH.h2_
              [ HH.text "Nothing To Judge" ]
            ]
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.div_ judgeableComponents
            ]
      ]
      where
      judgeableComponents :: Array (H.ComponentHTML Query)
      judgeableComponents = map (\judgeable ->
        HH.div_ 
          [
            HH.text judgeable.name
          , (
            case (getJudgement judgeable (fromMaybe [] st.judgements)) of
              Nothing ->
                HH.div_ [
                  HH.button
                    [ HP.disabled st.loading
                    , HE.onClick (HE.input_ (Judge judgeable.id false))
                    ]
                    [ HH.text "Shit" ]
                  , HH.button
                    [ HP.disabled st.loading
                    , HE.onClick (HE.input_ (Judge judgeable.id true))
                    ]
                    [ HH.text "Good" ]
                ]
              Just judgement ->
                HH.div_ [
                  HH.text (if judgement.isGood then "Judgement: Good" else "Judgement: Shit")
                ]
            )
          ]) (fromMaybe [] st.judgeables)

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      judgeableResponse <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/judgeables",
        method = Left GET,
        responseFormat = AXRF.string,
        withCredentials = true
      })
      judgementResponse <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"
        ],
        url = "http://localhost:8080/judgements",
        method = Left GET,
        responseFormat = AXRF.string,
        withCredentials = true
      })

      H.modify_ (_ {
        loading = false,
        judgeables = hush $ JSON.readJSON =<< lmap transformError judgeableResponse.body,
        judgements = hush $ JSON.readJSON =<< lmap transformError judgementResponse.body
      })

      pure next
    Judge id isGood next -> do
      H.modify_ (_ { loading = true })

      response <- H.liftAff $ AX.request (AX.defaultRequest {
        headers = [AXRH.RequestHeader "Accept" "application/json",
        AXRH.RequestHeader "Content-Type" "application/json"],
        url = "http://localhost:8080/judgeables/" <> (toStringAs decimal id) <> "/judgements",
        method = Left POST,
        responseFormat = AXRF.string,
        content = Just (AXRB.string (JSON.writeJSON { isGood: isGood })),
        withCredentials = true
      })
      
      case lmap transformError response.body >>= JSON.readJSON of
        Right (r :: Judgement) -> do
          H.modify_ (\s -> s { loading = false, judgements = Just ((fromMaybe [] s.judgements) <> [r]) })
        Left e -> do
          H.modify_ (_ { loading = false })

      pure next

transformError (AXRF.ResponseFormatError e _) = singleton e
