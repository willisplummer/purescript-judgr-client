module Router where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)

import Component.LogIn as LogIn

type Input = String

type State = { history :: Array String, route :: String }

data Query a = ChangeRoute String a

data Slot = LogInSlot
derive instance eqLogInSlot :: Eq Slot
derive instance ordLogInSlot :: Ord Slot

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: String -> State
  initialState = \initialRoute -> { history: [], route: initialRoute }

  render :: State -> H.ParentHTML Query LogIn.Query Slot m
  render state =
    HH.div_
      [ HH.slot LogInSlot LogIn.ui unit (const Nothing)
      , HH.text state.route
      ]

  eval :: Query ~> H.ParentDSL State Query LogIn.Query Slot Void m
  eval = case _ of
    ChangeRoute msg next -> do
      H.modify_ \st -> { history: st.history `A.snoc` msg, route: st.route }
      pure next
