module Router where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff.Class (class MonadAff)

import Component.LogIn as LogIn

type Input = Maybe Route

type State = { route :: Maybe Route }

data Query a = Navigate (Maybe Route) a

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

  initialState :: Maybe Route -> State
  initialState = \initialRoute -> { route: initialRoute }

  render :: State -> H.ParentHTML Query LogIn.Query Slot m
  render { route } = case route of
    Nothing -> HH.div_ [ HH.text "NOT FOUND" ]
    Just justRoute -> case justRoute of
      Login -> 
        HH.slot LogInSlot LogIn.ui unit (const Nothing)


  eval :: Query ~> H.ParentDSL State Query LogIn.Query Slot Void m
  eval (Navigate dest a) = do
    { route } <- H.get 
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a
