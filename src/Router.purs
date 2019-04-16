module Router where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP

import Effect.Aff.Class (class MonadAff)

import Component.LogIn as LogIn
import Component.Judgeable as Judgeable

type Input = Maybe Route

type State = { route :: Maybe Route }

data Query a = Navigate (Maybe Route) a

type ChildQuery = Coproduct2 LogIn.Query Judgeable.Query
type ChildSlot = Either2 Unit Unit

cpLogin :: CP.ChildPath LogIn.Query ChildQuery Unit ChildSlot
cpLogin = CP.cp1

cpJudgeable :: CP.ChildPath Judgeable.Query ChildQuery Unit ChildSlot
cpJudgeable = CP.cp2

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

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get 
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Nothing -> HH.div_ [ HH.text "NOT FOUND" ]
    Just justRoute -> case justRoute of
      Login -> 
        HH.slot' cpLogin unit LogIn.ui unit absurd
      Judgeable -> 
        HH.slot' cpJudgeable unit Judgeable.ui unit absurd


