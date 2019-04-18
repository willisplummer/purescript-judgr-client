module Router where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP

import Effect.Aff.Class (class MonadAff)

import Component.LogIn as LogIn
import Component.SignUp as SignUp
import Component.Judgeable as Judgeable
import Component.Users as Users

type Input = Maybe Route

type State = { route :: Maybe Route }

data Query a = Navigate (Maybe Route) a

type ChildQuery = Coproduct4 LogIn.Query Judgeable.Query SignUp.Query Users.Query
type ChildSlot = Either4 Unit Unit Unit Unit

cpLogin :: CP.ChildPath LogIn.Query ChildQuery Unit ChildSlot
cpLogin = CP.cp1

cpJudgeable :: CP.ChildPath Judgeable.Query ChildQuery Unit ChildSlot
cpJudgeable = CP.cp2

cpSignUp :: CP.ChildPath SignUp.Query ChildQuery Unit ChildSlot
cpSignUp = CP.cp3

cpUsers :: CP.ChildPath Users.Query ChildQuery Unit ChildSlot
cpUsers = CP.cp4

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
      SignUp ->
        HH.slot' cpSignUp unit SignUp.ui unit absurd
      Judgeable ->
        HH.slot' cpJudgeable unit Judgeable.ui unit absurd
      Users ->
        HH.slot' cpUsers unit Users.ui unit absurd


