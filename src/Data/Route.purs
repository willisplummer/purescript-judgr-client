module Data.Route where

import Prelude
import Control.Alternative ((<|>))
import Routing.Match (Match, lit, int, str, end, root)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))

data Route =
  Login
  | SignUp
  | Judgeable
  | Users

myRoute :: Match Route
myRoute = root *> oneOf
  [ Login <$ lit "login"
  , SignUp <$ lit "signup"
  , Judgeable <$ end
  , Users <$ lit "users"
  ]

maybeMyRoute :: Match (Maybe Route)
maybeMyRoute = oneOf
  [ Just <$> myRoute
  , pure Nothing
  ]
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
