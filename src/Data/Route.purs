module Data.Route where

import Prelude
import Control.Alternative ((<|>))
import Routing.Match (Match, lit, int, str, end)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))

data Route =
  Login

myRoute :: Match Route
myRoute = oneOf
  [ Login <$ lit "login"
  ]

maybeMyRoute :: Match (Maybe Route)
maybeMyRoute = oneOf
  [ Just <$> myRoute
  , pure Nothing
  ]
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
