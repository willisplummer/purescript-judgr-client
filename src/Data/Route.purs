module Data.Route where

import Prelude
import Control.Alternative ((<|>))
import Routing.Match (Match, lit, int, str, end, root)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))

data Route =
  Login
  | Judgeable

myRoute :: Match Route
myRoute = root *> oneOf
  [ Login <$ lit "login"
    , Judgeable <$ end
  ]

maybeMyRoute :: Match (Maybe Route)
maybeMyRoute = oneOf
  [ Just <$> myRoute
  , pure Nothing
  ]
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
