module Data.Route where

import Prelude
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root, print)
import Routing.Duplex.Generic (noArgs, sum)

data Route = Login | SignUp | Judgeable | Users

derive instance genericRoute :: Generic Route _

stringifyRoute :: Route -> String
stringifyRoute = print route

route :: RouteDuplex' Route
route = root $ sum
  { "Judgeable": noArgs
  , "SignUp": path "signup" noArgs
  , "Login": path "login" noArgs
  , "Users": path "users" noArgs
  }

derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
