module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Route (route)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as Router
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  nav <- liftEffect $ makeInterface
  io <- runUI Router.component Nothing body

  void $ liftEffect $ nav # matchesWith (parse route) \old new ->
  when (old /= Just new) do
    launchAff_ $ io.query $ H.action $ Router.Navigate (Just new)
  pure unit
