module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Route (maybeMyRoute)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as Router
import Routing.PushState (makeInterface, matches)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  nav <- liftEffect $ makeInterface
  io <- runUI Router.component Nothing body
  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  void $ liftEffect $ nav # matches maybeMyRoute \old new ->
  when (old /= Just new) do
    launchAff_ $ io.query $ H.action $ Router.Navigate new
  pure unit
