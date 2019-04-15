module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Data.Route (Route(..), maybeMyRoute)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as Router
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window
import Routing.Hash (getHash, matches)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  initialHash <- liftEffect $ getHash 
  io <- runUI Router.component Nothing body
  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  void $ liftEffect $ matches maybeMyRoute \old new ->
  when (old /= Just new) do
    launchAff_ $ io.query $ H.action $ Router.Navigate new
  pure unit
