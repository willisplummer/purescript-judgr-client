module Utils.SetHref where

import Prelude

import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location as WHL
import Effect (Effect)

setHref :: String -> Effect Unit
setHref href = do
  loc <- location =<< window
  WHL.setHref href loc
