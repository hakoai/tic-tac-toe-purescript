module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import View.Game (comp)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI comp unit body
