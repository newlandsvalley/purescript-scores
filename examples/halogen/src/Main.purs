module Main where

import Prelude

import Effect (Effect)
import Halogen.ScoreComponent (component, Message(..)) as Score
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = HA.runHalogenAff
  do
    body <- HA.awaitBody
    io <- runUI Score.component unit body
    pure Nothing
