module Main where

import Prelude (Unit, bind, discard, show, ($), (<>))
import Effect (Effect)
import Effect.Console (log)
import VexTab.Score

config :: Config
config =
  { canvasDivId : "#vextab"
  , canvasX : 10
  , canvasY : 10
  , canvasWidth : 1200
  , scale : 0.8
  }

sampleText :: String
sampleText =
  "stave \n"
        <> "notation=true \n"
        <> "key=G time=3/4 \n"
        <> "notes :q A/4 B/4 :8 C/5 D/5 |  E/5 $.top.$ $1───$ F/5  :q A/4 D/4 =:| :8 E/5 $.top.$ $2───$ F/5 :h ( A/4.D/4 ) |\n"

main :: Effect Unit
main = do
  initialised <- initialise config
  log "initialised?"
  log $ show initialised
  log "rendered?"
  rendered <- render sampleText
  log $ show rendered
