module Main where

import VexTab.Score
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Prelude (Unit, bind, discard, (<>), ($))
import VexTab.Abc.Score (renderParsedAbc)
import Data.Abc.Parser (parse)

config :: Config
config =
  { canvasDivId : "#vextab"
  , canvasX : 10
  , canvasY : 10
  , canvasWidth : 1200
  , scale : 0.8
  }

sampleAbc :: String
sampleAbc =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:2/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 A4 A4 |\r\n"

main :: forall e. Eff (vt :: VEXTAB, console :: CONSOLE | e) Unit
main = do
  initialised <- initialise config
  log "initialised?"
  logShow initialised
  log "rendered?"
  rendered <- renderParsedAbc $ parse sampleAbc
  logShow rendered
