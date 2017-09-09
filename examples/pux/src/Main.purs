module Main where


import Data.Monoid (mempty)
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, bind, const, discard, pure, ($), (<>))
import Pux (EffModel, noEffects, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, h1, button, canvas)
import Text.Smolder.HTML.Attributes (id)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import CSS.Geometry (margin)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, center)
import CSS.Font (fontSize)
import Signal.Channel (CHANNEL)

import VexTab.Score as VexScore

data Event
  = NoOp
  | RequestInitialiseVex
  | VexInitialised Boolean
  | RequestRender
  | VexRendered Boolean

type State =
  { initialised :: Boolean
  , rendered :: Boolean
  }

initialState :: State
initialState =
  { initialised : false
  , rendered : false
  }

foldp :: Event -> State -> EffModel State Event (vt :: VexScore.VEXTAB)
foldp NoOp state =  noEffects $ state
foldp RequestInitialiseVex state =
  { state: state
    , effects:
     [ do
         initialiseVex state
     ]
  }
foldp (VexInitialised initialised) state =
  noEffects $ state { initialised = initialised }
foldp RequestRender state =
  { state: state
    , effects:
     [ do
         rendered <- liftEff $ VexScore.render sampleText
         pure $ Just (VexRendered rendered)
     ]
  }
foldp (VexRendered rendered) state =
  noEffects $ state { rendered = rendered }

initialiseVex :: forall e. State -> Aff (vt :: VexScore.VEXTAB | e) (Maybe Event)
initialiseVex state =
  let
    config :: VexScore.Config
    config =
      { canvasDivId : "#vextab"
      , canvasX : 10
      , canvasY : 10
      , canvasWidth : 1200
      , scale : 0.8
      }
  in
    do
      initialised <- liftEff (VexScore.initialise (config))
      pure $ Just (VexInitialised initialised)

{-}
debugVex :: State -> HTML Event
debugVex state =
  do
    text ("vex rendered: " <> show state.rendered)
    text (" vex initialised: " <> show state.initialised)
-}

view :: State -> HTML Event
view state =
   div do
     div do
       h1 ! centreStyle $ text "Text vex with Pux"
       button ! buttonStyle  #! onClick (const RequestInitialiseVex) $ text "initialise"
       button ! buttonStyle  #! onClick (const RequestRender) $ text "render"
     canvas ! id "vextab" $ mempty
     -- debugVex state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

buttonStyle :: Attribute
buttonStyle =
  style do
    margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
    fontSize (em 1.0)

sampleText :: String
sampleText =
  "stave \n"
        <> "notation=true \n"
        <> "key=G time=3/4 \n"
        <> "notes :q A/4 B/4 :8 C/5 D/5 |  E/5 $.top.$ $1───$ F/5  :q A/4 D/4 =:| :8 E/5 $.top.$ $2───$ F/5 :h ( A/4.D/4 ) |\n"

main :: Eff (channel :: CHANNEL, exception :: EXCEPTION, vt :: VexScore.VEXTAB ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
