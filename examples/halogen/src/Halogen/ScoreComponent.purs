module Halogen.ScoreComponent where

-- | A halogen component for displaying a music score

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Halogen (IProp)
import VexTab.Score as VexScore


data Query a =
    Initialise a
  | Render a

data Message =
   IsInitialised Boolean
 | IsRendered Boolean

type State =
  {
    isInitialised :: Boolean
  , isRendered :: Boolean
  }

sampleText :: String
sampleText =
  "stave \n"
        <> "notation=true \n"
        <> "key=G time=3/4 \n"
        <> "notes :q A/4 B/4 :8 C/5 D/5 |  E/5 $.top.$ $1───$ F/5  :q A/4 D/4 =:| :8 E/5 $.top.$ $2───$ F/5 :h ( A/4.D/4 ) |\n"

-- initialiseVex :: Aff Booleanf
initialiseVex :: ∀ m. MonadEffect m => m Boolean
initialiseVex =
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
      H.liftEffect (VexScore.initialise config)

component :: H.Component HH.HTML Query Unit Message Aff
component =
  H.component
    { initialState: const initialState
    , render: render
    , eval: eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    {
      isInitialised : false
    , isRendered : true
    }


  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
     [ HH.div_
       [
         HH.button
           [ HE.onClick (HE.input_ Initialise)
           , HP.class_ $ ClassName "hoverable"
           ]
           [ HH.text "initialise VexTab" ]
       , HH.button
          [ HE.onClick (HE.input_ Render)
          , HP.class_ $ ClassName "hoverable"
          ]
          [ HH.text "render score" ]
        , HH.canvas
          [ HP.id_ "vextab" ]
       ]
     ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    Initialise next -> do
      isInitialised <- initialiseVex
      _ <- H.modify (\state -> state { isInitialised = isInitialised  } )
      H.raise $ IsInitialised isInitialised
      pure next
    Render next -> do
      isRendered <- H.liftEffect $ VexScore.render sampleText
      _ <- H.modify (\state -> state { isRendered = isRendered  } )
      H.raise $ IsRendered isRendered
      pure next
