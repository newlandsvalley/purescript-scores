module VexTab.Score
  (Config, initialise, render) where

import Effect (Effect)

-- | the configuration of the VexTab Canvas
type Config =
    { canvasDivId :: String
    , canvasX :: Int
    , canvasY :: Int
    , canvasWidth :: Int
    , scale :: Number
    }

foreign import initialise :: Config -> Effect Boolean

foreign import render :: String -> Effect Boolean
