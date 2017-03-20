module VexTab.Score
  (Config, VEXTAB, initialise, render) where

import Control.Monad.Eff (Eff)

-- | the configuration of the VexTab Canvas
type Config =
    { canvasDivId :: String
    , canvasX :: Int
    , canvasY :: Int
    , canvasWidth :: Int
    , scale :: Number
    }

-- | VEXTAB Effect
foreign import data VEXTAB :: !

foreign import initialise :: forall eff. Config -> Eff (vt :: VEXTAB | eff) Boolean

foreign import render :: forall eff. String -> Eff (vt :: VEXTAB | eff) Boolean
