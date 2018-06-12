module VexTab.Abc.Score
  (renderTune, renderParsedAbc) where

import Prelude (pure, bind, ($))
import Effect (Effect)
import Data.Either (Either(..))
import Data.Abc (AbcTune)
import VexTab.Score as VexScore
import VexTab.Abc.Translate (translate)
import VexTab.Abc.Canonical (toScoreText)

-- | attempt to render a parsed AbcTune to a visible score
renderTune :: AbcTune -> Effect Boolean
renderTune tune =
  let
    vexText = translate tune
  in
    case vexText of
      Right text ->
        do
          rendered <- VexScore.render $ toScoreText text
          pure rendered
      Left err ->
        pure false

-- | attempt to render parsed ABC (only if the parse was successful)
renderParsedAbc :: forall e. (Either e AbcTune) -> Effect Boolean
renderParsedAbc eitherTune =
  case eitherTune of
    Right tune ->
      renderTune tune
    Left _ ->
      pure false
