module VexTab.Abc.Score
  (renderTune, renderParsedAbc) where

import Prelude (not, pure, bind, (<>))
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Abc (AbcTune)
import VexTab.Score as VexScore
import VexTab.Abc.Translate (translate)
import VexTab.Abc.Canonical (toScoreText)

-- | attempt to render a parsed AbcTune to a visible score
renderTune :: AbcTune -> Effect Boolean
renderTune tune =
  let
    vexScore = translate tune
  in
    case vexScore of
      Right score ->
        do
          let
            text = toScoreText score
          rendered <- VexScore.render text
          -- log the error if we can't translate the vex score
          if (not rendered)
            then do
              _ <- log ("vex text in error: " <> text)
              pure rendered
            else
              pure rendered
      Left err -> do
        pure false

-- | attempt to render parsed ABC (only if the parse was successful)
renderParsedAbc :: forall e. (Either e AbcTune) -> Effect Boolean
renderParsedAbc eitherTune =
  case eitherTune of
    Right tune ->
      renderTune tune
    Left _ ->
      pure false
