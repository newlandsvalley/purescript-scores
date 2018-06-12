module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.VexTab.Score (translateSuite)

main :: Effect Unit
main = runTest do
  suite "scores" do
    translateSuite
