module Test.VexTab.Score (translateSuite) where

import Prelude
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Abc.Parser (parse)
import VexTab.Abc.Translate (translate)
import VexTab.Abc.Canonical (toScoreText)

import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertCanonical :: forall e. String -> String -> Test e
assertCanonical s canonical =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right tune ->
        case (translate tune) of
          Left err ->
            failure ("translation failed: " <> (show err))
          Right score ->
            Assert.equal canonical (toScoreText score)

      Left err ->
        failure ("parse failed: " <> (show err))

translateSuite :: forall t. Free (TestF t) Unit
translateSuite =
  suite "ABC translation" do
    test "notes" do
      assertCanonical "K:C\r\n| ABc |\r\n"
        ((preface "C" ) <> " notes | :8 A/4 :8 B/4 :8 C/5 |\r\n" )
    test "triplet" do
      assertCanonical "K:C\r\n| (3ABC |\r\n"
        ((preface "C" ) <> " notes | :8 A/4 :8 B/4 :8 C/4 ^3,3^ |\r\n" )
    test "triplet with rest" do
      assertCanonical "K:C\r\n| (3zBc |\r\n"
        ((preface "C" ) <> " notes |  :8 ## :8 B/4 :8 C/5 ^3,3^ |\r\n" )
    test "chord" do
      assertCanonical "K:C\r\n| [def] |\r\n"
        ((preface "C" ) <> " notes | :8 ( D/5 E/5 F/5 ) |\r\n" )
    test "broken rhythm >" do
       assertCanonical "K:C\r\n| d>e |\r\n"
         ((preface "C" ) <> " notes | :8d D/5 :16 E/5 |\r\n" )
    test "broken rhythm <" do
      assertCanonical "K:C\r\n| d<e |\r\n"
         ((preface "C" ) <> " notes | :16 D/5 :8d E/5 |\r\n" )
    test "two lines" do
      assertCanonical "K:C\r\n| ABc |\r\n| def |\r\n"
       ((preface "C" )
         <> " notes | :8 A/4 :8 B/4 :8 C/5 |\r\n"
         <> (stave "C")
         <> " notes | :8 D/5 :8 E/5 :8 F/5 |\r\n"
       )
    -- more to follow

preface :: String -> String
preface key =
  "options beam-rests=false\r\n" <> (stave key)

stave :: String -> String
stave key =
  "stave notation=true clef=treble key="
     <> key <> "  \r\n"
