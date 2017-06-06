module VexTab.Abc.Canonical
  (toScoreText) where

import Prelude (($), (<>), (>), map, show)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List, foldl, intercalate, length)
import Data.Either (Either(..))
import Data.Tuple (fst, snd)
import Data.Foldable (foldMap)
import Data.Abc
        ( Accidental(..)
        , Mode(..)
        , Bar
        , Thickness(..)
        , Repeat(..)
        )
import VexTab.Abc.VexScore

data NoteContext
    = Staved
    | Tupleted
    | Chordal

-- | translate the VEX score to a VexTab String
toScoreText :: Score -> String
toScoreText score =
  let
    f :: String -> VexBodyPart -> String
    f acc vl =
      acc <> vexBodyPart vl
  in
    options
      <> (foldl f "" score)

-- implementation

eol :: String
eol =
    "\r\n"

-- | concatenate strings and space them simply -
nicelySpace :: Array String -> String
nicelySpace xs =
  intercalate " " xs

options :: String
options =
  "options beam-rests=false\r\n"

vexBodyPart :: VexBodyPart -> String
vexBodyPart bp =
  case bp of
    VLine line ->
      vexLine line

    _ ->
      -- VContextChange or VEmptyLine
      ""

vexLine :: VexLine -> String
vexLine vl =
  vexStave vl.stave <> (vexItems vl.items) <> "\r\n"

vexStave :: Maybe VexStave -> String
vexStave mvs =
  case mvs of
    Just vs ->
      let
        clef =
          "clef=" <> (show vs.clef)
        time =
          case vs.mMeter of
            Just m ->
              "time=" <> show (fst m) <> "/" <> show (snd m)
            _ ->
              ""
        key =
          case vs.mKey of
            Just k ->
              let
                accidental' =
                  headerAccidental k.accidental
                md =
                  mode k.mode
              in
                "key=" <> show k.pitchClass <> accidental' <> md

            _ ->
              ""
      in
        (nicelySpace [ "stave notation=true", clef, key, time, eol, "notes" ])

    Nothing ->
      " notes"

vexItems :: List VexItem -> String
vexItems vis =
  foldMap vexItem vis

vexItem :: VexItem -> String
vexItem vi =
  case vi of
    VBar bar ->
      vexBar bar

    VNote vnote ->
      vexNote Staved vnote

    VRest duration ->
      vexRest duration

    VTuplet size vnotes ->
        " "
          <> (intercalate " " $ map vexRestOrNote vnotes)
          <> " ^"
          <> show size
          <> ","
          <> show (length vnotes)
          <> "^"

    VChord dur vnotes ->
      let
        chordDur =
          show dur
      in
        " "
          <> chordDur
          <> " ( "
          <> (intercalate " " $ map (vexNote Chordal) vnotes)
          <> " )"

    VNotePair vnote1 vnote2 ->
      vexNote Staved vnote1
        <> vexNote Staved vnote2

    VIgnore ->
      ""

vexRest :: VexRest -> String
vexRest r =
  let
    dur =
      show r.duration

    rest =
      "##"
  in
    nicelySpace [ "", dur, rest ]


vexNote :: NoteContext -> VexNote -> String
vexNote ctx vnote =
  let
    accident =
      fromMaybe "" $ map accidental vnote.accidental

    pitch =
      show vnote.pitchClass
        <> accident
        <> "/"
        <> show vnote.octave

    dur =
      show vnote.duration

    tie =
      if vnote.tied then
        "T"
      else
        ""

    decor =
      vexDecoration vnote
  in
    case ctx of
      Chordal ->
        pitch

      Tupleted ->
        nicelySpace [ dur, pitch ]

      _ ->
        if vnote.tied then
          nicelySpace [ "", dur, tie, pitch ] <> decor
        else
          nicelySpace [ "", dur, pitch ] <> decor

vexRestOrNote :: VexRestOrNote -> String
vexRestOrNote vrn =
  case vrn of
    Left r ->
      vexRest r
    Right n ->
      vexNote Tupleted n

accidental :: Accidental -> String
accidental a =
  case a of
    Sharp ->
      "#"

    Flat ->
      "@"

    DoubleSharp ->
      "##"

    DoubleFlat ->
      "@@"

    Natural ->
      "n"

vexBar :: Bar -> String
vexBar b =
  case b.repeat of
    Just Begin ->
      " =|:"

    Just End ->
      " =:|"

    Just BeginAndEnd ->
      " =::"

    Nothing ->
      case b.thickness of
        Thin ->
          " |"

        _ ->
         " =||"

headerAccidental :: Accidental -> String
headerAccidental a =
  case a of
    Sharp ->
      "#"

    Flat ->
      "b"

    _ ->
      ""

mode :: Mode -> String
mode m =
  case m of
    Major ->
      ""
    Minor ->
      "m"
    Ionian ->
      ""
    Aeolian ->
      "m"
    -- we need to trap this in translate - probably by converting modes to canonical forms
    _ ->
      "error not supported"

vexDecoration :: VexNote -> String
vexDecoration v =
  let
    formatDecoration :: Boolean -> String -> String
    formatDecoration isTop vexCode =
      let
        position =
          if isTop then
            "/top"
          else
            "/bottom"
      in
        " $.a" <> vexCode <> position <> ".$"

    isTopPosition =
      (v.octave > 4)
  in
    case v.decoration of
      -- staccato
      Just "." ->
        formatDecoration isTopPosition "."

      -- fermata
      Just "H" ->
        formatDecoration true "@a"

      -- accent
      Just "L" ->
        formatDecoration true ">"

      -- up bow
      Just "u" ->
        formatDecoration true "|"

      -- down bow
      Just "v" ->
        formatDecoration true "m"

      -- gross hack for 1st and 2nd repeats
      Just "1" ->
        " $.top.$ $1───$"

      Just "2" ->
        " $.top.$ $2───$"

      _ ->
        ""
