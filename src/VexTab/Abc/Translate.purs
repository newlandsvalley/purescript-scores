module VexTab.Abc.Translate
  (translate, translateText) where

import Prelude (($), (+), (-), (*), (<>), (<<<), map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (List(..), all, head, null, reverse, (:))
import Data.Foldable (foldl)
import Data.Rational (rational, fromInt, toNumber)
import Data.Int (round)
import Data.Abc
import Data.Abc.Canonical as AbcText
import Data.Abc.Parser (parse, PositionedParseError(..))
import Data.Abc.Notation (getKeySig, getMeter, getUnitNoteLength, dotFactor, normaliseModalKey)
import VexTab.Abc.VexScore (Clef(..), Score, VexBodyPart(..), VexDuration(..), VexItem(..), VexNote)

type Context =
    { modifiedKeySig :: ModifiedKeySignature
    , meter :: Maybe MeterSignature
    , unitNoteLength :: NoteDuration
    , tied :: Boolean                     -- tie the next note
    , decoration :: Maybe String          -- decorate the next note (staccato etc)
    , continuation ::  Boolean            -- end of line continuation
    }

-- | translate an ABC tune to a VexTab Score representation
translate :: AbcTune -> Either String Score
translate t =
  let
    ctx =
      initialContext t

    ksmod =
      ctx.modifiedKeySig.modifications
  in
    let
      result =
        tuneBody ctx t.body
    in
      if (null ksmod) then
        case result of
          Right sc ->
            Right (fst sc)

          Left e ->
            Left e
      else
        Left "modified key signatures not supported"

--| translate ABC text to a VexTab Score representation
translateText :: String -> Either String Score
translateText s =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right tune ->
        translate tune

      Left (PositionedParseError ppe) ->
        Left ("parse error: " <> (ppe.error))


--  | translate the tune body
tuneBody :: Context -> TuneBody -> Either String (Tuple Score Context)
tuneBody ctx tb =
  foldOverResult ctx tb bodyPart


bodyPart :: Context -> BodyPart -> Either String (Tuple VexBodyPart Context)
bodyPart ctx bp =
  case bp of
    Score musicline ->
      if emptyLine musicline then
        Right (Tuple VEmptyLine ctx )
      else
        vexLine ctx musicline

    BodyInfo h ->
      let
        newCtx =
          header ctx h
      in
        Right (Tuple VContextChange newCtx )

vexLine :: Context -> MusicLine -> Either String (Tuple VexBodyPart Context )
vexLine ctx line =
  let
    mKey =
      Just (normaliseMode $ ctx.modifiedKeySig.keySignature)

    vexStave =
      if (ctx.continuation) then
        Nothing
      else
        Just { clef : Treble, mKey : mKey, mMeter : ctx.meter }

    {- now we've processed the stave, remove the key signature from the context#
       which we don't need to generate any longer unless there's a key changes
       and now we've processed any possible end-of-line continuation then
       remove it from the comtext
    -}
    staveCtx =
      ctx { meter = Nothing, continuation = false }

    itemsRes =
      musicLine staveCtx line
  in
    case itemsRes of
      Right (Tuple items newCtx ) ->
        Right (Tuple (VLine { stave : vexStave, items : items }) newCtx )

      Left e ->
        Left e

musicLine :: Context -> MusicLine -> Either String (Tuple (List VexItem) Context )
musicLine ctx ml =
  foldOverResult ctx ml music

music :: Context -> Music -> Either String (Tuple VexItem Context )
music ctx m =
  case m of
    Barline bar ->
      let
        newCtx =
          case bar.iteration of
            Just 1 ->
              ctx { decoration = Just "1" }

            Just 2 ->
              ctx { decoration = Just "2" }

            _ ->
              ctx
      in
        Right (Tuple (VBar bar) newCtx )

    Note abcNote ->
      map (\(Tuple vn c ) -> (Tuple (VNote vn) c )) (note ctx abcNote)

    Rest duration ->
      let
        noteDurResult =
          noteDur ctx duration
      in
        case noteDurResult of
          Right d ->
            Right (Tuple (VRest d) ctx )

          Left e ->
            Left ("Rest " <> e <> ": " <> ("rest"))

    Tuplet tupletSignature notes ->
      let
        notesResult =
          noteList ctx notes
      in
        case notesResult of
          Right (Tuple vnotes _ ) ->
            Right (Tuple (VTuplet tupletSignature.p vnotes) ctx )

          Left e ->
            Left e

    Chord abcChord ->
      let
        notesResult =
          noteList ctx abcChord.notes

        nDur =
          firstNoteDuration abcChord.notes

        overallDur =
          (abcChord.duration) * nDur

        chordDurResult =
          noteDur ctx overallDur
      in
        case (Tuple notesResult chordDurResult ) of
            (Tuple (Right (Tuple vnotes _ )) (Right vexd) ) ->
              Right (Tuple (VChord vexd vnotes) ctx )

            (Tuple (Left e) _ ) ->
              Left e

            (Tuple _ (Left e) ) ->
              Left ("Chord " <> e <> ": " <> (AbcText.abcChord abcChord))

    BrokenRhythmPair abcNote1 broken abcNote2 ->
      let
        -- (Tuple bNote1 bNote2 )
        brokenNotes =
          makeBroken broken abcNote1 abcNote2

        note1Result =
          note ctx (fst brokenNotes)

        -- pass the context fron note1 to note 2
        ctx1 =
          case note1Result of
            Right (Tuple  _ n1ctx ) ->
              n1ctx

            _ ->
              ctx

        note2Result =
          note ctx1 (snd brokenNotes)
      in
        case (Tuple note1Result note2Result ) of
          (Tuple (Right (Tuple vnote1 _ )) (Right (Tuple vnote2 ctx2 )) ) ->
            Right (Tuple (VNotePair vnote1 vnote2) ctx2 )

          (Tuple (Left e) _ ) ->
            Left ("Note " <> e <> ": " <> (AbcText.abcNote abcNote1))

          (Tuple _  (Left e) ) ->
              Left ("Note " <> e <> ": " <> (AbcText.abcNote abcNote2))

    Decoration decor ->
      Right( Tuple VIgnore ctx { decoration = Just decor } )

    -- Inline headers not properly supported yet in VexTab
    Inline header' ->
      inlineHeader ctx header'

    Continuation ->
      Right ( Tuple VIgnore ctx { continuation = true } )

    _ ->
      Right (Tuple VIgnore ctx )


note :: Context -> AbcNote -> Either String (Tuple VexNote Context )
note ctx abcNote =
  let
    noteDurResult =
      noteDur ctx abcNote.duration
  in
    case noteDurResult of
      Right d ->
        let
          vexNote =
            { pitchClass : abcNote.pitchClass
            , accidental : abcNote.accidental
            , octave : abcNote.octave - 1
            , duration : d
            , tied : ctx.tied
            {- in ABC, ties attach to the first note in the pair
               but in VexTab, the second
            -}
            , decoration : ctx.decoration
            }

          {- pass the tie to the next note via the context
             and remove any note decoration (which would otherwise
             apply to the next note...
          -}
          newCtx =
            ctx { tied = abcNote.tied, decoration = Nothing }
        in
          -- Ok ( VNote vexNote, ctx )
          Right (Tuple vexNote newCtx )

      Left e ->
        Left ("Note " <> e <> ": " <> (AbcText.abcNote abcNote))



{- translate a note or rest duration, wrapping in a Result which indicates an
   unsupported duration.  This rounds values of 'short enough' note durations
   to the nearest supported value
-}
noteDur :: Context -> NoteDuration -> Either String VexDuration
noteDur ctx d =
  let
    durn =
      round $ toNumber $
         ctx.unitNoteLength * d * (fromInt 128)
  in
    case durn of
      128 ->
        Right Whole

      96 ->
        Right HalfDotted

      64 ->
        Right Half

      48 ->
        Right QuarterDotted

      32 ->
        Right Quarter

      24 ->
        Right EighthDotted

      16 ->
        Right Eighth

      12 ->
        Right SixteenthDotted

      8 ->
        Right Sixteenth

      6 ->
        Right ThirtySecondDotted

      4 ->
        Right ThirtySecond

      3 ->
        Right SixtyFourthDotted

      2 ->
        Right SixtyFourth

      _ ->
        Left "too long or too dotted"



{- apply the specified broken rhythm to each note in the note pair (presented individually)
   and return the broken note pair
-}
makeBroken :: Broken -> AbcNote -> AbcNote -> (Tuple AbcNote AbcNote )
makeBroken broken n1 n2 =
  let
    down i =
      -- Ratio.add (rational 1 1) (Ratio.negate (dotFactor i))
      (rational 1 1) - (dotFactor i)

    up i =
      -- Ratio.add (over 1 1) (dotFactor i)
      (rational 1 1) + (dotFactor i)
  in
    case broken of
      LeftArrow i ->
        let
          left =
            n1 { duration = n1.duration * (down i) }

          right =
            n2 { duration = n2.duration * (up i) }
        in
          (Tuple left right )

      RightArrow i ->
        let
          left =
            n1 { duration = n1.duration * (up i) }

          right =
            n2 { duration = n2.duration * (down i) }
        in
          (Tuple left right )


noteList :: Context -> List AbcNote -> Either String (Tuple (List VexNote) Context )
noteList ctx notes =
  foldOverResult ctx notes note

{- cater for a new header inside the tune body after a line has completed
   we need to cater for changes in key signature, meter or unit note length
   which all alter the translation context.  All other headers may be ignored

   These are headers within the tune body occupying a line of their own
-}
header :: Context -> Header -> Context
header ctx h =
  case h of
    Key mks ->
      ctx { modifiedKeySig = mks }

    UnitNoteLength dur ->
      ctx { unitNoteLength = dur }

    Meter meter ->
      ctx { meter = meter }

    _ ->
      ctx

{- Cater for inline headers (embedded within the growing stave)
   These are not properly supported yet by VexTab and so
   changes in key or time signature raise errors
-}
inlineHeader :: Context -> Header -> Either String (Tuple VexItem Context )
inlineHeader ctx h =
  case h of
    Key mks ->
      Left "inline key signature changes not supported"

    Meter meter ->
      Left "inline time signature changes not supported"

    UnitNoteLength dur ->
      Right (Tuple VIgnore  ctx { unitNoteLength = dur } )

    _ ->
      Right (Tuple VIgnore ctx )

{- get the initial translation context from the tune headers -}
initialContext :: AbcTune -> Context
initialContext t =
  let
    keySig =
      fromMaybe { keySignature : { pitchClass : C, accidental : Natural, mode : Major }, modifications : Nil }
        $ getKeySig t

    meter =
      getMeter t

    unl =
      fromMaybe (rational 1 8) $ getUnitNoteLength t
  in
    { modifiedKeySig : keySig
    , meter : meter
    , unitNoteLength : unl
    , tied : false
    , decoration : Nothing
    , continuation : false
    }

{- get the duration of the first note in a sequence -}
firstNoteDuration :: List AbcNote -> NoteDuration
firstNoteDuration =
   fromMaybe (rational 1 1) <<< head <<< map (_.duration)

-- Helper Functions
{- This is a generic function that operates where we start with a list in ABC and need to end up with the
   equivalent list in VexTab Score.  It performs a left fold over the list using the next function in the tree
   that we need to use in the fold.  It threads the context through the fold.  Because it's a left fold
   then we need to reverse the list in the result when we finish

-}
foldOverResult :: forall a b. Context -> List a -> (Context -> a -> Either String (Tuple b Context )) -> Either String (Tuple (List b) Context )
foldOverResult ctx aline fmus =
  let
    -- append via the pair through the result (we really need a monad here.....)
    apnd :: forall c. Either String (Tuple (List c) Context ) -> Either String (Tuple c Context ) ->  Either String (Tuple (List c) Context )
    apnd rvics rvic  =
      case (Tuple rvic rvics ) of
        ( Tuple (Right vic) (Right vics) ) ->
          let
            newvis =
              (fst vic) : (fst vics)
          in
            Right (Tuple newvis (snd vic) )

        ( Tuple _ (Left acc) ) ->
          Left acc

        ( Tuple (Left next) _ ) ->
          Left next

    -- thread the context through the fold
    -- f :: forall b . Either String (Tuple b Context ) -> Context
    f acc mus =
      let
        applicableCtx =
          case acc of
            Right ( Tuple _ accCtx ) ->
              accCtx

            _ ->
              ctx
      in
        -- fmus is the next function in the tree to apply in the fold
        apnd acc (fmus applicableCtx mus)
  in
    let
      result =
        foldl f (Right (Tuple Nil ctx )) aline
    in
      -- we have done a left fold so we need to reverse the result
      case result of
        Right (Tuple vis ctx' ) ->
          Right (Tuple (reverse vis) ctx' )

        _ ->
          result


normaliseMode :: KeySignature -> KeySignature
normaliseMode ks =
  case ks.mode of
    Ionian ->
      ks

    Major ->
      ks

    Minor ->
      ks

    _ ->
      normaliseModalKey ks

{- check if a line of music is effectively empty -}
emptyLine :: MusicLine -> Boolean
emptyLine mLine =
  let
    f music' =
      case music' of
        Spacer _ ->
          true

        Ignore ->
          true

        Continuation ->
          true

        _ ->
          false
  in
    all f mLine
