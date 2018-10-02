module VexTab.Abc.VexScore  where

import Prelude (class Show)
import Data.List (List)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Abc
        ( KeySignature
        , MeterSignature
        , PitchClass
        , Accidental
        , BarType
        )

-- | a VexTab representation of a score.
type Score =
    List VexBodyPart

-- | a body part within a score.
data VexBodyPart
  = VLine VexLine
  | VContextChange
  | VEmptyLine

-- | a list of VexTab bars (prefaced by an optional stave).
type VexLine =
  { stave :: Maybe VexStave
  , bars :: List VexBar
  }

-- | a bar within a line
type VexBar =
  { barType :: BarType
  , items :: List VexItem
  }

-- | an item that can occur within a stave.
data VexItem
  = VNote VexNote
  | VRest VexRest
  | VTuplet Int (List VexRestOrNote)
  | VChord VexDuration (List VexNote)
  | VNotePair VexNote VexNote
  | VIgnore

-- | a Stave.
type VexStave =
  { clef :: Clef
  , mKey :: Maybe KeySignature
  , mMeter :: Maybe MeterSignature
  }

-- A Clef.
data Clef
  = Treble
  | Bass

instance showClef :: Show Clef where
   show Treble = "treble"
   show Bass = "bass"

-- | A note duration.
data VexDuration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | ThirtySecond
  | SixtyFourth
  | HalfDotted
  | QuarterDotted
  | EighthDotted
  | SixteenthDotted
  | ThirtySecondDotted
  | SixtyFourthDotted

instance showVexDuration :: Show VexDuration where
  show Whole = ":w"
  show Half = ":h"
  show Quarter = ":q"
  show Eighth = ":8"
  show Sixteenth = ":16"
  show ThirtySecond = ":32"
  show SixtyFourth = ":64"
  show HalfDotted = ":hd"
  show QuarterDotted = ":qd"
  show EighthDotted = ":8d"
  show SixteenthDotted = ":16d"
  show ThirtySecondDotted = ":32d"
  show SixtyFourthDotted = ":64d"

-- | a note.
type VexNote =
  { pitchClass :: PitchClass
  , accidental :: Accidental
  , octave :: Int
  , duration :: VexDuration
  , tied :: Boolean               -- to the next note
  , decoration ::  Maybe String   -- is the note decorated (staccato etc)
  }

-- | a rest.
type VexRest =
  { duration :: VexDuration }

-- | a rest or a note
type VexRestOrNote =
  Either VexRest VexNote
