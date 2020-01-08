{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cwis.PrintMethod
    ( PrintMethod (..)
    , CommonOptions (..)
    , Duplex (..)
    , Colour (..)
    , Staple (..)
    , Punch (..)
    , OutputTray (..)
    , InputTray (..)
    , PaperSize (..)
    , PaperType (..)
    , commonOptions
    , numCopies
    , doSort
    , duplex
    , colourMode
    , staple
    , punch
    , outputTray
    , inputTray
    , paperSize
    , paperType
    , (.=?)
    ) where

import           Control.Lens              (ASetter, Lens', makeLenses, (.=))
import           Control.Monad.State.Class (MonadState)
import           Data.Default              (Default, def)
import           Data.Foldable             (traverse_)


-- $setup
-- >>> import Control.Lens

{- |A printing mode.

* 'SecurityPrint'

    * A method which requires you to input your username and password into the printer to print the document.
      It helps to keep your document safe.
-}
data PrintMethod
    = SecurityPrint
        String          -- ^Username for security print.
        String          -- ^Password for security print.
        FilePath        -- ^Filepath of the file.
        CommonOptions   -- ^Options.
    deriving (Show, Eq)

{- |A Lens for 'CommonOptions' of 'PrintMethod'.

    >>> let m = SecurityPrint "name" "pass" "file" $ def &~ numCopies .= 100
    >>> m ^. commonOptions . numCopies
    100
-}
commonOptions :: Lens' PrintMethod CommonOptions
commonOptions f (SecurityPrint username password file options) =
    SecurityPrint username password file <$> f options

{- |'CommonOptions' is a collection of optional values which are common to 'PrintMethod'.

    Constructs it with 'Lens'. For instance:

    >>> (def &~ numCopies .= 2) ^. numCopies
    2

    >>> (def &~ doSort ?= True) ^. doSort
    Just True

    >>> (def &~ colourMode .=? Just MultiColoured) ^. colourMode
    MultiColoured
-}
data CommonOptions = CommonOptions
    { _numCopies  :: Int              -- ^The number of copies.
    , _doSort     :: Maybe Bool       -- ^Give a sort or not.
    , _duplex     :: Maybe Duplex     -- ^Duplex mode.
    , _colourMode :: Colour           -- ^Colour mode.
    , _staple     :: Maybe Staple     -- ^The position of staple(s).
    , _punch      :: Maybe Punch      -- ^The position of punches.
    , _outputTray :: OutputTray       -- ^The output tray.
    , _inputTray  :: InputTray        -- ^The input tray.
    , _paperSize  :: Maybe PaperSize  -- ^The paper size.
    , _paperType  :: Maybe PaperType  -- ^The paper type.
    } deriving (Show, Eq)

instance Default CommonOptions where
    def = CommonOptions
            1 Nothing Nothing ColourAuto
            Nothing Nothing
            OutputTray InputTrayAuto
            Nothing Nothing

-- |Represents a duplex-printing mode.
data Duplex = LongEdge | ShortEdge
    deriving (Read, Show, Enum, Eq)

-- |Represents a colour mode.
data Colour = ColourAuto | MultiColoured | MonoColoured
    deriving (Read, Show, Enum, Eq)

-- |Represents a staple mode.
data Staple
    -- |Staple in the top left corner.
    = StapleTopLeft
    -- |Staple in the bottom left corner.
    | StapleBottomLeft
    -- |Staple in the top right corner.
    | StapleTopRight
    -- |Staple in the bottom right corner.
    | StapleBottomRight
    -- |2 Staples in top.
    | StapleTop2
    -- |2 Staples in bottom.
    | StapleBottom2
    -- |2 Staples in the left side.
    | StapleLeft2
    -- |2 Staples in the right side.
    | StapleRight2
    deriving (Read, Show, Enum, Eq)

-- |Represents a punch mode.
data Punch
    = PunchTop2 | PunchBottom2 | PunchLeft2 | PunchRight2
    | PunchTop4 | PunchBottom4 | PunchLeft4 | PunchRight4
    deriving (Read, Show, Enum, Eq)

-- |Represents an output tray.
data OutputTray = OutputTray | FinisherTray
    deriving (Read, Show, Enum, Eq)

{- |Represents an input tray.

    If you want to have @Tray5@, 'ManualFeed' may be the one.
-}
data InputTray = InputTrayAuto | Tray1 | Tray2 | Tray3 | Tray4 | ManualFeed
    deriving (Read, Show, Enum, Eq)

-- |Represents a size of papers.
data PaperSize
    = A3 | B4 | A4 | B5 | A5 | SizeLetter | FoolscapFolio | SizeLegal | I15 | SizeLedger
    deriving (Read, Show, Enum, Eq)

-- |Represents a type of papers.
data PaperType
    = NormalPaper
    | RecycledPaper
    | UserDefinedPaper1
    | UserDefinedPaper2
    | UserDefinedPaper3
    | UserDefinedPaper4
    | UserDefinedPaper5
    deriving (Read, Show, Enum, Eq)

makeLenses ''CommonOptions

{- |A variety of ('.=') which takes a value in @'Foldable' t@.
    Typically, this is used with Maybe values.
-}
(.=?) :: (Foldable t, MonadState s m) => ASetter s s a b -> t b -> m ()
l .=? t = traverse_ (l .=) t
