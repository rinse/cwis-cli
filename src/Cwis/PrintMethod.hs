{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cwis.PrintMethod
    ( PrintMethod (..)
    , CommonOptions (CommonOptions)
    , Duplex (..)
    , Colour (..)
    , Staple (..)
    , Punch (..)
    , OutputTray (..)
    , InputTray (..)
    , PaperSize (..)
    , PaperType (..)
    , defaultSecurityPrint
    , PrintMethodBuilderT
    , runPrintMethodBuilderT
    , evalPrintMethodBuilderT
    , execPrintMethodBuilderT
    , PrintMethodBuilder
    , runPrintMethodBuilder
    , evalPrintMethodBuilder
    , execPrintMethodBuilder
    , numCopies
    , numCopies'
    , doSort
    , doSort'
    , onBothSides
    , onBothSides'
    , colourMode
    , colourMode'
    , withStaple
    , withStaple'
    , withPunch
    , withPunch'
    , outputTray
    , outputTray'
    , inputTray
    , inputTray'
    , paperSize
    , paperSize'
    , paperType
    , paperType'
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Default           (Default, def)
import           Data.Foldable          (traverse_)


{- |A printing mode.

    'SecurityPrint': a method which requires you to input
    your username and password into the printer to print the document.
    It helps to keep your document safe.
-}
data PrintMethod
    = SecurityPrint
        String          -- ^Username for security-print.
        String          -- ^Password for security-print.
        FilePath        -- ^Filepath of the file.
        CommonOptions   -- ^Options.
    deriving (Show, Eq)

-- |A Lens for 'CommonOptions' of 'PrintMethod'.
commonOptions :: Lens' PrintMethod CommonOptions
commonOptions f (SecurityPrint username password file options) =
    SecurityPrint username password file <$> f options

-- |Common options to every 'PrintMethod'.
data CommonOptions = CommonOptions
    { _cpn  :: Int              -- ^num of copies
    , _colt :: Maybe Bool       -- ^sort
    , _dup  :: Maybe Duplex     -- ^print on both sides
    , _clr  :: Colour           -- ^colour mode
    , _stpl :: Maybe Staple     -- ^staple
    , _pnch :: Maybe Punch      -- ^punch
    , _ot   :: OutputTray       -- ^output tray
    , _it   :: InputTray        -- ^input tray
    , _siz  :: Maybe PaperSize  -- ^paper size
    , _med  :: PaperType        -- ^paper type
    } deriving (Show, Eq)

instance Default CommonOptions where
    def = CommonOptions
            1 Nothing Nothing ColourAuto
            Nothing Nothing
            OutputTray InputTrayAuto
            Nothing TypeAuto

-- |Represents a duplex-printing mode.
data Duplex = LongEdge | ShortEdge
    deriving (Show, Enum, Eq)

-- |Represents a colour mode.
data Colour = ColourAuto | MultiColoured | MonoColoured
    deriving (Show, Enum, Eq)

-- |Represents a staple mode.
data Staple
    = StapleUpperLeft
    | StapleLowerLeft
    | StapleUpperRight
    | StapleLowerRight
    | StapleTop2
    | StapleBottom2
    | StapleLeft2
    | StapleRight2
    deriving (Show, Enum, Eq)

-- |Represents a punch mode.
data Punch
    = PunchTop2 | PunchBottom2 | PunchLeft2 | PunchRight2
    | PunchTop4 | PunchBottom4 | PunchLeft4 | PunchRight4
    deriving (Show, Enum, Eq)

-- |Represents an output tray.
data OutputTray = OutputTray | FinisherTray
    deriving (Show, Enum, Eq)

{- |Represents an input tray.

    If you want to have @Tray5@, 'ManualFeed' may be the one.
-}
data InputTray = InputTrayAuto | Tray1 | Tray2 | Tray3 | Tray4 | ManualFeed
    deriving (Show, Enum, Eq)

-- |Represents a size of papers.
data PaperSize
    = A3 | B4 | A4 | B5 | A5 | SizeLetter | FoolscapFolio | SizeLegal | I15 | SizeLedger
    deriving (Show, Enum, Eq)

-- |Represents a type of papers.
data PaperType = TypeAuto
    deriving (Show, Enum, Eq)

makeLenses ''CommonOptions

{- |Makes a 'PrintMethod' with default common options.
    Tipically, this is used with 'PrintMethodBuilder'.

    @runPrintMethodBuilder (defaultSecurityPrint u p f) $ do
    ..
    @
-}
defaultSecurityPrint
    :: String       -- ^Username for security-print.
    -> String       -- ^Password for security-print.
    -> FilePath     -- ^Filepath of the file.
    -> PrintMethod  -- ^A 'PrintMethod'.
defaultSecurityPrint username password file =
    SecurityPrint username password file def

{- |A builder for 'PrintMethod'.
    It allows you to construct 'PrintMethod' monadically.

    @'runPrintMethodBuilder' ('defaultSecurityPrint' u p f) $ do
    n <- liftIO $ lookupEnv "numCopies"
    'numCopies'' $ somwNumCopiesMaybe '<|>' (n >>= readMaybe)
    'doSort' True
    'onBothSides' True
    ..
    @
-}
newtype PrintMethodBuilderT m a = PrintMethodBuilderT (StateT PrintMethod m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

-- |Runs the builder and constructs 'PrintMethod'.
runPrintMethodBuilderT :: PrintMethod
                       -> PrintMethodBuilderT m a
                       -> m (a, PrintMethod)
runPrintMethodBuilderT p (PrintMethodBuilderT s) = runStateT s p

execPrintMethodBuilderT :: Monad m => PrintMethod -> PrintMethodBuilderT m a -> m PrintMethod
execPrintMethodBuilderT p (PrintMethodBuilderT s) = execStateT s p

evalPrintMethodBuilderT :: Monad m => PrintMethod -> PrintMethodBuilderT m a -> m a
evalPrintMethodBuilderT p (PrintMethodBuilderT s) = evalStateT s p

-- |A pure variant of 'PrintMethodBuilderT'.
type PrintMethodBuilder = PrintMethodBuilderT Identity

-- |A pure runner for 'PrintMethodBuilder'.
runPrintMethodBuilder :: PrintMethod
                       -> PrintMethodBuilder a
                       -> (a, PrintMethod)
runPrintMethodBuilder = fmap runIdentity . runPrintMethodBuilderT

execPrintMethodBuilder :: PrintMethod -> PrintMethodBuilder a -> PrintMethod
execPrintMethodBuilder p (PrintMethodBuilderT s) = execState s p

evalPrintMethodBuilder :: PrintMethod -> PrintMethodBuilder a -> a
evalPrintMethodBuilder p (PrintMethodBuilderT s) = evalState s p

instance MonadIO m => MonadIO (PrintMethodBuilderT m) where
    liftIO action = PrintMethodBuilderT $ liftIO action

-- |An action which specifies the number of copies.
numCopies :: Monad m => Int -> PrintMethodBuilderT m ()
numCopies = setParam cpn

-- |Alternative variant of 'numCopies'.
numCopies' :: (Monad m, Foldable t) => t Int -> PrintMethodBuilderT m ()
numCopies' = traverse_ numCopies

-- |An action which specifies if you want to sort your papers.
doSort :: Monad m => Maybe Bool -> PrintMethodBuilderT m ()
doSort = setParam colt

-- |Alternative variant of 'doSort'.
doSort' :: (Monad m, Foldable t) => t (Maybe Bool) -> PrintMethodBuilderT m ()
doSort' = traverse_ doSort

-- |An action which specifies if you want to print on the both sides.
onBothSides :: Monad m => Maybe Duplex -> PrintMethodBuilderT m ()
onBothSides = setParam dup

-- |Alternative variant of 'onBothSides'.
onBothSides' :: (Monad m, Foldable t) => t (Maybe Duplex) -> PrintMethodBuilderT m ()
onBothSides' = traverse_ onBothSides

-- |An action which specifies a colour mode.
colourMode :: Monad m => Colour -> PrintMethodBuilderT m ()
colourMode = setParam clr

-- |Alternative variant of 'colourMode'.
colourMode' :: (Monad m, Foldable t) => t Colour -> PrintMethodBuilderT m ()
colourMode' = traverse_ colourMode

-- |An action which specifies if you want to staple your documents.
withStaple :: Monad m => Maybe Staple -> PrintMethodBuilderT m ()
withStaple = setParam stpl

-- |Alternative variant of 'withStaple'.
withStaple' :: (Monad m, Foldable t) => t (Maybe Staple) -> PrintMethodBuilderT m ()
withStaple' = traverse_ withStaple

-- |An action which specifies if you want to punch your documents.
withPunch :: Monad m => Maybe Punch -> PrintMethodBuilderT m ()
withPunch = setParam pnch

-- |Alternative variant of 'withPunch'.
withPunch' :: (Monad m, Foldable t) => t (Maybe Punch) -> PrintMethodBuilderT m ()
withPunch' = traverse_ withPunch

-- |An action which specifies an output tray.
outputTray :: Monad m => OutputTray -> PrintMethodBuilderT m ()
outputTray = setParam ot

-- |Alternative variant of 'outputTray'.
outputTray' :: (Monad m, Foldable t) => t OutputTray -> PrintMethodBuilderT m ()
outputTray' = traverse_ outputTray

-- |An action which specifies an input tray.
inputTray :: Monad m => InputTray -> PrintMethodBuilderT m ()
inputTray = setParam it

-- |Alternative variant of 'inputTray'.
inputTray' :: (Monad m, Foldable t) => t InputTray -> PrintMethodBuilderT m ()
inputTray' = traverse_ inputTray

-- |An action which specifies a size of papers.
paperSize :: Monad m => Maybe PaperSize -> PrintMethodBuilderT m ()
paperSize = setParam siz

-- |Alternative variant of 'paperSize'.
paperSize' :: (Monad m, Foldable t) => t (Maybe PaperSize) -> PrintMethodBuilderT m ()
paperSize' = traverse_ paperSize

-- |An action which specifies a type of papers.
paperType :: Monad m => PaperType -> PrintMethodBuilderT m ()
paperType = setParam med

-- |Alternative variant of 'paperType'.
paperType' :: (Monad m, Foldable t) => t PaperType -> PrintMethodBuilderT m ()
paperType' = traverse_ paperType

-- |A helper for actions which specifies a parameter on 'PrintMethodBuilderT'.
setParam :: Monad m => Lens' CommonOptions a -> a -> PrintMethodBuilderT m ()
setParam f = PrintMethodBuilderT . modify . set (commonOptions . f)
