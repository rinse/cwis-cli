{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cwis.PrintMethod
    ( PrintMethod (..)
    , CommonOptions (CommonOptions)
    , Colour (..)
    , OutputTray (..)
    , InputTray (..)
    , PaperSize (..)
    , PaperType (..)
    , defaultSecurityPrint
    , PrintMethodBuilderT
    , runPrintMethodBuilderT
    , PrintMethodBuilder
    , runPrintMethodBuilder
    , numCopies
    , numCopiesMaybe
    , doSort
    , doSortMaybe
    , onBothSides
    , onBothSidesMaybe
    , colourMode
    , colourModeMaybe
    , withStaple
    , withStapleMaybe
    , withPunch
    , withPunchMaybe
    , outputTray
    , outputTrayMaybe
    , inputTray
    , inputTrayMaybe
    , paperSize
    , paperSizeMaybe
    , paperType
    , paperTypeMaybe
    ) where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Default              (Default, def)
import           Data.Foldable             (traverse_)


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
    { _cpn  :: Int         -- ^num of copies
    , _colt :: Bool        -- ^sort
    , _dup  :: Bool        -- ^print on both sides
    , _clr  :: Colour      -- ^colour mode
    , _stpl :: Bool        -- ^staple
    , _pnch :: Bool        -- ^punch
    , _ot   :: OutputTray  -- ^output tray
    , _it   :: InputTray   -- ^input tray
    , _siz  :: PaperSize   -- ^paper size
    , _med  :: PaperType   -- ^paper type
    } deriving (Show, Eq)

instance Default CommonOptions where
    def = CommonOptions
            1 False False ColourAuto
            False False
            OutputTray InputTrayAuto
            SizeAuto TypeAuto

-- |Represents a colour mode.
data Colour = ColourAuto
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
data PaperSize = SizeAuto
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
    'numCopiesMaybe' $ (n >>= readMaybe) '<|>' somwNumCopiesMaybe
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

-- |A pure variant of 'PrintMethodBuilderT'.
type PrintMethodBuilder = PrintMethodBuilderT Identity

-- |A pure runner for 'PrintMethodBuilder'.
runPrintMethodBuilder :: PrintMethod
                       -> PrintMethodBuilder a
                       -> (a, PrintMethod)
runPrintMethodBuilder = fmap runIdentity . runPrintMethodBuilderT

-- |An action which specifies the number of copies.
numCopies :: Monad m => Int -> PrintMethodBuilderT m ()
numCopies n = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . cpn .~ n

-- |Alternative variant of 'numCopies'.
numCopiesMaybe :: Monad m => MaybeT m Int -> PrintMethodBuilderT m ()
numCopiesMaybe = lift . runMaybeT >=> traverse_ numCopies

-- |An action which specifies if you want to sort your papers.
doSort :: Monad m => Bool -> PrintMethodBuilderT m ()
doSort b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . colt .~ b

-- |Alternative variant of 'doSort'.
doSortMaybe :: Monad m => MaybeT m Bool -> PrintMethodBuilderT m ()
doSortMaybe = lift . runMaybeT >=> traverse_ doSort

-- |An action which specifies if you want to print on the both sides.
onBothSides :: Monad m => Bool -> PrintMethodBuilderT m ()
onBothSides b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . dup .~ b

-- |Alternative variant of 'onBothSides'.
onBothSidesMaybe :: Monad m => MaybeT m Bool -> PrintMethodBuilderT m ()
onBothSidesMaybe = lift . runMaybeT >=> traverse_ onBothSides

-- |An action which specifies a colour mode.
colourMode :: Monad m => Colour -> PrintMethodBuilderT m ()
colourMode c = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . clr .~ c

-- |Alternative variant of 'colourMode'.
colourModeMaybe :: Monad m => MaybeT m Colour -> PrintMethodBuilderT m ()
colourModeMaybe = lift . runMaybeT >=> traverse_ colourMode

-- |An action which specifies if you want to staple your documents.
withStaple :: Monad m => Bool -> PrintMethodBuilderT m ()
withStaple b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . stpl .~ b

-- |Alternative variant of 'withStaple'.
withStapleMaybe :: Monad m => MaybeT m Bool -> PrintMethodBuilderT m ()
withStapleMaybe = lift . runMaybeT >=> traverse_ withStaple

-- |An action which specifies if you want to punch your documents.
withPunch :: Monad m => Bool -> PrintMethodBuilderT m ()
withPunch b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . pnch .~ b

-- |Alternative variant of 'withPunch'.
withPunchMaybe :: Monad m => MaybeT m Bool -> PrintMethodBuilderT m ()
withPunchMaybe = lift . runMaybeT >=> traverse_ withPunch

-- |An action which specifies an output tray.
outputTray :: Monad m => OutputTray -> PrintMethodBuilderT m ()
outputTray o = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . ot .~ o

-- |Alternative variant of 'outputTray'.
outputTrayMaybe :: Monad m => MaybeT m OutputTray -> PrintMethodBuilderT m ()
outputTrayMaybe = lift . runMaybeT >=> traverse_ outputTray

-- |An action which specifies an input tray.
inputTray :: Monad m => InputTray -> PrintMethodBuilderT m ()
inputTray i = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . it .~ i

-- |Alternative variant of 'inputTray'.
inputTrayMaybe :: Monad m => MaybeT m InputTray -> PrintMethodBuilderT m ()
inputTrayMaybe = lift . runMaybeT >=> traverse_ inputTray

-- |An action which specifies a size of papers.
paperSize :: Monad m => PaperSize -> PrintMethodBuilderT m ()
paperSize s = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . siz .~ s

-- |Alternative variant of 'paperSize'.
paperSizeMaybe :: Monad m => MaybeT m PaperSize -> PrintMethodBuilderT m ()
paperSizeMaybe = lift . runMaybeT >=> traverse_ paperSize

-- |An action which specifies a type of papers.
paperType :: Monad m => PaperType -> PrintMethodBuilderT m ()
paperType t = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . med .~ t

-- |Alternative variant of 'paperType'.
paperTypeMaybe :: Monad m => MaybeT m PaperType -> PrintMethodBuilderT m ()
paperTypeMaybe = lift . runMaybeT >=> traverse_ paperType
