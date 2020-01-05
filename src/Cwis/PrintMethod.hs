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
    , doSort
    , onBothSides
    , colourMode
    , withStaple
    , withPunch
    , outputTray
    , inputTray
    , paperSize
    , paperType
    ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Default


{- |A printing mode.

    'SecurityPrint': a method which requires you to input
    your username and password into the printer to print the document.
    It helps to keep your document safe.
-}
data PrintMethod = SecurityPrint String String FilePath CommonOptions
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

-- |Makes a 'PrintMethod' with default common options.
defaultSecurityPrint
    :: String       -- ^Username for security-print.
    -> String       -- ^Password for security-print.
    -> FilePath     -- ^Filepath of the file.
    -> PrintMethod  -- ^A 'PrintMethod'.
defaultSecurityPrint username password file =
    SecurityPrint username password file def

-- |A builder for 'PrintMethod'.
newtype PrintMethodBuilderT m a = PrintMethodBuilderT (StateT PrintMethod m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

-- |Runs the builder and constructs 'PrintMethod'.
runPrintMethodBuilderT :: PrintMethod
                       -> PrintMethodBuilderT m a
                       -> m (a, PrintMethod)
runPrintMethodBuilderT p (PrintMethodBuilderT s) = runStateT s p

-- |A builder for 'PrintMethod'.
type PrintMethodBuilder = PrintMethodBuilderT Identity

-- |Runs the builder and constructs 'PrintMethod'.
runPrintMethodBuilder :: PrintMethod
                       -> PrintMethodBuilder a
                       -> (a, PrintMethod)
runPrintMethodBuilder = fmap runIdentity . runPrintMethodBuilderT

-- |An action which specifies the number of copies.
numCopies :: Monad m => Int -> PrintMethodBuilderT m ()
numCopies n = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . cpn .~ n

-- |An action which specifies if you want to sort your papers.
doSort :: Monad m => Bool -> PrintMethodBuilderT m ()
doSort b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . colt .~ b

-- |An action which specifies if you want to print on the both sides.
onBothSides :: Monad m => Bool -> PrintMethodBuilderT m ()
onBothSides b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . dup .~ b

-- |An action which specifies a colour mode.
colourMode :: Monad m => Colour -> PrintMethodBuilderT m ()
colourMode c = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . clr .~ c

-- |An action which specifies if you want to staple your documents.
withStaple :: Monad m => Bool -> PrintMethodBuilderT m ()
withStaple b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . stpl .~ b

-- |An action which specifies if you want to punch your documents.
withPunch :: Monad m => Bool -> PrintMethodBuilderT m ()
withPunch b = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . pnch .~ b

-- |An action which specifies an output tray.
outputTray :: Monad m => OutputTray -> PrintMethodBuilderT m ()
outputTray o = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . ot .~ o

-- |An action which specifies an input tray.
inputTray :: Monad m => InputTray -> PrintMethodBuilderT m ()
inputTray i = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . it .~ i

-- |An action which specifies a size of papers.
paperSize :: Monad m => PaperSize -> PrintMethodBuilderT m ()
paperSize s = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . siz .~ s

-- |An action which specifies a type of papers.
paperType :: Monad m => PaperType -> PrintMethodBuilderT m ()
paperType t = PrintMethodBuilderT $ modify $ \p ->
    p & commonOptions . med .~ t
