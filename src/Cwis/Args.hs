{-# LANGUAGE TemplateHaskell #-}

module Cwis.Args
    ( Args
    , hostname
    , nCopies
    , username
    , password
    , filepath
    , argsParserInfo
    ) where

import           Control.Lens
import           Options.Applicative


-- $setup
-- >>> let args = Args "hostname" 1 "" "" ""

{- |Represents arguments that the application can take.
    This is the interface which faces to the user.

    Use 'Lens' operators to get the contents.

    >>> args ^. hostname 
    "hostname"
-}
data Args = Args
    { _hostname :: String   -- ^Hostname of a printer
    , _nCopies  :: Int      -- ^The number of copies
    , _username :: String   -- ^Username
    , _password :: String   -- ^Password
    , _filepath :: FilePath -- ^File to print
    }
makeLenses ''Args

-- |A parser info for 'Args'.
argsParserInfo :: ParserInfo Args
argsParserInfo =
    withHelper argsParser `withInfo` "This is a cli interface for a printer."
    where
    withHelper = (helper <*>)
    withInfo p = info p . progDesc

argsParser :: Parser Args
argsParser = Args
    <$> strOption hostnameMod
    <*> option auto copiesMod
    <*> strOption usernameMod
    <*> strOption passwordMod
    <*> strArgument fileMod
    where
    hostnameMod = mconcat
        [ long "hostname"
        , help "the hostname of the printer"
        ]
    copiesMod = mconcat
        [ long "copies"
        , short 'n'
        , value 1
        , showDefault
        , help "the number of copies"
        ]
    usernameMod = mconcat
        [ long "username"
        , short 'u'
        , help "the username for security print"
        ]
    passwordMod = mconcat
        [ long "password"
        , short 'p'
        , help "the password for security print"
        ]
    fileMod = mconcat
        [ metavar "FILE"
        , action "file"
        , help "the file to print"
        ]
