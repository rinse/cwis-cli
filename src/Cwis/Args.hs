module Cwis.Args
    ( Args (..)
    , argsParserInfo
    ) where

import           Control.Lens        ((^.))
import           Cwis.PrintMethod
import           Data.Default        (def)
import           Options.Applicative


data Args = Args String PrintMethod
    deriving (Show, Eq)

argsParserInfo :: ParserInfo Args
argsParserInfo = (helper <*> argsParser) `withDesc` "This is a cli interface for a printer."

argsParser :: Parser Args
argsParser = Args
    <$> strOption hostnameMod
    <*> printMethodParser
    where
    hostnameMod = mconcat
        [ long "hostname"
        , help "The hostname of the printer"
        ]

printMethodParser :: Parser PrintMethod
printMethodParser = subparser securityPrintMod
    where
    securityPrintMod = command "security-print" $
        securityPrintParser `withDesc` "Commits security print which keeps your documents safe."

withDesc :: Parser a -> String -> ParserInfo a
withDesc p = info p . progDesc

securityPrintParser :: Parser PrintMethod
securityPrintParser = SecurityPrint
    <$> strOption usernameMod
    <*> strOption passwordMod
    <*> strArgument filepathMod
    <*> commonOptionsParser
    where
    usernameMod = mconcat
        [ long "username"
        , short 'u'
        , help "The username required when to print."
        ]
    passwordMod = mconcat
        [ long "password"
        , short 'p'
        , help "The password required when to print."
        ]
    filepathMod = mconcat
        [ metavar "FILE"
        , action "file"
        , help "The file to print."
        ]

commonOptionsParser :: Parser CommonOptions
commonOptionsParser = CommonOptions
    <$> option auto numCopiesMod
    <*> optional (flag' True doSortMod)
    <*> optional (option auto duplexMod)
    <*> option auto colourModeMod
    <*> optional (option auto stapleMod)
    <*> optional (option auto punchMod)
    <*> option auto outputTrayMod
    <*> option auto inputTrayMod
    <*> optional (option auto paperSizeMod)
    <*> optional (option auto paperTypeMod)
    where
    numCopiesMod = mconcat
        [ long "num-copies"
        , short 'n'
        , value $ def ^. numCopies
        , help "Specifies the number of copies."
        ]
    doSortMod = mconcat
        [ long "sort"
        , help "Specifies if documents are sorted."
        ]
    duplexMod = mconcat
        [ long "duplex"
        , help "Specifies if documents are printed on the both side."
        ]
    colourModeMod = mconcat
        [ long "colour-mode"
        , long "color-mode"
        , value $ def ^. colourMode
        , help "Specifies the colour mode."
        ]
    stapleMod = mconcat
        [ long "staple"
        , help "Specifies the position of staples."
        ]
    punchMod = mconcat
        [ long "punch"
        , help "Specifies the position of punches."
        ]
    outputTrayMod = mconcat
        [ long "output-tray"
        , value $ def ^. outputTray
        , help "Specifies which output tray to deliver."
        ]
    inputTrayMod = mconcat
        [ long "input-tray"
        , value $ def ^. inputTray
        , help "Specifies which input tray to get papers."
        ]
    paperSizeMod = mconcat
        [ long "paper-size"
        , help "Specifies which size of papers."
        ]
    paperTypeMod = mconcat
        [ long "paper-size"
        , help "Specifies which type of papers."
        ]
