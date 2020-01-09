module Main where

import           Control.Exception.Safe
import           Cwis.Args
import           Cwis.OrderPrint
import qualified Data.Text              as T
import           Options.Applicative
import           System.IO


main :: IO ()
main = do
    (Args hostname printMethod) <- execParser argsParserInfo
    let proxy = return :: Either SomeException a -> IO (Either SomeException a)
    pure printMethod
        >>= try . uploadFile (T.pack hostname)
        >>= proxy
        >>= coutText

coutText :: Exception e => Either e T.Text -> IO ()
coutText (Right a) = putStrLn $ T.unpack a
coutText (Left e)  = hPutStrLn stderr $ displayException e
