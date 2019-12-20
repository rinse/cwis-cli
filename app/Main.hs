module Main where

import           Control.Exception.Safe
import           Control.Lens
import           Cwis.Args
import           Cwis.OrderPrint
import qualified Cwis.PrintDetail       as P
import qualified Data.Text              as T
import           Options.Applicative
import           System.IO


toDetail :: Args -> P.PrintDetail
toDetail args = sp & P.cpn .~ (args ^. nCopies)
    where
    sp = P.securityPrint (args ^. username) (args ^. password) (args ^. filepath)

coutText :: Exception e => Either e T.Text -> IO ()
coutText (Right a) = putStrLn $ T.unpack a
coutText (Left e)  = hPutStrLn stderr $ displayException e

main :: IO ()
main = do
    args <- execParser argsParserInfo
    let detail = toDetail args
        proxy = return :: Either SomeException a -> IO (Either SomeException a)
    pure detail
        >>= try . uploadFile (T.pack $ args ^. hostname)
        >>= proxy
        >>= coutText
