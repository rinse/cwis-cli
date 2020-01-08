module Main where

import           Control.Exception.Safe
import           Control.Lens
import           Cwis.Args
import           Cwis.OrderPrint
import           Cwis.PrintMethod
import           Data.Default           (def)
import qualified Data.Text              as T
import           Options.Applicative
import           System.IO


toMethod :: Args -> PrintMethod
toMethod args = SecurityPrint (args ^. username) (args ^. password) (args ^. filepath) $
    def &~ numCopies .= args ^. nCopies

coutText :: Exception e => Either e T.Text -> IO ()
coutText (Right a) = putStrLn $ T.unpack a
coutText (Left e)  = hPutStrLn stderr $ displayException e

main :: IO ()
main = do
    args <- execParser argsParserInfo
    let detail = toMethod args
        proxy = return :: Either SomeException a -> IO (Either SomeException a)
    pure detail
        >>= try . uploadFile (T.pack $ args ^. hostname)
        >>= proxy
        >>= coutText
