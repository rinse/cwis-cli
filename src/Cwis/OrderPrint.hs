{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint
    ( uploadFile
    ) where

import           Control.Arrow            ((|||))
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Cwis.OrderPrint.Internal
import qualified Cwis.PrintDetail         as P
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import           Network.HTTP.Req


{- |Uploads a file to a printer.
    The printer is identified by the given host name.
    It may throw:

    * 'T.UnicodeException' when the response is not utf-8.
    * 'ScrapeFailedException' when it failed to parse the response.
    * 'HttpException' for http-related problems.
    * 'SomeException' for some other reasons.

    The return value is a message from the printer.
-}
uploadFile :: (MonadIO m, MonadThrow m)
           => T.Text        -- ^Host name
           -> P.PrintDetail -- ^Detail of the request
           -> m T.Text      -- ^Message from the server
uploadFile hostname detail =
    runRequest hostname detail
    >>= decodeUtf8Response
    >>= scrapeHTML

-- |Runs the req.
runRequest :: MonadIO m => T.Text -> P.PrintDetail -> m BsResponse
runRequest = (fmap . fmap) (runReq defaultHttpConfig) request

-- |Constructs a req monad.
request :: MonadHttp m => T.Text -> P.PrintDetail -> m BsResponse
request hostName detail = do
    reqBody <- reqBodyMultipart $ P.toParts detail
    req POST url reqBody bsResponse mempty
    where
    url = http hostName /: "UPLPRT.cmd"

{- |Decodes a bytestring response to utf8.
    For a checked variation, see 'decodeUtf8Response''.
-}
decodeUtf8Response :: MonadThrow m => BsResponse -> m T.Text
decodeUtf8Response = (throw ||| return) . decodeUtf8Response'

{- |Decodes a bytestring response to utf8
    For a unchecked variation, see 'decodeUtf8Response'
-}
decodeUtf8Response' :: BsResponse -> Either T.UnicodeException T.Text
decodeUtf8Response' = T.decodeUtf8' . responseBody
