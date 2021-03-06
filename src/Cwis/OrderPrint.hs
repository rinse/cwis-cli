{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint
    ( uploadFile
    ) where

import           Control.Arrow            ((|||))
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Cwis.OrderPrint.Internal
import           Cwis.PrintDetail
import           Cwis.PrintMethod
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
           -> PrintMethod   -- ^Detail of the request
           -> m T.Text      -- ^Message from the server
uploadFile hostname method =
    runRequest hostname method
    >>= decodeUtf8Response
    >>= scrapeResponse

-- |Runs the req.
runRequest :: MonadIO m => T.Text -> PrintMethod -> m BsResponse
runRequest = (fmap . fmap) (runReq defaultHttpConfig) request

-- |Constructs a req monad.
request :: MonadHttp m => T.Text -> PrintMethod -> m BsResponse
request hostName method = do
    reqBody <- reqBodyMultipart $ methodToParts method
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
