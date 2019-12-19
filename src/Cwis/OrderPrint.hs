{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint
    ( uploadFile
    , ScrapeFailedException (..)
    ) where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import qualified Cwis.PrintDetail         as P
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import           Network.HTTP.Req
import           Text.HTML.Scalpel.Core
import           Text.StringLike


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
    >>= fmap pretty . scrapeHTML

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
decodeUtf8Response = stomp . decodeUtf8Response'

{- |Decodes a bytestring response to utf8
    For a unchecked variation, see 'decodeUtf8Response'
-}
decodeUtf8Response' :: BsResponse -> Either T.UnicodeException T.Text
decodeUtf8Response' = T.decodeUtf8' . responseBody

-- |An exception when it failed to parse html.
newtype ScrapeFailedException = ScrapeFailedException String deriving (Show)

instance Exception ScrapeFailedException where
    displayException (ScrapeFailedException reason) =
        "Failed to parse an HTML: " ++ reason

{- |Scrapes html. The return value is something like the following.

リクエストは実行されました。

<!--
rmParams();
 if (parent != self) parent.TF.window.location.href = parent.hdrname;
//-->

-}
scrapeHTML :: (MonadThrow m, StringLike str) => str -> m str
scrapeHTML = stomp . scrapeHTML'

-- |A checked variation of 'scrapeHTML'.
scrapeHTML' :: StringLike str => str -> Either ScrapeFailedException str
scrapeHTML' str = maybe (reason str) Right $
    scrapeStringLike str $
        text $ tagSelector "body"
    where
    reason = Left . ScrapeFailedException . toString

-- |It stomps an either with exception on its left.
stomp :: (MonadThrow m, Exception e) => Either e a -> m a
stomp (Left e)  = throw e
stomp (Right a) = return a


{- |Expects an input like the following

リクエストは実行されました。

<!--
rmParams();
 if (parent != self) parent.TF.window.location.href = parent.hdrname;
//-->

-}
pretty :: T.Text -> T.Text
pretty = fromMaybe "" . listToMaybe . filter (not . T.null) . fmap T.strip . T.lines
