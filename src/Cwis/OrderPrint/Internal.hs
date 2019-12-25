{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint.Internal
    ( scrapeResponse
    , scrapeResponse'
    , ScrapeFailedException (..)
    ) where

import           Control.Arrow          (right, (|||))
import           Control.Exception.Safe
import           Data.Maybe
import qualified Data.Text              as T
import           Text.HTML.Scalpel.Core
import           Text.StringLike


-- |Scrapes html. The return value is something like the following.
scrapeResponse :: MonadThrow m => T.Text -> m T.Text
scrapeResponse = (throw ||| return) . scrapeResponse'

-- |A checked variety of 'scrapeResponse'.
scrapeResponse' :: T.Text -> Either ScrapeFailedException T.Text
scrapeResponse' = right pretty . scrapeRoughly

-- |Scrape the response roughly.
scrapeRoughly :: StringLike str => str -> Either ScrapeFailedException str
scrapeRoughly str = maybe (reason str) Right $
    scrapeStringLike str $ text $ tagSelector "body"
    where
    reason = Left . ScrapeFailedException . toString

-- |An exception when it failed to parse html.
newtype ScrapeFailedException = ScrapeFailedException String
    deriving (Show, Eq)

instance Exception ScrapeFailedException where
    displayException (ScrapeFailedException reason) =
        "Failed to parse an HTML: " ++ reason

-- |Does pretty print outputs from 'scrapeRoughly'.
pretty :: T.Text -> T.Text
pretty = fromMaybe "" . listToMaybe . filter (not . T.null) . fmap T.strip . T.lines
