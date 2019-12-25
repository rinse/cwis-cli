{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint.Internal
    ( scrapeHTML
    , scrapeHTML'
    , ScrapeFailedException (..)
    ) where

import           Control.Arrow          (right, (|||))
import           Control.Exception.Safe
import           Data.Maybe
import qualified Data.Text              as T
import           Text.HTML.Scalpel.Core
import           Text.StringLike


-- |Scrapes html. The return value is something like the following.
scrapeHTML :: MonadThrow m => T.Text -> m T.Text
scrapeHTML = (throw ||| return) . scrapeHTML'

-- |A checked variety of 'scrapeHTML'.
scrapeHTML' :: T.Text -> Either ScrapeFailedException T.Text
scrapeHTML' = right pretty . scrapeRoughly

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
