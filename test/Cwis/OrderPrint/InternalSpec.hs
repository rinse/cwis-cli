{-# LANGUAGE OverloadedStrings #-}

module Cwis.OrderPrint.InternalSpec (spec) where

import           Cwis.OrderPrint.Internal
import qualified Data.Text                as T
import           Test.Hspec


spec :: Spec
spec = do
    describe "scrapeResponse" $
        it "scrapes Response and get a message from a printer." $ do
            actual <- scrapeResponse inputScrapeResponse
            actual `shouldBe` "リクエストは実行されました。"

    describe "scrapeResponse'" $
        it "scrapes Response and get a message from a printer." $
            scrapeResponse' inputScrapeResponse `shouldBe` Right "リクエストは実行されました。"

inputScrapeResponse :: T.Text
inputScrapeResponse = T.unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\">"
    , "<HTML>"
    , "<HEAD>"
    , "<TITLE>REQUEST: ACCEPTED</TITLE>"
    , "<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
    , "<SCRIPT language=\"JavaScript1.2\"><!--"
    , "function rmParams() {"
    , "if (parent != self) {"
    , "top.saved_params = null;"
    , "parent.reset_cgi();"
    , "}"
    , "}"
    , "//--></SCRIPT>"
    , "</HEAD>"
    , "<BODY bgcolor=\"#D1D8E8\" link=\"blue\" vlink=\"blue\" alink=\"blue\">"
    , "<P align=\"center\">"
    , "リクエストは実行されました。"
    , "<FORM>"
    , "<SCRIPT language=\"JavaScript1.2\"><!--"
    , "rmParams();"
    , " if (parent != self) parent.TF.window.location.href = parent.hdrname;"
    , "//--></SCRIPT>"
    , "</FORM>"
    , "</BODY>"
    , "</HTML>"
    ]
