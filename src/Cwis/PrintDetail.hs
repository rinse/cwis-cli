{- |__WIP__.

    It is not a good idea to export the contructor of PrintDetail.
    Also, it seems that 'toParts' should not be here.
-}
{-# LANGUAGE OverloadedStrings #-}

module Cwis.PrintDetail
    ( PrintDetail (..)
    , securityPrint
    , toParts
    ) where

import           Control.Monad
import qualified Data.ByteString                       as B
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Network.HTTP.Client.MultipartFormData (Part, partBS, partFile)


-- |An arguments for printer.
data PrintDetail = PrintDetail
    { cpn   :: Int      -- ^CPN 部数
    , colt  :: String   -- ^COLT ソート(1部ごと)
    , dup   :: String   -- ^DUP 両面
    , clr   :: String   -- ^CLR カラーモード
    , stpl  :: String   -- ^STPL ホチキス
    , pnch  :: String   -- ^PNCH パンチ
    , ot    :: String   -- ^OT 排出先: MT(排出トレイ) FIN (フィニッシャートレイ)
    , it    :: String   -- ^IT 用紙トレイ: AUTO(自動) T1(トレイ1) T2(トレイ2) T3(トレイ3) T4(トレイ4) SMH(トレイ5(手差し))
    , siz   :: String   -- ^SIZ 用紙サイズ
    , med   :: String   -- ^MED 用紙種類
    , del   :: String   -- ^DEL プリント種類
    , ppusr :: String   -- ^PPUSR user for a sample print
    , hour  :: String   -- ^HOUR (0-23)
    , min   :: String   -- ^MIN  (0-59)
    , spusr :: String   -- ^SPUSR userid for security print
    , spid  :: String   -- ^SPID  password
    , rspid :: String   -- ^RSPID retyped password
    , espid :: String   -- ^ESPID a hidden input
    , file  :: FilePath -- ^FILE ファイル
    }

defaultPrintDetail :: PrintDetail
defaultPrintDetail = PrintDetail 1 "NO" "NO" "AUTO" "NO" "NO" "MT" "AUTO" "NUL" "NUL" "IMP" "" "" "" "" "" "" "on" ""


-- |Commit a security print
securityPrint :: String   -- ^Userid
              -> String   -- ^Password
              -> FilePath -- ^Path to file
              -> PrintDetail
securityPrint username password filepath = defaultPrintDetail
    { del   = "SECP"
    , spusr = username
    , spid  = password
    , rspid = password
    , file  = filepath
    }


{- |Convert a 'PrintDetail' to multiparts.
    This is used by 'Cwis.OrderPrint'.
-}
toParts :: PrintDetail -> [Part]
toParts = ap toParts' . pure

toParts' :: [PrintDetail -> Part]
toParts' =
    [ partBS "CPN"   . packEncodeUtf8 . show . cpn
    , partBS "COLT"  . packEncodeUtf8 . colt
    , partBS "DUP"   . packEncodeUtf8 . dup
    , partBS "CLR"   . packEncodeUtf8 . clr
    , partBS "STPL"  . packEncodeUtf8 . stpl
    , partBS "PNCH"  . packEncodeUtf8 . pnch
    , partBS "OT"    . packEncodeUtf8 . ot
    , partBS "IT"    . packEncodeUtf8 . it
    , partBS "SIZ"   . packEncodeUtf8 . siz
    , partBS "MED"   . packEncodeUtf8 . med
    , partBS "DEL"   . packEncodeUtf8 . del
    , partBS "PPUSR" . packEncodeUtf8 . ppusr
    , partBS "HOUR"  . packEncodeUtf8 . hour
    , partBS "MIN"   . packEncodeUtf8 . Cwis.PrintDetail.min
    , partBS "SPUSR" . packEncodeUtf8 . spusr
    , partBS "SPID"  . packEncodeUtf8 . spid
    , partBS "RSPID" . packEncodeUtf8 . rspid
    , partBS "ESPID" . packEncodeUtf8 . espid
    , partFile "FILE" . file
    ]

packEncodeUtf8 :: String -> B.ByteString
packEncodeUtf8 = T.encodeUtf8 . T.pack
