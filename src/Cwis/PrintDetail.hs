{-# LANGUAGE OverloadedStrings #-}

module Cwis.PrintDetail
    ( methodToParts
    ) where

import           Control.Monad
import           Cwis.PrintMethod
import qualified Data.ByteString                       as B
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Network.HTTP.Client.MultipartFormData (Part, partBS, partFile)


-- |Converts 'PrintMethod' to multiparts.
methodToParts :: PrintMethod -> [Part]
methodToParts = detailToParts . methodToDetail

-- |Converts 'PrintMethod' to 'PrintDetail' which represents lower-layerd arguments.
methodToDetail :: PrintMethod -> PrintDetail
methodToDetail (SecurityPrint username password file (CommonOptions cpn colt dup clr stpl pnch ot it siz med)) =
    PrintDetail
        (numCopiesToDetail cpn)
        (doSortToDetail colt)
        (onBothSidesToDetail dup)
        (colourToDetail clr)
        (withStapleToDetail stpl)
        (withPunchToDetail pnch)
        (outputTrayToDetail ot)
        (inputTrayToDetail it)
        (paperSizeToDetail siz)
        (paperTypeToDetail med)
        "SECP"  -- for security-prints
        ""  -- ppusr
        ""  -- hour
        ""  -- min
        username
        password
        password
        "on"  -- espid
        file

numCopiesToDetail :: Int -> Int
numCopiesToDetail = id

doSortToDetail :: Maybe Bool -> String
doSortToDetail Nothing = "NO"
doSortToDetail (Just True) = "ON"
doSortToDetail (Just False) = "OFF"

onBothSidesToDetail :: Maybe Duplex -> String
onBothSidesToDetail Nothing = "NO"
onBothSidesToDetail (Just LongEdge) = "DP"
onBothSidesToDetail (Just ShortEdge) = "TB"

colourToDetail :: Colour -> String
colourToDetail ColourAuto = "AUTO"
colourToDetail MultiColoured = "CLR"
colourToDetail MonoColoured = "BW"

withStapleToDetail :: Maybe Staple -> String
withStapleToDetail Nothing = "NO"
withStapleToDetail (Just UpperLeft) = "UL"
withStapleToDetail (Just LowerLeft) = "LL"
withStapleToDetail (Just UpperRight) = "UR"
withStapleToDetail (Just LowerRight) = "LR"
withStapleToDetail (Just Top2) = "TD"
withStapleToDetail (Just Bottom2) = "BD"
withStapleToDetail (Just Left2) = "LD"
withStapleToDetail (Just Right2) = "RD"

withPunchToDetail :: Maybe Punch -> String
withPunchToDetail Nothing = "NO"
withPunchToDetail (Just PunchTop2) = "TD"
withPunchToDetail (Just PunchBottom2) = "BD"
withPunchToDetail (Just PunchLeft2) = "LD"
withPunchToDetail (Just PunchRight2) = "RD"
withPunchToDetail (Just PunchTop4) = "TF"
withPunchToDetail (Just PunchBottom4) = "BF"
withPunchToDetail (Just PunchLeft4) = "LF"
withPunchToDetail (Just PunchRight4) = "RF"

outputTrayToDetail :: OutputTray -> String
outputTrayToDetail OutputTray   = "MT"
outputTrayToDetail FinisherTray = "FIN"

inputTrayToDetail :: InputTray -> String
inputTrayToDetail InputTrayAuto = "AUTO"
inputTrayToDetail Tray1         = "T1"
inputTrayToDetail Tray2         = "T2"
inputTrayToDetail Tray3         = "T3"
inputTrayToDetail Tray4         = "T4"
inputTrayToDetail ManualFeed    = "SMH"

paperSizeToDetail :: PaperSize -> String
paperSizeToDetail SizeAuto = "NUL"

paperTypeToDetail :: PaperType -> String
paperTypeToDetail TypeAuto = "NUL"

-- |Primitive arguments for printer.
data PrintDetail = PrintDetail
    { _cpn   :: Int      -- ^CPN 部数
    , _colt  :: String   -- ^COLT ソート(1部ごと)
    , _dup   :: String   -- ^DUP 両面
    , _clr   :: String   -- ^CLR カラーモード
    , _stpl  :: String   -- ^STPL ホチキス
    , _pnch  :: String   -- ^PNCH パンチ
    , _ot    :: String   -- ^OT 排出先: MT(排出トレイ) FIN (フィニッシャートレイ)
    , _it    :: String   -- ^IT 用紙トレイ: AUTO(自動) T1(トレイ1) T2(トレイ2) T3(トレイ3) T4(トレイ4) SMH(トレイ5(手差し))
    , _siz   :: String   -- ^SIZ 用紙サイズ
    , _med   :: String   -- ^MED 用紙種類
    , _del   :: String   -- ^DEL プリント種類
    , _ppusr :: String   -- ^PPUSR user for a sample print
    , _hour  :: String   -- ^HOUR (0-23)
    , _min   :: String   -- ^MIN  (0-59)
    , _spusr :: String   -- ^SPUSR userid for security print
    , _spid  :: String   -- ^SPID  password
    , _rspid :: String   -- ^RSPID retyped password
    , _espid :: String   -- ^ESPID a hidden input
    , _file  :: FilePath -- ^FILE ファイル
    }

-- |Convert a 'PrintDetail' to multiparts.
detailToParts :: PrintDetail -> [Part]
detailToParts = ap detailToParts' . pure

detailToParts' :: [PrintDetail -> Part]
detailToParts' =
    [ partBS "CPN"   . packEncodeUtf8 . show . _cpn
    , partBS "COLT"  . packEncodeUtf8 . _colt
    , partBS "DUP"   . packEncodeUtf8 . _dup
    , partBS "CLR"   . packEncodeUtf8 . _clr
    , partBS "STPL"  . packEncodeUtf8 . _stpl
    , partBS "PNCH"  . packEncodeUtf8 . _pnch
    , partBS "OT"    . packEncodeUtf8 . _ot
    , partBS "IT"    . packEncodeUtf8 . _it
    , partBS "SIZ"   . packEncodeUtf8 . _siz
    , partBS "MED"   . packEncodeUtf8 . _med
    , partBS "DEL"   . packEncodeUtf8 . _del
    , partBS "PPUSR" . packEncodeUtf8 . _ppusr
    , partBS "HOUR"  . packEncodeUtf8 . _hour
    , partBS "MIN"   . packEncodeUtf8 . _min
    , partBS "SPUSR" . packEncodeUtf8 . _spusr
    , partBS "SPID"  . packEncodeUtf8 . _spid
    , partBS "RSPID" . packEncodeUtf8 . _rspid
    , partBS "ESPID" . packEncodeUtf8 . _espid
    , partFile "FILE" . _file
    ]

-- |Converts given 'String' to strict 'ByteString' encoding to utf-8.
packEncodeUtf8 :: String -> B.ByteString
packEncodeUtf8 = T.encodeUtf8 . T.pack
