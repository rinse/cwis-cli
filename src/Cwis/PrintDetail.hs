{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cwis.PrintDetail
    ( PrintDetail
    , cpn           -- |It supplies the number of copies for 'PrintDetail'
    , securityPrint
    , toParts
    ) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString                       as B
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Network.HTTP.Client.MultipartFormData (Part, partBS, partFile)


{- |An arguments for printer.

    Use a smart constructor to get a 'PrintDetail'.
    Use 'Lens' operators to give options.

    ==== __Examples__
    To have a 'PrintDetail' for 2 copies of security print:

    @securityPrint "name" "password" "path to file" & cpn .~ 2@
-}
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
makeLenses ''PrintDetail

defaultPrintDetail :: PrintDetail
defaultPrintDetail = PrintDetail 1 "NO" "NO" "AUTO" "NO" "NO" "MT" "AUTO" "NUL" "NUL" "IMP" "" "" "" "" "" "" "on" ""

{- |For safe construction of 'PrintDetail'.
    It supplies a PrintDetail for a security print.
-}
securityPrint :: String   -- ^Userid
              -> String   -- ^Password
              -> FilePath -- ^Path to file
              -> PrintDetail
securityPrint username password filepath = defaultPrintDetail
    { _del   = "SECP"
    , _spusr = username
    , _spid  = password
    , _rspid = password
    , _file  = filepath
    }

{- |Convert a 'PrintDetail' to multiparts.
    This is used by 'Cwis.OrderPrint'.
-}
toParts :: PrintDetail -> [Part]
toParts = ap toParts' . pure

toParts' :: [PrintDetail -> Part]
toParts' =
    [ partBS "CPN"   . packEncodeUtf8 . show . view cpn
    , partBS "COLT"  . packEncodeUtf8 . view colt
    , partBS "DUP"   . packEncodeUtf8 . view dup
    , partBS "CLR"   . packEncodeUtf8 . view clr
    , partBS "STPL"  . packEncodeUtf8 . view stpl
    , partBS "PNCH"  . packEncodeUtf8 . view pnch
    , partBS "OT"    . packEncodeUtf8 . view ot
    , partBS "IT"    . packEncodeUtf8 . view it
    , partBS "SIZ"   . packEncodeUtf8 . view siz
    , partBS "MED"   . packEncodeUtf8 . view med
    , partBS "DEL"   . packEncodeUtf8 . view del
    , partBS "PPUSR" . packEncodeUtf8 . view ppusr
    , partBS "HOUR"  . packEncodeUtf8 . view hour
    , partBS "MIN"   . packEncodeUtf8 . view Cwis.PrintDetail.min
    , partBS "SPUSR" . packEncodeUtf8 . view spusr
    , partBS "SPID"  . packEncodeUtf8 . view spid
    , partBS "RSPID" . packEncodeUtf8 . view rspid
    , partBS "ESPID" . packEncodeUtf8 . view espid
    , partFile "FILE" . view file
    ]

packEncodeUtf8 :: String -> B.ByteString
packEncodeUtf8 = T.encodeUtf8 . T.pack
