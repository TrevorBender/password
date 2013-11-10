{-# LANGUAGE OverloadedStrings #-}

module Password (genPass) where

import              Crypto.Cipher.AES                   (initAES, encryptECB)
import              Crypto.Padding                      (padPKCS5)
import qualified    Data.ByteString.Char8 as BC         (pack)
import qualified    Data.ByteString.Lazy.Char8 as BLC   (pack)
import qualified    Data.ByteString as B                (ByteString, take, concat)
import qualified    Data.ByteString.Lazy as BL          (ByteString, toChunks, fromChunks)
import              Data.Digest.Pure.SHA                (sha256, Digest, SHA256State, bytestringDigest)
import              Data.ByteString.Base64              (encode, decode)
import              Numeric                             (showHex)

sha256' ::  String -> Digest SHA256State
sha256' = sha256 . BLC.pack

b2bl :: B.ByteString -> BL.ByteString
b2bl strict = BL.fromChunks [strict]

bl2b :: BL.ByteString -> B.ByteString
bl2b lazy = B.concat $ BL.toChunks lazy

aes :: B.ByteString  -- key
    -> B.ByteString  -- message text
    -> B.ByteString -- cipher text

aes k mt = encryptECB (initAES k) (padPKCS5 16 mt)

-- | generate password
genPass :: String        -- ^ master password
        -> String        -- ^ message text
        -> Maybe Int     -- ^ Just maxLength or Nothing
        -> B.ByteString  -- ^ password
genPass mp mt mml = res
  where kd = bl2b $ bytestringDigest $ sha256' mp
        mt' = BC.pack mt
        ct = aes kd mt'
        digest = sha256 (b2bl ct)
        b64 = encode $ bl2b $ bytestringDigest digest
        res = case mml of
                   Just ml -> B.take ml b64
                   _       -> b64

