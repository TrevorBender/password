{-# LANGUAGE OverloadedStrings #-}

module PasswordJSON (Account(..), accounts, writeAccounts) where

import              Data.Aeson
import              Data.Text
import              Control.Applicative ((<$>),(<*>))
import              Control.Monad (mzero)
import              System.IO
import              System.Environment
import qualified    Data.ByteString as B
import qualified    Data.ByteString.Char8 as BC
import qualified    Data.ByteString.Lazy as BL
import qualified    Data.ByteString.Lazy.Char8 as BLC

b2bl :: B.ByteString -> BL.ByteString
b2bl strict = BL.fromChunks [strict]

bl2b :: BL.ByteString -> B.ByteString
bl2b lazy = B.concat $ BL.toChunks lazy

data Account = Account
    { name         :: Text
    , hasMaxLength :: Bool
    , maxLength    :: Int
    } deriving (Show)

instance FromJSON Account where
    parseJSON (Object v) = Account
        <$> v .: "name"
        <*> v .: "hasMaxLength"
        <*> v .: "maxLength"
    parseJSON _ = mzero

filename :: IO FilePath
filename = do
    home <- getEnv "HOME"
    return $ home ++ "/.passwordAccounts.json"

accounts :: IO [Account]
accounts = do
    f <- filename
    withFile f ReadMode (\h -> do
        c <- B.hGetContents h
        let mas = decode' (b2bl c) :: Maybe [Account]
            as = case mas of
                      Just as -> as
                      _ -> []
        return as
        )

instance ToJSON Account where
    toJSON (Account name hasML ml) = object
        [ "name" .= name
        , "hasMaxLength" .= hasML
        , "maxLength" .= ml
        ]

test :: IO ()
test = do
    let json = encode [Account "account name" True 16]
    BLC.putStrLn json

writeAccounts :: [Account] -> IO ()
writeAccounts as = do
    f <- filename
    let json = encode as
    BL.writeFile f json

main = do
    f <- filename
    h <- openFile f ReadMode
    c <- B.hGetContents h
    let mas = decode' (b2bl c) :: Maybe [Account]
    putStrLn $ show mas
    hClose h
