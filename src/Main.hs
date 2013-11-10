module Main (main) where

import qualified    Data.ByteString.Char8 as BC         (hPutStrLn)
import              Data.Char                           (toLower)
import qualified    Data.Map as M                       (Map, lookup, fromList)
import              Data.Maybe                          (isJust, fromJust)
import qualified    Data.Text as T                      (Text, pack)
import              Password
import              PasswordJSON
import              System.IO                           (hSetEcho, hGetEcho, stdin, stderr)

getPassword :: IO String
getPassword = withEcho False getLine

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    hSetEcho stdin echo
    res <- action
    hSetEcho stdin old
    return res

type AccountMap = M.Map T.Text Account

accountMap :: [Account] -> AccountMap
accountMap as = M.fromList (map (\a -> (name a, a)) as)

readAccountMap :: IO AccountMap
readAccountMap = do
    as <- accounts
    return (accountMap as)

readMaxLength :: IO (Maybe Int)
readMaxLength = do
    putStrLn "Has Max Len (y/n):"
    hml <- getLine
    let hasMaxLength = (toLower . head) hml == 'y'
    if hasMaxLength
       then do
           putStrLn "Max Length:"
           ln <- getLine
           return $ Just (read ln)
       else
           return Nothing

saveAccount :: Maybe Account -> [Account] -> IO ()
saveAccount (Just _) _ = return ()
saveAccount Nothing as = do
    putStrLn $ "Save account? (y/n):"
    sal <- getLine
    let save = (toLower . head) sal == 'y'
    if save
       then do
           writeAccounts as
       else return ()

main :: IO ()
main = do
    putStrLn "Enter master password:"
    mp <- withEcho False getLine
    putStrLn "Enter account name:"
    an <- getLine
    as <- accounts
    let am = accountMap as
    let maybeAccount = M.lookup (T.pack an) am
    maxLength <- case maybeAccount of
                      Just a -> return (if hasMaxLength a then Just (maxLength a) else Nothing)
                      _      -> readMaxLength
    let newAs = (Account (T.pack an) (isJust maxLength) (if isJust maxLength then fromJust maxLength else 0)):as
    saveAccount maybeAccount newAs
    let pw = genPass mp an maxLength
    BC.hPutStrLn stderr pw
