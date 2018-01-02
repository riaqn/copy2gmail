{-# LANGUAGE FlexibleContexts #-}
module Main where

import Network.Connection
import Network.IMAP
import Network.IMAP.Types
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Environment
import System.Exit
import Data.List
import qualified Data.Text as T

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS

folder2tags :: String -> [T.Text]
folder2tags f = map T.pack $ if elem f ["v2ex"] then []
  else if elem f ["feed", "haskell-cafe", "smzdm-faxian", "smzdm", "smzdm-haitao"] then [f]
  else ["INBOX", f]

tryappend :: String -> BS.ByteString -> IO (Either T.Text [[UntaggedResult]])
tryappend folder msg = runExceptT $ do  
  let tls = TLSSettingsSimple False False False
  let params = ConnectionParams "imap.gmail.com" 993 (Just tls) Nothing
  conn <- lift $ connectServer params Nothing
  username <- lift $ getEnv "USERNAME"
  password <- lift $ getEnv "PASSWORD"
  ExceptT $ simpleFormat $ login conn (T.pack username) (T.pack password)
  let tags = folder2tags folder
  let try tag =
        let loop = do
              putStrLn "try append"
              r <- simpleFormat $ append conn tag msg Nothing Nothing
              case r of
                Left e -> if isPrefixOf "[TRYCREATE]" (T.unpack e) then do
                  putStrLn "folder inexists, try create"
                  r <- simpleFormat $ create conn tag
                  case r of
                    Left e -> error $ T.unpack e
                    Right b -> return ()
                  loop
                  else do
                  error $ T.unpack e
                Right b -> return b
        in loop
    in lift $ mapM try tags
    
main :: IO ()
main = do
  e <- runExceptT $ do
    args <- lift getArgs
    folder <- case args of
      [] -> throwE "pass the folder name as argument"
      [x] -> return x
      _ -> throwE "too many arguments"
    msg <- lift BS.getContents
    r <- lift $ tryappend folder msg
    case r of
      Left e -> throwE $ T.unpack e
      Right b -> return b
  case e of
    Left e -> do
      putStrLn e 
      exitFailure
    Right _ -> return ()

