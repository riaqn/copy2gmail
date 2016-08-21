{-# LANGUAGE FlexibleContexts #-}
module Main where

import Network.Connection
import Network.IMAP
import Network.IMAP.Types
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.Except
import System.Environment
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.Text as T

folder2tags :: T.Text -> [T.Text]
folder2tags f = [f, (T.pack "INBOX")]

tryappend :: T.Text -> BS.ByteString -> EitherT T.Text IO [[UntaggedResult]]
tryappend folder msg = do  
  let tls = TLSSettingsSimple False False False
  let params = ConnectionParams "imap.gmail.com" 993 (Just tls) Nothing
  conn <- lift $ connectServer params Nothing
  username <- lift $ getEnv "USERNAME"
  password <- lift $ getEnv "PASSWORD"
  EitherT $ simpleFormat $ login conn (T.pack username) (T.pack password)
  let tags = folder2tags folder
  let try tag =
        let loop = do
              (EitherT $ simpleFormat $ append conn tag msg Nothing Nothing) `catchError`
                (\e -> if T.isPrefixOf (T.pack "[TRYCREATE]") e then do
                    EitherT $ simpleFormat $ create conn tag
                    loop
                       else throwError e)
        in loop
    in mapM try tags
    
main :: IO ()
main = do
  e <- runEitherT $ do
    args <- lift getArgs
    when (length args /= 1) (left $ T.pack "requires exactly one argument representing the folder")
    let folder = args !! 0
    msg <- lift BS.getContents
    tryappend (T.pack folder) msg
  case e of
    Left e -> do
      putStrLn (T.unpack e)
      exitFailure
    Right _ -> return ()

