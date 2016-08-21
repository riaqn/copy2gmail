{-# LANGUAGE FlexibleContexts #-}
module Main where

import Network.Connection
import Network.IMAP
import Network.IMAP.Types
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.Text as T

folder2tags :: T.Text -> [T.Text]
{-
folder2tags f = let f' = T.unpack f in
                  if f' == "v2ex" then []
                  -- if sent to v2ex@riaqn.org, drop the mail
                  else if f' == "feed" then [f]
                  -- if sent to feed@riaqn.org, add the "feed" tag,
                  -- this mail won't be shown in INBOX
                  else [f, (T.pack "INBOX")]
                  -- otherwise for foo@riaqn.org, attach the mail with
                  -- the tag "foo" as well as put it into INBOX
-}

folder2tags f = [f]

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
    Left e -> putStrLn (T.unpack e)
    Right _ -> return ()

