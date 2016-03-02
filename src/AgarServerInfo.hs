{-# LANGUAGE OverloadedStrings #-}
module AgarServerInfo where

import Network.HTTP.Conduit
import Control.Monad
import Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- url port token
data ServerInfo = ServerInfo T.Text Int T.Text deriving(Show)

-- make a post request to the given url
makePostReq url body = do
        naked <- parseUrl url
        return (naked {method = "POST", requestBody = RequestBodyBS (E.encodeUtf8 body)})

getServerInfo :: T.Text -> Integer -> IO (ServerInfo)
getServerInfo region version = do
        manager <- newManager tlsManagerSettings
        request <- makePostReq "http://m.agar.io" (T.unlines [region, T.pack $ show version])
        response <- httpLbs request manager
        case T.lines $ E.decodeUtf8 $ toStrict $ responseBody response of
            [url, tok] -> case T.splitOn ":" url of
                              [url, port] -> let p = read $ T.unpack port in
                                  return $ ServerInfo url p tok

