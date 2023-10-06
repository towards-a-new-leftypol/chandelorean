{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module DataClient
  ( HttpError(..)
  , get
  , getSiteBoards
  , getAllSites
  , postSite
  , post
  , SiteResponse(..)
  ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.HTTP.Types.Status (statusCode)
import Control.Exception.Safe (tryAny, SomeException)
import qualified Data.ByteString.Char8 as C8
import Data.Aeson
  ( eitherDecode
  , FromJSON
  , (.=)
  , object
  , encode
  )
import GHC.Generics
import qualified Types as T

data HttpError
    = HttpException SomeException
    | StatusCodeError Int LBS.ByteString
    deriving (Show)

data SiteResponse = SiteResponse
    { site_id :: Int
    , name :: String
    , url :: String
    } deriving (Show, Generic, FromJSON)


get :: T.JSONSettings -> String -> IO (Either HttpError LBS.ByteString)
get settings path = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestHeader "Authorization" [C8.pack $ "Bearer " ++ T.jwt settings] initReq
    putStrLn $ "calling " ++ requestUrl
    handleHttp (httpLBS req)


post
  :: T.JSONSettings
  -> String
  -> LBS.ByteString
  -> Bool
  -> IO (Either HttpError LBS.ByteString)
post settings path payload return_repr = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestMethod "POST"
            . setRequestHeader "Authorization" [ jwt_header ]
            . setRequestHeader "Content-Type" [ "application/json" ]
            . setRequestBodyLBS payload
            . prefer
            $ initReq

    putStrLn $ "posting to " ++ requestUrl
    handleHttp (httpLBS req)

    where
      jwt_header = C8.pack $ "Bearer " ++ T.jwt settings
      prefer =
        if return_repr
        then setRequestHeader "Prefer" [ "return=representation" ]
        else id


handleHttp :: IO (Response LBS.ByteString) -> IO (Either HttpError LBS.ByteString)
handleHttp action = do
    result <- tryAny action
    case result of
        Right response -> 
            let responseBody = getResponseBody response
            in if 200 <= (statusCode $ getResponseStatus response) && (statusCode $ getResponseStatus response) < 300
               then return $ Right responseBody
               else return $ Left (StatusCodeError (statusCode $ getResponseStatus response) responseBody)
        Left e -> return $ Left $ HttpException e


getSiteBoards :: T.JSONSettings -> Int -> IO (Either HttpError [ String ])
getSiteBoards settings site_id_ = get settings path >>= return . eitherDecodeResponse
  where
    path = "/boards?select=name&boards.site_id=eq." ++ show site_id_


postSite :: T.JSONSettings -> IO (Either HttpError SiteResponse)
postSite settings =
    post settings "/sites" payload True >>= return . eitherDecodeResponse

    where
      payload = encode $
            object [ "name" .= T.site_name settings
                   , "url"  .= T.site_url  settings
                   ]


getAllSites :: T.JSONSettings -> IO (Either HttpError [SiteResponse])
getAllSites settings = get settings "/sites" >>= return . eitherDecodeResponse

eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err
