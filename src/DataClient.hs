{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module DataClient
  ( HttpError(..)
  , get
  , getWebsiteBoards
  , getAllSites
  , postSite
  , post
  , SiteResponse(..)
  ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
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


post :: T.JSONSettings -> String -> LBS.ByteString -> IO (Either HttpError LBS.ByteString)
post settings path payload = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestMethod "POST"
            . setRequestHeader "Authorization" [C8.pack $ "Bearer " ++ T.jwt settings]
            . setRequestHeader "Content-Type" ["application/json"]
            . setRequestBodyLBS payload
            $ initReq
    putStrLn $ "posting to " ++ requestUrl
    handleHttp (httpLBS req)


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


getWebsiteBoards :: T.JSONSettings -> IO (Either HttpError [ String ])
getWebsiteBoards settings = do
  response <- get settings path

  case response of
    Right body -> do
      print body
      undefined
    Left err -> do
      print err
      return $ Left err

  where
    path = "/boards?select=name,board_id,sites(site_id)&sites.name=eq."
              ++ (T.site_name settings)


postSite :: T.JSONSettings -> IO (Either HttpError LBS.ByteString)
postSite settings = do
    let payload = encode $ object ["name" .= T.site_name settings, "url" .= T.site_url settings]
    post settings "/sites" payload


getAllSites :: T.JSONSettings -> IO (Either HttpError [SiteResponse])
getAllSites settings = do
    response <- get settings "/sites"
    case response of
        Right x -> do
          putStrLn "getAllSites response:"
          print x

          return $ case eitherDecode x :: Either String [SiteResponse] of
            Right sites -> Right sites
            Left _ -> Left $ StatusCodeError 500 "Failed to decode JSON"
        Left err -> return $ Left err
