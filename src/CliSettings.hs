{-# LANGUAGE DeriveAnyClass #-}

module CliSettings where

import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Text (Text)
import GHC.Generics
import Data.Aeson (decode, FromJSON, ToJSON)
import Data.Set (Set)

data ClientApiType = LainJSON | TinyboardHTML
    deriving (Eq, Show, Generic)

instance FromJSON ClientApiType

data JSONSiteSettings = JSONSiteSettings
    { name :: String
    , root_url :: String
    , boards :: [ String ]
    , client_api_type :: ClientApiType
    } deriving (Show, Generic)

instance FromJSON JSONSiteSettings

data ConsumerJSONSettings = ConsumerJSONSettings
    { websites :: [ JSONSiteSettings ]
    , postgrest_url :: String
    , jwt :: Text
    , media_root_path :: String
    , sync_max_concurrent_workers :: Int
    , sync_loop_timeout_microseconds :: Int
    , spam_noticer :: Maybe SpamNoticerSettings
    } deriving (Show, Generic)

instance FromJSON ConsumerJSONSettings

data SpamNoticerSettings =
    SpamNoticerSettings
        { base_url :: String
        , max_concurrent_requests :: Int
        , trusted_sites :: Maybe (Set String)
        } deriving (Show, Generic, ToJSON, FromJSON)

newtype CliArgs = CliArgs
  { settingsFile :: String
  } deriving (Show, Data, Typeable)

getSettings :: IO ConsumerJSONSettings
getSettings = do
    cliArgs <- cmdArgs $ CliArgs "consumer_settings.json"

    let filePath = settingsFile cliArgs
    if null filePath
    then do
        putStrLn "Error: No JSON settings file provided."
        exitFailure
    else do
        putStrLn $ "Loading settings from: " ++ filePath
        content <- B.readFile filePath
        case decode content :: Maybe ConsumerJSONSettings of
            Nothing -> do
                putStrLn "Error: Invalid JSON format."
                exitFailure
            Just settings -> return settings
