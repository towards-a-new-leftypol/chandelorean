module Hash
( computeSHA256
) where

import Crypto.Hash (hashlazy, Digest, SHA256)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteArray.Encoding as BA
import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- Function to compute SHA256 hash of a file
computeSHA256 :: FilePath -> IO Text
computeSHA256 filePath = do
    fileData <- B.readFile filePath
    let hashDigest = hashlazy fileData :: Digest SHA256
    return $ T.decodeUtf8 $ BA.convertToBase BA.Base16 hashDigest
