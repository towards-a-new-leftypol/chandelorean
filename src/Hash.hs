{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Hash
( computeSHA256
, computeMD5
) where

import Crypto.Hash (hashlazy, Digest, SHA256, MD5, HashAlgorithm)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteArray.Encoding as BA
import Data.Text (Text)
import qualified Data.Text.Encoding as T

computeHash :: forall a. HashAlgorithm a => FilePath -> IO Text
computeHash filePath = do
    fileData <- B.readFile filePath
    let hashDigest = hashlazy fileData :: Digest a
    return $ T.decodeUtf8 $ BA.convertToBase BA.Base16 hashDigest

-- Function to compute SHA256 hash of a file
computeSHA256 :: FilePath -> IO Text
computeSHA256 = computeHash @SHA256

computeMD5 :: FilePath -> IO Text
computeMD5 = computeHash @MD5
