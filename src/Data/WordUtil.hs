-- These two functions were stolen from the module
-- Data.ProtoLens.Encoding.Bytes from the proto-lens library.

module Data.WordUtil
( signedInt64ToWord
, wordToSignedInt64
) where

import Data.Int (Int64)
import Data.Word (Word64)
import Data.Bits ((.&.), xor, shiftR, shiftL)

signedInt64ToWord :: Int64 -> Word64
signedInt64ToWord n = fromIntegral $ shiftL n 1 `xor` shiftR n 63

wordToSignedInt64 :: Word64 -> Int64
wordToSignedInt64 n
    = fromIntegral (shiftR n 1) `xor` negate (fromIntegral $ n .&. 1)
