{- | Byte and ByteString types.

NOTE: This module was written with ASCII characters and strings in mind.
      The ASCII assumption allows for quick resolutions when decrypting.
      UTF8 is possible but more costly, so I'm not bothering with it here.
-}

module Bytes
       ( Byte, ByteString
       , fromString, toString
       , parseHex
       ) where

import LibCommon

import Data.Char
import Data.Word

-- | Type representing a byte.
type Byte = Word8

-- | Type representing a byte string.
--
-- Obviously neither the fastest nor the most memory-efficient approach
-- but that is not the point here. I'm going for the easiest type to solve
-- the challenges, I'm not trying to make a reference implementation.
type ByteString = [Byte] 

-- | Create a `ByteString` from a `String`.
--
-- NOTE: Again, this works for ASCII `String`s only.
fromString :: String -> ByteString
fromString = map (fromIntegral . ord)

-- | Create a `String` from a `ByteString`.
--
-- NOTE: Again, this works for ASCII `String`s only.
toString :: ByteString -> String
toString = map (chr . fromIntegral)

-- | Parse a `String` of hexadecimal digits representing bytes.
parseHex :: String -> ByteString
parseHex = map parseHexPair . pairOff

-- | Parse a pair of hexadecimal digits representing one byte.
parseHexPair :: (Char, Char) -> Byte
parseHexPair (d1, d0) = tens * 16 + ones
  where tens = fromIntegral $ digitToInt d1
        ones = fromIntegral $ digitToInt d0