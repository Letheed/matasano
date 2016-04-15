-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

--------------------------------------------------------------------------------
-- | Byte and ByteString types.
--
-- NOTE: This module was written with ASCII characters and strings in mind.
--       The ASCII assumption allows for quick resolutions when decrypting.
--       UTF8 is possible but more costly, so I'm not bothering with it here.
--
--------------------------------------------------------------------------------

module Bytes
  ( -- * Byte types
    Byte, ByteString
    -- * Byte conversion functions
    -- *** String
  , fromString, toString
    -- *** Hexadecimal
  , parseHex, toHex
  ) where

import LibCommon

import Data.Char
import Data.Word

-- | Type representing a byte.
type Byte = Word8

-- | Type representing a string of `Byte`s.
--
-- Obviously neither the fastest nor the most memory-efficient approach
-- but that is not the point here. I'm going for the easiest type to solve
-- the challenges, I'm not trying to make a reference implementation.
type ByteString = [Byte]

-- | Creates a `ByteString` from a `String`.
--
-- NOTE: Again, this works for ASCII `String`s only.
fromString :: String -> ByteString
fromString = map (fromIntegral . ord)

-- | Creates a `String` from a `ByteString`.
--
-- NOTE: Again, this works for ASCII `String`s only.
toString :: ByteString -> String
toString = map (chr . fromIntegral)

-- | Parses a `String` of hexadecimal digits representing bytes.
--
-- Produces an error if the `String` length is odd
-- or if it contains non hexadecimal characters.
parseHex :: String -> ByteString
parseHex = map parseHexPair . pairOff

-- | Parses a pair of hexadecimal digits representing one byte.
parseHexPair :: (Char, Char) -> Byte
parseHexPair (d1, d0) = tens * 16 + ones
  where tens = fromIntegral $ digitToInt d1
        ones = fromIntegral $ digitToInt d0

-- | Converts a `ByteString` into a printable hexadecimal `String`
toHex :: ByteString -> String
toHex []     = []
toHex (b:bs) = toHexDigit d1 : toHexDigit d0 : toHex bs
  where (d1, d0) = b `quotRem` 16
        toHexDigit = intToDigit . fromIntegral
