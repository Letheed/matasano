{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------------------
-- | Base64 encoding and decoding operations.
--
--------------------------------------------------------------------------------

module Base64
  ( -- * Base64 related types
    B64String
    -- * Base64 conversion functions
    -- *** Bytes
  , fromBytes, toBytes
  ) where

import Bytes

import Data.Bits
import Data.Char
import Data.Vector (Vector, fromList, (!))

-- | Base64 string.
type B64String = String

-- | Base64 padding character.
padCharB64 :: Char
padCharB64 = '='

-- | Table of characters mapping 6-bit words to base64 character.
tableCharB64 :: Vector Char
tableCharB64 = fromList $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']

-- | Creates a base64 string representing a `ByteString`.
fromBytes :: ByteString -> B64String
fromBytes (b0:b1:b2:rest) = from3B b0 b1 b2 ++ fromBytes rest
fromBytes [b0, b1]        = from2B b0 b1
fromBytes [b0]            = from1B b0
fromBytes []              = []

-- | Maps three bytes to base64 characters.
from3B :: Byte -> Byte -> Byte -> B64String
from3B b0 b1 b2 = [c0, c1, c2, c3]
  where c0 = fromByte $ shift b0 (-2)
        c1 = fromByte $ shift (b0 .&. 0x3) 4 .|. shift b1 (-4)
        c2 = fromByte $ shift (b1 .&. 0xf) 2 .|. shift b2 (-6)
        c3 = fromByte $ b2 .&. 0x3f

-- | Maps two bytes to base64 characters.
from2B :: Byte -> Byte -> B64String
from2B b0 b1 = [c0, c1, c2, padCharB64]
  where c0 = fromByte $ shift b0 (-2)
        c1 = fromByte $ shift (b0 .&. 0x3) 4 .|. shift b1 (-4)
        c2 = fromByte $ shift (b1 .&. 0xf) 2

-- | Maps one byte to base64 characters.
from1B :: Byte -> B64String
from1B b0 = [c0, c1, padCharB64, padCharB64]
  where c0 = fromByte $ shift b0 (-2)
        c1 = fromByte $ shift (b0 .&. 0x3) 4

-- | Maps a 6-bit word to a base64 character.
fromByte :: Byte -> Char
fromByte = (tableCharB64 !) . fromIntegral

-- | Parses a base64 string into a `ByteString`.
--
-- Produces an error if the string is invalid.
toBytes :: B64String -> ByteString
toBytes (c0:c1:c2:c3:rest) = toBytes' c0 c1 c2 c3 ++ toBytes rest
  where toBytes' c0 c1 c2 c3
          | c3 == padCharB64 = if | not . null $ rest -> error "base64: invalid padding position"
                                  | c2 == padCharB64  -> [b0]
                                  | otherwise         -> [b0, b1]
          | otherwise        =                           [b0, b1, b2]
          where b0 = shift c0' 2 .|. shift c1' (-4)
                b1 = shift c1' 4 .|. shift c2' (-2)
                b2 = shift c2' 6 .|. c3'
                c0' = toByte c0
                c1' = toByte c1
                c2' = toByte c2
                c3' = toByte c3
toBytes []                 = []
toBytes _                  = error "base64: invalid string length"

-- | Parses a base64 character into a 6-bit word.
--
-- Produces an error if character is not base64.
toByte :: Char -> Byte
toByte c = fromIntegral d
  where d | 'A' <= c && c <= 'Z' = ord c - 65
          | 'a' <= c && c <= 'z' = ord c - 71
          | '0' <= c && c <= '9' = ord c + 4
          | c == '+'             = 62
          | c == '/'             = 63
          | otherwise            = error "base64: invalid character"
