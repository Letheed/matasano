{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------------------
-- | Padding methods and functions.
--
--------------------------------------------------------------------------------

module Padding
  ( -- * Padding methods
    Padding(..)
    -- * Padding functions
  , pad, depad
  ) where

import Bytes
import Data.List.Split

-- | Padding method.
data Padding = PKCS7 Int -- ^ PKCS#7 padding method (RFC 5652) with blocksize parameter.

-- | Pads a `ByteString` using a `Padding` method.
pad :: Padding -> ByteString -> ByteString
pad padding bs = case padding of
  PKCS7 n -> bs ++ padString
    where lastChunk = last $ chunksOf n bs
          padSize = let size = n - length lastChunk in (if size == 0 then n else size)
          padString = replicate padSize (fromIntegral padSize)

-- | Depads a `ByteString` using a `Padding` method.
depad :: Padding -> ByteString -> ByteString
depad padding bs = case padding of
  PKCS7 n -> if | any (/= p) ps -> error "depad: invalid or corrupted padding (PKCS7)"
                | otherwise     -> bs'
                where (bs', p:ps) = depadLastIsCount n bs

-- | Depads a `ByteString` where the last `Byte`'s value represents
-- the length of the padding, given a blocksize.
--
-- Returns the depadded `ByteString` and the dropped padding for checks
-- specific to the `Padding` method.
depadLastIsCount :: Int -> ByteString -> (ByteString, ByteString)
depadLastIsCount n bs
  | length lastChunk /= n = error "depad: bytestring length not a multiple of blocksize"
  | padSize > n           = error "depad: padding length counter > blocksize"
  | null padString        = error "depad: padding length counter is null"
  | otherwise             = (concat (init chunks) ++ lastChunk', padString)
  where chunks    = chunksOf n bs
        lastChunk = last chunks
        padSize   = fromIntegral $ last lastChunk
        (lastChunk', padString) = splitAt (n-padSize) lastChunk
