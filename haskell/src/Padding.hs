-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------------------
-- | Padding methods and operations.
--
--------------------------------------------------------------------------------

module Padding
  ( -- * Padding methods
    PaddingType(..)
  , Padding, mkPadding
    -- * Padding functions
  , pad, depad
  ) where

import Bytes
import Data.List.Split
import Data.Result

-- | Padding method.
data PaddingType = PKCS7 -- ^ PKCS#7 padding method (RFC 5652).
                         -- Supports blocksizes in [0..255]

-- | Opaque padding method with blocksize parameter.
data Padding = Padding PaddingType Int

-- | Creates a `Padding` from a `PaddingType` and a blocksize.
--
-- Returns `Nothing` if the requested blocksize is invalid with the padding method.
mkPadding :: PaddingType -> Int -> Maybe Padding
mkPadding padding n = case padding of
  PKCS7 -> if | n < 0 || 255 < n -> Nothing
              | otherwise        -> Just (Padding PKCS7 n)

-- | Pads a `ByteString` using a `Padding` method.
pad :: Padding -> ByteString -> ByteString
pad (Padding padding n) bs = case padding of
  PKCS7 -> bs ++ padString
    where lastChunk = last $ chunksOf n bs
          padSize = let size = n - length lastChunk in (if size == 0 then n else size)
          padString = replicate padSize (fromIntegral padSize)

-- | Depads a `ByteString` using a `Padding` method.
--
-- Returns an `Err` if the padding is invalid or the file is corrupted.
depad :: Padding -> ByteString -> Result String ByteString
depad (Padding padding n) bs = case padding of
  PKCS7 -> depadLastIsCount n bs `andThen` checkPaddingWasValid
    where checkPaddingWasValid (bs', p:ps)
            | any (/= p) ps = Err "depad: invalid or corrupted padding (PKCS7)"
            | otherwise     = Ok bs'

-- | Depads a `ByteString` where the last `Byte`'s value represents
-- the length of the padding, given a blocksize.
--
-- Returns the depadded `ByteString` and the dropped padding for checks
-- specific to the `Padding` method.
depadLastIsCount :: Int -> ByteString -> Result String (ByteString, ByteString)
depadLastIsCount n bs
  | length lastChunk /= n = Err "depad: bytestring length not a multiple of blocksize"
  | padSize > n           = Err "depad: padding length counter > blocksize"
  | null padString        = Err "depad: padding length counter is null"
  | otherwise             = Ok (concat (init chunks) ++ lastChunk', padString)
  where chunks    = chunksOf n bs
        lastChunk = last chunks
        padSize   = fromIntegral $ last lastChunk
        (lastChunk', padString) = splitAt (n-padSize) lastChunk
