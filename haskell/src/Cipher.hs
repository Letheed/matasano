-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

--------------------------------------------------------------------------------
-- | Cipher class definitions and modes of operation.
--
--------------------------------------------------------------------------------

module Cipher
  ( -- * Key related types and functions
    Key, KeySchedule
    -- * Cipher classes
  , Cipher(..)
    -- *** Block ciphers
  , BlockCipher(..)
    -- *** Product ciphers
  , ProductCipher(..)
    -- * Initialisation vector
  , IV, ivInit, ivNull
    -- * Modes of operation
  , OperationMode(..)
    -- *** Ciphering / Deciphering
    -- **** ECB
  , ecbCipher, ecbDecipher
    -- **** CBC
  , cbcCipher, cbcDecipher
    -- * Decryption
  , ecbProbe, ecbBlockSize
  ) where

import Bytes
import Padding

import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Result

-- | Cipher key.
type Key = ByteString

-- | Key schedule used for ciphering.
type KeySchedule = ByteString

-- | Cipher class.
class Cipher cipher where
  -- | Creates a cipher from a Key.
  --
  -- Returns `Nothing` if the keysize is inappropriate.
  cipherInit    :: Key -> Maybe cipher
  -- | Returns the cipher's key size.
  cipherKeySize :: cipher -> Int
  -- | Returns the cipher's name.
  cipherName    :: cipher -> String

-- | Block cipher class.
class (Cipher cipher) => BlockCipher cipher where
  -- | Returns the cipher's block size.
  blockSize   :: cipher -> Int
  -- | Ciphers a `ByteString` using a `BlockCipher`.
  --
  -- Returns `Nothing` if the `ByteString` is not the size of a  block.
  cipherBlock :: cipher -> ByteString -> Maybe ByteString
  -- | Deciphers a `ByteString` using a `BlockCipher`.
  --
  -- Returns `Nothing` if the `ByteString` is not the size of a  block.
  decipherBlock :: cipher -> ByteString -> Maybe ByteString

-- | Product cipher class.
class (Cipher cipher) => ProductCipher cipher where
  -- | Returns the number of repetitive rounds.
  cipherRounds      :: cipher -> Int
  -- | Returns the key schedule used for ciphering.
  cipherKeySchedule :: cipher -> ByteString

-- | Cipher parametrised initialisation vector.
data IV cipher = IV ByteString

-- | Creates an initialisation vector from a `ByteString`.
--
-- Returns `Nothing` if the `ByteString`'s length differs from the cipher's blocksize.
ivInit :: (BlockCipher cipher) => ByteString -> Maybe (IV cipher)
ivInit iv = go undefined
  where go :: (BlockCipher cipher) => cipher -> Maybe (IV cipher)
        go cipher
          | length iv == n = Just (IV iv)
          | otherwise      = Nothing
          where n = blockSize cipher

-- | Creates an initialisation vector containing only null bytes.
ivNull :: (BlockCipher cipher) => IV cipher
ivNull = go undefined
  where go :: (BlockCipher cipher) => cipher -> IV cipher
        go cipher = IV (replicate n 0x00)
          where n = blockSize cipher

-- | Operation mode used for ciphering.
data OperationMode = ECB -- ^ Electronic Codebook mode.
                   | CBC -- ^ Cipher Block Chaining mode.
                   deriving (Eq, Enum, Show, Read)

-- | Ciphers a plaintext using the `ECB` mode with the provided cipher and padding method.
ecbCipher :: (BlockCipher cipher) => cipher -> PaddingType -> ByteString -> ByteString
ecbCipher cipher padding = cipherPlaintext . padPlaintext
  where n = blockSize cipher
        padPlaintext    = pad (fromJust . mkPadding padding $ n)
        cipherPlaintext = concatMap (fromJust . cipherBlock cipher) . chunksOf n

-- | Deciphers a ciphertext using the `ECB` mode with the provided cipher and padding method.
--
-- Returns an informative `Err` if the ciphertext's length is not a multiple of the cipher's
-- blocksize or if the depadding failed (wrong `PaddingType` or corrupted file).
ecbDecipher :: (BlockCipher cipher) => cipher -> PaddingType -> ByteString -> Result String ByteString
ecbDecipher cipher padding ciphertext = decipher ciphertext `andThen` depadPlaintext
  where n = blockSize cipher
        depadPlaintext = depad (fromJust . mkPadding padding $ n)
        decipher       = getAndOk . foldMap (AndOk . blame errorString . decipherBlock cipher) . chunksOf n
          where errorString = "ecbDecipher: bad block size"

-- | Ciphers a plaintext using the `CBC` mode with the provided cipher,
-- initialisation vector and padding method.
cbcCipher :: (BlockCipher cipher) => cipher -> IV cipher -> PaddingType -> ByteString -> ByteString
cbcCipher cipher (IV iv) padding plaintext = ciphertext
  where n = blockSize cipher
        ciphertext   = cipherBlocks . mkInblocks . padPlaintext $ plaintext
          where padPlaintext = pad (fromJust . mkPadding padding $ n)
                mkInblocks   = chunksOf n . zipWith xor (iv ++ ciphertext)
                cipherBlocks = concatMap (fromJust . cipherBlock cipher)

-- | Deciphers a plaintext using the `CBC` mode with the provided cipher,
-- initialisation vector and padding method.
--
-- Returns an informative `Err` if the plaintext's length is not a multiple of the cipher's
-- blocksize or if the depadding failed (wrong `PaddingType` or corrupted file).
cbcDecipher :: (BlockCipher cipher) => cipher -> IV cipher -> PaddingType -> ByteString -> Result String ByteString
cbcDecipher cipher (IV iv) padding ciphertext = (xor' <$> decipher ciphertext) `andThen` depadPlaintext
    where n = blockSize cipher
          depadPlaintext = depad (fromJust . mkPadding padding $ n)
          xor'           = zipWith xor (iv ++ ciphertext)
          decipher       = getAndOk . foldMap (AndOk . blame errorString . decipherBlock cipher) . chunksOf n
            where errorString = "cbcbDecipher: bad block size"

-- | Grades the likelyhood for a ciphertext to be the result of a block ciphering
-- using the ECB mode, with the given blocksize.
ecbProbe :: Int -> ByteString -> Int
ecbProbe n = sum . map countDuplicates . tails . chunksOf n
  where countDuplicates lst = case lst of
          []   -> 0
          [_]  -> 0
          x:xs -> length . filter (== x) $ xs

ecbBlockSize :: (ByteString -> ByteString) -> Int
ecbBlockSize = undefined
