{- | Cipher commons. -}

module Cipher
       ( -- * Key related types and functions
         Key, KeySchedule
         -- * Cipher classes
       , Cipher(..), BlockCipher(..), ProductCipher(..)
         -- * Initialisation vector
       , IV, ivInit, ivNull
         -- * Modes of operation
       , ecbCipher, ecbDecipher
       , cbcCipher, cbcDecipher
         -- *** Mode detection
       , ecbProbe
       ) where

import Bytes
import Data.Bits
import Data.List
import Data.List.Split

-- | Cipher key.
type Key = ByteString

-- | Key schedule used for ciphering.
type KeySchedule = ByteString

-- | Cipher class.
class Cipher cipher where
  -- | Create a cipher from a Key.
  cipherInit    :: Key -> Maybe cipher
  -- | Get cipher's key size.
  cipherKeySize :: cipher -> Int
  -- | Get cipher's name.
  cipherName    :: cipher -> String

-- | Block cipher class.
class (Cipher cipher) => BlockCipher cipher where
  -- | Get cipher's block size.
  blockSize   :: cipher -> Int
  -- | Cipher a `ByteString` using a `BlockCipher`.
  -- Undefined behavior if `ByteString` length is not blocksize.
  cipherBlock :: cipher -> ByteString -> ByteString
  -- | Decipher a `ByteString` using a `BlockCipher`.
  -- Undefined behavior if `ByteString` length is not blocksize.
  decipherBlock :: cipher -> ByteString -> ByteString

-- | Product cipher class.
class (Cipher cipher) => ProductCipher cipher where
  -- | Get number of repetitive rounds.
  cipherRounds      :: cipher -> Int
  -- | Get the key schedule used for ciphering
  cipherKeySchedule :: cipher -> ByteString

-- | Cipher parametrised initialisation vector.
data IV cipher = IV ByteString

-- | Create an initialisation vector from a `ByteString`
--
-- Return Nothing if the string length differs from the cipher's blocksize
ivInit :: (BlockCipher cipher) => ByteString -> Maybe (IV cipher)
ivInit iv = go undefined
  where go :: (BlockCipher cipher) => cipher -> Maybe (IV cipher)
        go cipher
          | length iv == n = Just (IV iv)
          | otherwise      = Nothing
          where n = blockSize cipher

-- | Create an initialisation vector containing only null bytes.
ivNull :: (BlockCipher cipher) => IV cipher
ivNull = go undefined
  where go :: (BlockCipher cipher) => cipher -> IV cipher
        go cipher = IV (replicate n 0x00)
          where n = blockSize cipher

-- | Cipher a plaintext using the ECB mode with the provided cipher.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
ecbCipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbCipher cipher = concatMap (cipherBlock cipher) . chunksOf n
  where n = blockSize cipher

-- | Decipher a ciphertext using the ECB mode with the provided cipher.
--
-- Undefined behavior if the ciphertext's length is not a multiple of the cipher's blocksize.
ecbDecipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbDecipher cipher = concatMap (decipherBlock cipher) . chunksOf n
  where n = blockSize cipher


-- | Cipher a plaintext using the CBC mode with the provided cipher
-- and initialisation vector.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
cbcCipher :: (BlockCipher cipher) => cipher -> IV cipher -> ByteString -> ByteString
cbcCipher cipher (IV iv) plaintext = ciphertext
  where n = blockSize cipher
        ciphertext = concatMap (cipherBlock cipher) inblocks
        inblocks   = chunksOf n $ zipWith xor plaintext (iv ++ ciphertext)

-- | Decipher a plaintext using the CBC mode with the provided cipher
-- and initialisation vector.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
cbcDecipher :: (BlockCipher cipher) => cipher -> IV cipher -> ByteString -> ByteString
cbcDecipher cipher (IV iv) ciphertext = plaintext
  where n = blockSize cipher
        plaintext = zipWith xor outblocks (iv ++ ciphertext)
        outblocks = concatMap (decipherBlock cipher) (chunksOf n ciphertext)

-- | Grade the likelyhood of a ciphertext to be the result of a block ciphering
-- using the ECB mode, with the given blocksize.
ecbProbe :: Int -> ByteString -> Int
ecbProbe n = sum . map countDuplicates . tails . chunksOf n
  where countDuplicates lst = case lst of
          []   -> 0
          [_]  -> 0
          x:xs -> length . filter (== x) $ xs
