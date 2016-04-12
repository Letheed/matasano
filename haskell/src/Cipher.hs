--------------------------------------------------------------------------------
-- | Cipher commons.
--
--------------------------------------------------------------------------------

module Cipher
  ( -- * Key related types and functions
    Key, KeySchedule
    -- * Cipher classes
  , Cipher(..), BlockCipher(..), ProductCipher(..)
    -- * Initialisation vector
  , IV, ivInit, ivNull
    -- * Modes of operation
  , OperationMode(..)
  , ecbCipher, ecbDecipher
  , cbcCipher, cbcDecipher
    -- * Decryption
  , ecbProbe, ecbBlockSize
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
  -- | Creates a cipher from a Key.
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
  -- Undefined behavior if `ByteString` length is not blocksize.
  cipherBlock :: cipher -> ByteString -> ByteString
  -- | Deciphers a `ByteString` using a `BlockCipher`.
  --
  -- Undefined behavior if `ByteString` length is not blocksize.
  decipherBlock :: cipher -> ByteString -> ByteString

-- | Product cipher class.
class (Cipher cipher) => ProductCipher cipher where
  -- | Returns the number of repetitive rounds.
  cipherRounds      :: cipher -> Int
  -- | Returns the key schedule used for ciphering
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
                   deriving (Eq)

-- | Ciphers a plaintext using the `ECB` mode with the provided cipher.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
ecbCipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbCipher cipher = concatMap (cipherBlock cipher) . chunksOf n
  where n = blockSize cipher

-- | Deciphers a ciphertext using the `ECB` mode with the provided cipher.
--
-- Undefined behavior if the ciphertext's length is not a multiple of the cipher's blocksize.
ecbDecipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbDecipher cipher = concatMap (decipherBlock cipher) . chunksOf n
  where n = blockSize cipher


-- | Ciphers a plaintext using the `CBC` mode with the provided cipher
-- and initialisation vector.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
cbcCipher :: (BlockCipher cipher) => cipher -> IV cipher -> ByteString -> ByteString
cbcCipher cipher (IV iv) plaintext = ciphertext
  where n = blockSize cipher
        ciphertext = concatMap (cipherBlock cipher) inblocks
        inblocks   = chunksOf n $ zipWith xor plaintext (iv ++ ciphertext)

-- | Deciphers a plaintext using the `CBC` mode with the provided cipher
-- and initialisation vector.
--
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
cbcDecipher :: (BlockCipher cipher) => cipher -> IV cipher -> ByteString -> ByteString
cbcDecipher cipher (IV iv) ciphertext = plaintext
  where n = blockSize cipher
        plaintext = zipWith xor outblocks (iv ++ ciphertext)
        outblocks = concatMap (decipherBlock cipher) (chunksOf n ciphertext)

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
