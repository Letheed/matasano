{- | Cipher commons. -}

module Cipher
       ( Key, KeySchedule
       , Cipher(..), BlockCipher(..), ProductCipher(..)
       , ecbCipher, ecbDecipher
       , ecbProbe
       ) where

import Bytes
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

-- | Cipher a plaintext `ByteString` using a `BlockCipher`.
-- Undefined behavior if the plaintext's length is not a multiple of the cipher's blocksize.
ecbCipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbCipher cipher = concatMap (cipherBlock cipher) . chunksOf n
  where n = blockSize cipher

-- | Decipher a ciphertext `ByteString` using a `BlockCipher`.
-- Undefined behavior if the ciphertext's length is not a multiple of the cipher's blocksize.
ecbDecipher :: (BlockCipher cipher) => cipher -> ByteString -> ByteString
ecbDecipher cipher = concatMap (decipherBlock cipher) . chunksOf n
  where n = blockSize cipher

-- | Grade the likelyhood of a ciphertext to be the result of a block ciphering
-- using the ecb mode of operation, with the given blocksize.
ecbProbe :: Int -> ByteString -> Int
ecbProbe n = sum . map countDuplicates . tails . chunksOf n
  where countDuplicates lst = case lst of
          []   -> 0
          [_]  -> 0
          x:xs -> length . filter (== x) $ xs
