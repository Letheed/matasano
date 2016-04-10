{- | Cipher commons. -}

module Cipher
       ( ecbGrade
       ) where

import Bytes
import Data.List
import Data.List.Split

-- | Grade the likelyhood of a ciphertext to be the result of a block ciphering
-- using the ecb mode of operation, with the given blocksize.
ecbGrade :: Int -> ByteString -> Int
ecbGrade n = sum . map countDuplicates . tails . chunksOf n
  where countDuplicates lst = case lst of
          []   -> 0
          [_]  -> 0
          x:xs -> length . filter (== x) $ xs

-- -- | Cipher key.
-- type Key = ByteString

-- -- | Key schedule used for ciphering.
-- type KeySchedule = ByteString

-- -- | Cipher class.
-- class Cipher cipher where
--   -- | Create a cipher from a Key.
--   cipherInit    :: Key -> Maybe cipher
--   -- | Get cipher's key size.
--   cipherKeySize :: cipher -> Int
--   -- | Get cipher's name.
--   cipherName    :: cipher -> String

-- -- | Block cipher class.
-- class (Cipher cipher) => BlockCipher cipher where
--   -- | Get cipher's block size.
--   blockSize :: cipher -> Int

-- -- | Product cipher class.
-- class (Cipher cipher) => ProductCipher cipher where
--   -- | Get number of repetitive rounds.
--   cipherRounds      :: cipher -> Int
--   -- | Get the key schedule used for ciphering
--   cipherKeySchedule :: cipher -> ByteString
