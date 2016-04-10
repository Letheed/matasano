{- | Advanced Encryption Standard ciphers.

This AES implementation is only meant to be an implementation excercise.
Do not use it for actual cryptographical purposes.
-}

module AES
       ( -- * AES class
         AES
         -- * AES implementations
       , AES128, AES192, AES256
       ) where

import Bytes
import Cipher
import Data.Bits
import Data.List
import Data.List.Split
import Data.Word
import qualified Data.Array.Unboxed as UA

-- | AES cipher class.
class (BlockCipher cipher, ProductCipher cipher) => AES cipher

-- | AES symmetric block cipher with 128 bit key.
newtype AES128 = AES128 KeySchedule

instance AES AES128

instance Cipher AES128 where
  cipherInit key
    | keySize == 16 = Just $ AES128 keySchedule
    | otherwise     = Nothing
    where keySize     = length key
          keySchedule = scheduleKeys (11*16) key
  cipherKeySize     = const 16
  cipherName        = const "AES-128"

instance BlockCipher AES128 where
  blockSize     = const 16
  cipherBlock   = aesCipher
  decipherBlock = aesDecipher

instance ProductCipher AES128 where
  cipherRounds                           = const 10
  cipherKeySchedule (AES128 keySchedule) = keySchedule


-- | AES symmetric block cipher with 192 bit key.
newtype AES192 = AES192 KeySchedule

instance AES AES192

instance Cipher AES192 where
  cipherInit key
    | keySize == 24 = Just $ AES192 keySchedule
    | otherwise     = Nothing
    where keySize     = length key
          keySchedule = scheduleKeys (13*16) key
  cipherKeySize     = const 24
  cipherName        = const "AES-192"

instance BlockCipher AES192 where
  blockSize     = const 16
  cipherBlock   = aesCipher
  decipherBlock = aesDecipher

instance ProductCipher AES192 where
  cipherRounds                           = const 12
  cipherKeySchedule (AES192 keySchedule) = keySchedule


-- | AES symmetric block cipher with 256 bit key.
newtype AES256 = AES256 KeySchedule

instance AES AES256

instance Cipher AES256 where
  cipherInit key
    | keySize == 32 = Just $ AES256 keySchedule
    | otherwise     = Nothing
    where keySize     = length key
          keySchedule = scheduleKeys (15*16) key
  cipherKeySize     = const 32
  cipherName        = const "AES-256"

instance BlockCipher AES256 where
  blockSize     = const 16
  cipherBlock   = aesCipher
  decipherBlock = aesDecipher

instance ProductCipher AES256 where
  cipherRounds                           = const 14
  cipherKeySchedule (AES256 keySchedule) = keySchedule


-- | Multiplication in GF(2⁸).
gmul :: Byte -> Byte -> Byte
gmul = go (8::Int) 0
  where go n p a b
          | n == 0    = p
          | otherwise = go (n-1) p' a' b'
          where p' = if b .&. 1 /= 0 then p `xor` a else p
                a' = let a'' = a `shiftL` 1 in (if a .&. 0x80 /= 0 then a'' `xor` 0x1b else a'')
                b' = b `shiftR` 1

-- | Yield the exponentiation of 2 to some 8-bit value in GF(2⁸).
rcon :: Word8 -> Byte
rcon = (rconVec UA.!)

-- | Table of the exponentiation of 2 to the value of the index in GF(2⁸).
rconVec :: UA.Array Word8 Byte
rconVec = UA.listArray (0, 255) . take 256 . iterate nextRcon $ 0x8d
  where nextRcon r = (r `shiftL` 1) `xor` (0x1b .&. negate (r `shiftR` 7))

-- | Apply the Rijndael S-Box to a byte.
substitute :: Byte -> Byte
substitute = (sBoxVec UA.!)

-- | Apply the inverse Rijndael S-Box to a byte.
invSubstitute :: Byte -> Byte
invSubstitute = (invSBoxVec UA.!)

-- | Table of the Rijndael S-Box values.
sBoxVec :: UA.Array Byte Byte
sBoxVec = UA.listArray (0, 255) sBoxList

-- | Table of the inverse Rijndael S-Box values.
invSBoxVec :: UA.Array Byte Byte
invSBoxVec = UA.array (0, 255) [(substitute i, i) | i <- [0..255]]

-- | List of the Rijndael S-Box values.
sBoxList :: [Byte]
sBoxList = [
  0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
  0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
  0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
  0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
  0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
  0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
  0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
  0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
  0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
  0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
  0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
  0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
  0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
  0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
  0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
  0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16]

-- | Generate the requested bytelength of subkeys needed for AES block ciphering.
--
-- Produce an error if the key is not one of the standard keylengths (128, 192 or 256 bits).
scheduleKeys :: Int -> Key -> KeySchedule
scheduleKeys b key = take b expandedKey
  where n = length key
        expandedKey = key ++ expand 1 key (drop (n-4) key)
        expand i prev t
          | n `elem` [16, 24] = ts ++ expand (i+1) ts (drop (n-4) ts)
          | n == 32           = ts' ++ expand (i+1) ts' (drop 12 ts1)
          | otherwise         = error $ "scheduleKeys (AES): illegal key size (" ++ show (n*8) ++ " bits)"
          where ts  = zipWith xor prev (scheduleCore i t ++ ts)
                ts' = ts0 ++ ts1
                ts0 = zipWith xor ps0  (scheduleCore i t ++ ts0)
                ts1 = zipWith xor ps1  (t' ++ ts1)
                  where t' = map substitute . drop 12 $ ts0
                (ps0, ps1) = splitAt 16 prev

-- | Key schedule core operation.
-- Takes the iteration number for the rcon operation and a list of four bytes.
--
-- Produce an error if the list length is not four.
scheduleCore :: Word8 -> ByteString -> ByteString
scheduleCore i [b0, b1, b2, b3] = b `xor` rcon i : rest
  where (b:rest) = map substitute [b1, b2, b3, b0]
scheduleCore _ _                = error "scheduleCore: expected four bytes"

-- | Add round key step. ((+) <=> xor in GF(2⁸))
addRoundKey :: Key -> ByteString -> ByteString
addRoundKey = zipWith xor

-- | Substitution step (using the S-Box).
subBytes :: ByteString -> ByteString
subBytes = map substitute

-- | Inverted substitution step (using the inverted S-Box).
invSubBytes :: ByteString -> ByteString
invSubBytes = map invSubstitute

-- | Shift rows step.
shiftRows :: ByteString -> ByteString
shiftRows [b00, b10, b20, b30, b01, b11, b21, b31, b02, b12, b22, b32, b03, b13, b23, b33]
        = [b00, b11, b22, b33, b01, b12, b23, b30, b02, b13, b20, b31, b03, b10, b21, b32]
shiftRows _ = error "shiftRows: blocksize is not 128 bits"

-- | Inverted shift rows step.
invShiftRows :: ByteString -> ByteString
invShiftRows [b00, b11, b22, b33, b01, b12, b23, b30, b02, b13, b20, b31, b03, b10, b21, b32]
           = [b00, b10, b20, b30, b01, b11, b21, b31, b02, b12, b22, b32, b03, b13, b23, b33]
invShiftRows _ = error "shiftRows: blocksize is not 128 bits"

-- | Mix columns step.
mixColumns :: ByteString -> ByteString
mixColumns = concatMap mixColumn . chunksOf 4
  where mixColumn col@[b0, b1, b2, b3] = map (foldl1' xor) matrix
          where [a0, a1, a2, a3] = map f col
                  where f b = let b' = b `shiftL` 1  in (if b .&. 0x80 /= 0 then b' `xor` 0x1b else b')
                matrix = [[a0, a1, b1, b2, b3]
                         ,[a1, a2, b0, b2, b3]
                         ,[a2, a3, b0, b1, b3]
                         ,[a3, a0, b0, b1, b2]]
        mixColumn _ = error "mixColumn: blocksize is not 128 bits"

-- | Inverted mix columns step.
invMixColumns :: ByteString -> ByteString
invMixColumns = concatMap invMixColumn . chunksOf 4
  where invMixColumn col = map (foldl1' xor . zipWith gmul col) matrix
          where matrix = [[14, 11, 13, 9]
                         ,[9, 14, 11, 13]
                         ,[13, 9, 14, 11]
                         ,[11, 13, 9, 14]]

-- | Cipher a plaintext block using an AES cipher.
--
-- Produce an error if the plaintext's size doesn't match the cipher's blocksize.
aesCipher :: (AES cipher) => cipher -> ByteString -> ByteString
aesCipher = aesCore "aesCipher" foldl' fRound fRound'
  where fRound  subkey = addRoundKey subkey . mixColumns . shiftRows . subBytes
        fRound' subkey = addRoundKey subkey . shiftRows . subBytes

-- | Decipher a ciphertext block using an AES cipher.
--
-- Produce an error if the ciphertext's size doesn't match the cipher's blocksize.
aesDecipher :: (AES cipher) => cipher -> ByteString -> ByteString
aesDecipher = aesCore "aesDecipher" foldr' fRound fRound'
  where  fRound  subkey = invSubBytes . invShiftRows . invMixColumns . addRoundKey subkey
         fRound' subkey = invSubBytes . invShiftRows . addRoundKey subkey
         foldr' f = foldr (flip f)

-- | Core routine for the AES ciphering/deciphering process.
aesCore name fold fRound fRound' cipher block
  | length block == 16 = fold doRound block (zip roundFunctions subkeys)
  | otherwise          = error $ name ++ ": block size is not 128 bits"
  where nRounds                   = cipherRounds cipher
        keySchedule               = cipherKeySchedule cipher
        subkeys                   = chunksOf 16 keySchedule
        roundFunctions            = zipWith xor : replicate (nRounds-1) fRound ++ [fRound']
        doRound state (f, subkey) = f subkey state
