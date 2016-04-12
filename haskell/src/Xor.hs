--------------------------------------------------------------------------------
-- | XOR and XOR-decryption operations.
--
-- NOTE: This module was written with ASCII characters and strings in mind.
--       The ASCII assumption allows for quick resolutions when decrypting.
--       UTF8 is possible but more costly, so I'm not bothering with it here.
--
--------------------------------------------------------------------------------

module Xor
  ( -- * XOR operations
    xorEqLen, xorCycle
    -- * XOR decryption
  , xorDecrypt, xorDecryptWithKeys
  ) where

import LibCommon
import Bytes
import Hamming

import Data.Array.Unboxed
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Ratio

type Stats = Array Byte Int

-- | XOR two `ByteString`s of equal lengths.
--
-- Produces an error if the lengths differ.
xorEqLen :: ByteString -> ByteString -> ByteString
xorEqLen (a:as) (b:bs) = a .^. b : xorEqLen as bs
xorEqLen []     []     = []
xorEqLen _      _      = error "xorEqLen: ByteStrings have different lengths"

-- | XOR a `ByteString` (typically a text or cipher) with another `ByteString`,
-- repeating (cycling) the second one (typically a key).
xorCycle :: ByteString -> ByteString -> ByteString
xorCycle cipher = zipWith (.^.) cipher . cycle

-- | Tries to decrypt a repeating-key XOR cipher, given a set of candidate keys
-- and an ordered `String` of the most frequent ASCII characters expected.
--
-- Returns the key with the highest grade, as well as the grade (higher is better)
-- and the resulting plaintext, or `Nothing` if no key met the minimum criteria.
xorDecryptWithKeys :: [ByteString]  -- ^ Set of candidate keys.
                   -> String        -- ^ Ordered `String` of the most frequent ASCII characters expected.
                   -> ByteString    -- ^ Ciphertext.
                   -> Maybe (ByteString, Int, ByteString) -- ^ Key, grade and plaintext.
xorDecryptWithKeys keys topChars cipher = listToMaybe . sortBy (flip $ comparing snd3) $ solutions
  where topLen    = length topChars
        topBytes  = fromString topChars
        solutions = [(key, gradeKey, plaintext) | key <- keys
                    , let plaintext = cipher `xorCycle` key
                    , let stats     = asciiStats plaintext
                    , let histogram = take topLen . genHistogram $ fromJust stats
                    , let gradeKey  = grade histogram
                    , isJust stats, hasMatches topBytes histogram]
          where genHistogram stats  = sortBy (flip (comparing snd)) [(b, stats ! b) | b <- [0..byteMax]]
                hasMatches byteList = (==) minLen . length . take minLen . filter ((`elem` byteList) . fst)
                  where minLen = 2
                grade = foldl' (\acc (i, (b, _)) -> acc + gradeByte i b) 0 . zip [0..]
                  where gradeByte i b = case b `elemIndex` topBytes of
                          Just j  -> topLen - abs (i - j)
                          Nothing -> -topLen

-- | Creates a table of the number of occurences of the acceptable ASCII characters.
--
-- Returns `Nothing` upon encountering a non-ASCII or unusual character.
asciiStats :: ByteString -> Maybe Stats
asciiStats = go $ array (0,byteMax) [(i, 0) | i <- [0..byteMax]]
  where go stats []        = Just stats
        go stats (b:bs)
          | isBadAscii ! b = Nothing
          | otherwise      = go stats' bs
          where stats' = stats // [(b, 1 + stats ! b)]

-- | Highest accepted ASCII value.
asciiMax :: Int
asciiMax = ord '~'

-- | Highest accepted byte value.
byteMax :: Byte
byteMax = fromIntegral asciiMax

-- | Truth table of non-acceptable `Byte`s.
isBadAscii :: Array Byte Bool
isBadAscii = listArray (0, 255) [isBadAscii' b | b <- [0..255]]
  where isBadAscii' b = b > asciiMax
          || (b < ord ' ' && (b /= ord '\t' && b /= ord '\n' && b /= ord '\r' && b /= ord '\ESC'))

-- | Tries to decrypt a repeating-key XOR cipher, given an ordered `String` of
-- the most frequent ASCII characters expected.
--
-- Returns the key if all the blocks were decrypted successfully.
-- Returns `Nothing` otherwise.
xorDecrypt :: String -> ByteString -> Maybe ByteString
xorDecrypt topChars cipher = mapAllJust (head . fst3) solutions
  where keySize   = keysize cipher
        keys      = [[b] | b <- [0..255::Byte]]
        blocks    = transpose . chunksOf keySize $ cipher
        solutions = map (xorDecryptWithKeys keys topChars) blocks

-- | Finds the most likely keysize for a repeating-key XOR cipher.
-- Selects the keysize with the smallest normalized mean hamming distance
-- over the cipher.
keysize :: ByteString -> Int
keysize cipher = head . sortOn weightedMeanHammingDistance $ [2..maxKeySize]
  where cipherLen  = length cipher
        maxKeySize = min 40 (cipherLen `quot` 2)
        weightedMeanHammingDistance keySize = htot % (keySize * (nFullChunks - 1))
          where htot = sum . map (uncurry (^+)) . slidingPairs . take nFullChunks . chunksOf keySize $ cipher
                nFullChunks = cipherLen `quot` keySize
