module Set02
  ( execAll
  , challenge9, challenge10, challenge11, challenge12
  , challenge13, challenge14, challenge15, challenge16
  ) where

import AppCommon
import AES
import Base64
import Bytes
import Cipher
import Padding
import Random

import Data.Maybe
import System.Random

execAll :: IO ()
execAll = do
  challenge9
  challenge10
  challenge11
  challenge12
  -- challenge13
  -- challenge14
  -- challenge15
  -- challenge16

challenge9 :: IO ()
challenge9 = do
  plaintext  <- Bytes.fromString <$> readFile' 2 9 "plaintext"
  paddedtext <- Bytes.fromString <$> readFile' 2 9 "plaintext_padded"
  let solution = pad (PKCS7 20) plaintext
  testPrint 2 9 $ solution == paddedtext

challenge10 :: IO ()
challenge10 = do
  key       <- Bytes.fromString <$> readFile' 2 10 "key"
  cipher    <- Base64.toBytes . concat . lines <$> readFile' 2 10 "cipher_base64"
  plaintext <- readFile' 2 10 "plaintext"
  let aes128   = fromJust $ cipherInit key :: AES128
  let iv       = ivNull :: IV AES128
  let solution = Bytes.toString . depad (PKCS7 (blockSize aes128)) . cbcDecipher aes128 iv $ cipher
  testPrint 2 10 $ solution == plaintext

challenge11 :: IO ()
challenge11 = do
  gen <- newStdGen
  -- 64 bytes (4 blocks) garanty us at least 2 identical blocks, given that the
  -- outermost blocks each contain 5 to 10 random bytes.
  let (ciphertext, rdMode) = randomCipher gen (replicate 64 0x00)
  let detectedMode = if ecbProbe 16 ciphertext /= 0 then ECB else CBC
  testPrint 2 11 $ rdMode == detectedMode

challenge12 :: IO ()
challenge12 = do
  gen    <- newStdGen
  suffix <- Base64.toBytes . concat . lines <$> readFile' 2 12 "suffix_base64"
  let rdKey     = take 16 . randoms $ gen :: ByteString
  let aes128    = fromJust $ cipherInit rdKey :: AES128
  let cipher    = ecbCipher aes128 . pad (PKCS7 (blockSize aes128)) . (++ suffix)
  let blockSize = ecbBlockSize cipher
  testPrint 2 12 False

challenge13 :: IO ()
challenge13 = do
  putStr ""
  testPrint 2 13 False

challenge14 :: IO ()
challenge14 = do
  putStr ""
  testPrint 2 14 False

challenge15 :: IO ()
challenge15 = do
  putStr ""
  testPrint 2 15 False

challenge16 :: IO ()
challenge16 = do
  putStr ""
  testPrint 2 16 False
