module Set01
       ( execAll
       , challenge1, challenge2, challenge3, challenge4
       , challenge5, challenge6, challenge7, challenge8
       ) where

import AppCommon
import LibCommon
import Base64
import Bytes
import Xor

import Data.List
import Data.Maybe
import Data.Ord

execAll :: IO ()
execAll = do
  challenge1
  challenge2
  challenge3
  challenge4
  challenge5
  challenge6
  -- challenge7
  -- challenge8

challenge1 :: IO ()
challenge1 = do
  b64    <- readFile' 1 1 "base64"
  hexStr <- readFile' 1 1 "hexadecimal"
  let result = Base64.fromBytes . Bytes.parseHex $ hexStr
  testPrint 1 1 $ b64 == result

challenge2 :: IO ()
challenge2 = do
  [hex1, hex2, result] <- map parseHex . lines <$> readFile' 1 2 "data"
  let xorResult = hex1 `xorEqLen` hex2
  testPrint 1 2 $ result == xorResult

challenge3 :: IO ()
challenge3 = do
  cipher    <- parseHex <$> readFile' 1 3 "ciphertext"
  plaintext <- readFile' 1 3 "plaintext"
  let keys     = [[b] | b <- [0..255::Byte]]
  let solution = xorDecryptWithKeys keys " etaoin" cipher
  let success  = maybeTest ((plaintext ==) . Bytes.toString . thd3) solution
  testPrint 1 3 success

challenge4 :: IO ()
challenge4 = do
  ciphers   <- map parseHex . lines <$> readFile' 1 4 "ciphertexts"
  plaintext <- readFile' 1 4 "plaintext"
  let keys      = [[b] | b <- [0..255::Byte]]
  let solutions = mapMaybe (xorDecryptWithKeys keys " etaoin") ciphers
  let solution  = lastMaybe . sortBy (comparing snd3) $ solutions
  let success   = maybeTest ((plaintext ==) . Bytes.toString . thd3) solution
  testPrint 1 4 success

challenge5 :: IO ()
challenge5 = do
  plaintext <- Bytes.fromString <$> readFile' 1 5 "plaintext"
  key       <- Bytes.fromString <$> readFile' 1 5 "key"
  cipher    <- parseHex <$> readFile' 1 5 "ciphertext"
  testPrint 1 5 $ cipher == plaintext `xorCycle` key

challenge6 :: IO ()
challenge6 = do
  cipher    <- Base64.toBytes . concat . lines <$> readFile' 1 6 "cipher_base64"
  plaintext <- readFile' 1 6 "plaintext"
  let solution = xorCycle cipher <$> xorDecrypt " etaoin" cipher
  let success  = maybeTest ((plaintext ==) . Bytes.toString) solution
  testPrint 1 6 success

challenge7 :: IO ()
challenge7 = do
  putStrLn ""
  testPrint 1 7 False

challenge8 :: IO ()
challenge8 = do
  putStrLn ""
  testPrint 1 8 False
