module Set02
       ( execAll
       , challenge1, challenge2, challenge3, challenge4
       , challenge5, challenge6, challenge7, challenge8
       ) where

import AppCommon
import Bytes
import Padding

execAll :: IO ()
execAll = do
  challenge1
  -- challenge2
  -- challenge3
  -- challenge4
  -- challenge5
  -- challenge6
  -- challenge7
  -- challenge8

challenge1 :: IO ()
challenge1 = do
  plaintext  <- Bytes.fromString <$> readFile' 2 1 "plaintext"
  paddedtext <- Bytes.fromString <$> readFile' 2 1 "plaintext_padded"
  let solution = pad (PKCS7 20) plaintext
  testPrint 2 1 $ solution == paddedtext

challenge2 :: IO ()
challenge2 = do
  putStr ""
  testPrint 2 2 False

challenge3 :: IO ()
challenge3 = do
  putStr ""
  testPrint 2 3 False

challenge4 :: IO ()
challenge4 = do
  putStr ""
  testPrint 2 4 False

challenge5 :: IO ()
challenge5 = do
  putStr ""
  testPrint 2 5 False

challenge6 :: IO ()
challenge6 = do
  putStr ""
  testPrint 2 6 False

challenge7 :: IO ()
challenge7 = do
  putStr ""
  testPrint 2 7 False

challenge8 :: IO ()
challenge8 = do
  putStr ""
  testPrint 2 8 False
