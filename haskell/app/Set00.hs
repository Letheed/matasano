-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Set00
  ( execAll
  , challenge1, challenge2, challenge3, challenge4
  , challenge5, challenge6, challenge7, challenge8
  ) where

import AppCommon
import LibCommon

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
  putStr ""
  testPrint 0 1 False

challenge2 :: IO ()
challenge2 = do
  putStr ""
  testPrint 0 2 False

challenge3 :: IO ()
challenge3 = do
  putStr ""
  testPrint 0 3 False

challenge4 :: IO ()
challenge4 = do
  putStr ""
  testPrint 0 4 False

challenge5 :: IO ()
challenge5 = do
  putStr ""
  testPrint 0 5 False

challenge6 :: IO ()
challenge6 = do
  putStr ""
  testPrint 0 6 False

challenge7 :: IO ()
challenge7 = do
  putStr ""
  testPrint 0 7 False

challenge8 :: IO ()
challenge8 = do
  putStr ""
  testPrint 0 8 False
