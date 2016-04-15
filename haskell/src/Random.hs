-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

--------------------------------------------------------------------------------
-- | Functions introducing random behaviors.
--
--------------------------------------------------------------------------------

module Random
  ( randomCipher
  ) where

import AES
import Bytes
import Cipher
import Padding

import Data.Maybe
import System.Random

-- | Takes a `ByteString` and ciphers it with a random `Key`, using randomly
-- `ECB` or `CBC`.
--
-- `CBC` uses a random `IV`. A random amount (in [5-10]) of random bytes is
-- prepended to the plaintext. Similarly, a random amount (in [5-10])
-- of random bytes is appended to the plaintext. The plaintext is padded with
-- `PKCS7`.
--
-- Returns the ciphertext and the `OperationMode` used so that the correctness
-- of the mode detection functions can be verified.
randomCipher :: (RandomGen g) => g -> ByteString -> (ByteString, OperationMode)
randomCipher gen plaintext = (rdModeCipher plaintext', mode)
  where (ecbMode, gen1)  = random gen
        (nBefore, gen2)  = randomR (5, 10) gen1
        (nAfter,  gen3)  = randomR (5, 10) gen2
        neededBytes      = nBefore + nAfter + if ecbMode then 16 else 32
        randomBytes      = take neededBytes . randoms $ gen3
        (rdKey,  rest1)  = splitAt 16      randomBytes
        (rdPref, rest2)  = splitAt nBefore rest1
        (rdSuf,  rdBs)   = splitAt nAfter  rest2
        plaintext'       = rdPref ++ plaintext ++ rdSuf
        cipher           = fromJust $ cipherInit rdKey :: AES128
        (mode, rdModeCipher)
          | ecbMode   = (ECB, ecbCipher cipher PKCS7)
          | otherwise = (CBC, cbcCipher cipher rdIV PKCS7)
          where rdIV = fromJust $ ivInit rdBs
