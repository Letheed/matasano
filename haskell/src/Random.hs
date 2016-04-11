{- | Random functions. -}

module Random
       ( randomCipher
       ) where 

import AES
import Bytes
import Cipher
import Padding

import Data.Maybe
import System.Random

-- | Take a `ByteString` and cipher it with a random key, using randomly ECB or CBC.
-- CBC uses a random IV. A random amount (in [5-10]) of random bytes is prepended
-- to the plaintext. A random amount (in [5-10]) of random bytes is appended to the
-- plaintext. The plaintext is padded.
--
-- Return the ciphertext and the operation mode used so that the results of the
-- mode detection functions can be verified.
randomCipher :: (RandomGen g) => g -> ByteString -> (ByteString, OperationMode)
randomCipher gen plaintext = (rdModeCipher . pad (PKCS7 (blockSize cipher)) $ plaintext', mode)
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
          | ecbMode   = (ECB, ecbCipher cipher)
          | otherwise = (CBC, cbcCipher cipher rdIV)
          where rdIV = fromJust $ ivInit rdBs
