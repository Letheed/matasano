--------------------------------------------------------------------------------
-- | Hamming distance computations and operators.
--
--------------------------------------------------------------------------------

module Hamming
  ( -- * Hamming operators
    (^+), (.^+.)
  ) where

import LibCommon
import Bytes

import Data.Bits
import Data.List

-- | Computes the hamming distance of two `ByteString`s.
--
-- The `ByteString`s must have the same length.
-- Produces an error otherwise.
(^+) :: ByteString -> ByteString -> Int
(^+) bs0 bs1 = foldl' (\hamDist (b0, b1) -> hamDist + b0 .^+. b1) 0 $ zipEqLen bs0 bs1

-- | Computes the hamming distance of two `Byte`s.
(.^+.) :: Byte -> Byte -> Int
(.^+.) b0 b1 = hammingByte' (b0 .^. b1) 0
  where hammingByte' b dist
          | b == 0         = dist
          | b .&. 0x1 == 0 = hammingByte' b' (dist + 1)
          | otherwise      = hammingByte' b' dist
          where b' = shift b (-1)
