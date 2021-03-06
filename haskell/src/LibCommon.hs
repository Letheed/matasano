-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

--------------------------------------------------------------------------------
-- | Generic utility functions common to the library.
--
--------------------------------------------------------------------------------

module LibCommon
  ( (.^.)
  , fst3, snd3, thd3, fstThd3
  , lastMaybe, mapAllJust, pairOff, slidingPairs, zipEqLen
  ) where

import Data.Bits

-- | Infix operator for the `xor` function.
(.^.) :: Bits a => a -> a -> a
(.^.) = xor

-- | Returns the first element of a triplet.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Returns the second element of a triplet.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Returns the third element of a triplet.
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Returns the first and the third element of a triplet.
fstThd3 :: (a, b, c) -> (a, c)
fstThd3 (a, _, c) = (a, c)

-- | Extracts the last element of a list.
--
-- Returns `Nothing` if the list is empty.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe a  = Just (last a)

-- | Maps the elements in a list of `Maybe`s if all elements are `Just`s.
--
-- Returns `Nothing` otherwise.
mapAllJust :: (a -> b) -> [Maybe a] -> Maybe [b]
mapAllJust f = go
  where go []     = Just []
        go (m:ms) = case m of
          Just a  -> (f a :) <$> go ms
          Nothing -> Nothing

-- | Pairs off successive values in a list.
--
-- The list must have a even number of elements.
-- Produces an error otherwise.
pairOff :: [a] -> [(a, a)]
pairOff (u:v:rest) = (u, v) : pairOff rest
pairOff []         = []
pairOff [_]        = error "odd list length"

-- | Creates a list of sliding pairs out of a list.
slidingPairs :: [a] -> [(a, a)]
slidingPairs (x0:x1:xs) = (x0, x1) : slidingPairs (x1:xs)
slidingPairs _            = []


-- | Zips together two lists of equal lengths.
--
-- Produces an error if the lengths differ.
zipEqLen :: [a] -> [b] -> [(a, b)]
zipEqLen []     []     = []
zipEqLen (a:as) (b:bs) = (a,b) : zipEqLen as bs
zipEqLen _      _      = error "zipEqLen: lists have different lengths"
