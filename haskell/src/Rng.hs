module Rng (randoms) where

import Data.Word
import Data.Bits

randoms :: Integer -> [Int]
randoms seed = map toInt15 (rng32 (fromIntegral seed))

rng32 :: Word32 -> [Word32]
rng32 seed = seed `seq` (seed : rng32 (next seed))

next :: Word32 -> Word32
next seed = 1103515245 * seed + 12345

toInt15 :: Word32 -> Int
toInt15 n = fromIntegral (shiftR n 16 .&. 32767)
