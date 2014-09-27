module System.LXC.Internal.Utils where

import Data.Bits
import Data.List

-- | Collect flags in a single integer value.
mkFlags :: (Num b, Bits b) => (a -> b) -> [a] -> b
mkFlags f = foldl' (.|.) 0 . map f
