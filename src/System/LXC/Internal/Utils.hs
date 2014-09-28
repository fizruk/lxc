-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC.Internal.Utils
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- Internal module with utility functions.
-- Normally you should import @System.LXC@ module only.
--
-----------------------------------------------------------------------------
module System.LXC.Internal.Utils where

import Data.Bits
import Data.List

-- | Collect flags in a single integer value.
mkFlags :: (Num b, Bits b) => (a -> b) -> [a] -> b
mkFlags f = foldl' (.|.) 0 . map f
