-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- Create, control and manage LXC containers through Haskell API.
-- You can get more info about LXC at <https://help.ubuntu.com/lts/serverguide/lxc.html>.
--
-----------------------------------------------------------------------------
module System.LXC (
  module System.LXC.Container,
  module System.LXC.AttachOptions
) where

import System.LXC.Container
import System.LXC.AttachOptions
