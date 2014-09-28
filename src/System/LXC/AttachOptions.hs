-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC.AttachOptions
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- Options and structures to run commands inside LXC containers.
-- You can get more info about LXC at <https://help.ubuntu.com/lts/serverguide/lxc.html>.
--
-- Normally you should import @System.LXC@ module only.
--
-----------------------------------------------------------------------------
module System.LXC.AttachOptions (
  -- * Attach options
  AttachOptions(..),
  defaultAttachOptions,
  -- * Attach command
  AttachCommand(..),
  -- * Attach @exec@ functions
  AttachExecFn(..),
  attachRunCommand,
  attachRunShell,
  -- * Flags and environment policies
  AttachEnvPolicy(..),
  AttachFlag(..),
  fromAttachEnvPolicy,
  fromAttachFlag,
) where

import System.LXC.Internal.AttachOptions
