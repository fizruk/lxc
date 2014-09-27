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
