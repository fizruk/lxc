-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC.Container
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- This module provides a set of functions to create, control and manage
-- LXC containers. You can get more info about LXC at <https://help.ubuntu.com/lts/serverguide/lxc.html>.
--
-- Normally you should import @System.LXC@ module only.
--
-----------------------------------------------------------------------------
module System.LXC.Container (
  -- * Data types
  Container(..),
  Snapshot(..),
  BDevSpecs(..),
  ContainerState(..),
  parseState, printState,
  mkContainer,
  -- * Flags
  CloneOption(..),
  CreateOption(..),
  cloneFlag, createFlag,
  -- * Container methods
  -- ** Query container state.
  isDefined,
  isRunning,
  state,
  initPID,
  -- ** Container config
  configFileName,
  getConfigPath,
  setConfigPath,
  loadConfig,
  saveConfig,
  getKeys,
  setConfigItem,
  getConfigItem,
  getRunningConfigItem,
  clearConfig,
  clearConfigItem,
  -- ** Freeze/unfreeze
  freeze,
  unfreeze,
  -- ** Control container state
  start,
  stop,
  reboot,
  shutdown,
  wait,
  -- ** Manage containers
  create,
  clone,
  rename,
  destroy,
  -- ** Console
  consoleGetFD,
  console,
  -- ** Attach to container
  attach,
  attachRunWait,
  -- ** Snapshots
  snapshot,
  snapshotList,
  snapshotRestore,
  -- ** Misc
  wantDaemonize,
  wantCloseAllFDs,
  getInterfaces,
  getIPs,
  getCGroupItem,
  setCGroupItem,
  mayControl,
  addDeviceNode,
  removeDeviceNode,
  getRef,
  dropRef,
  -- * Global LXC functions
  -- ** List containers
  listDefinedContainers,
  listActiveContainers,
  listAllContainers,
  -- ** Misc
  getWaitStates,
  getGlobalConfigItem,
  getVersion,
  logClose,
) where

import System.LXC.Internal.Container
