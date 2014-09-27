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
