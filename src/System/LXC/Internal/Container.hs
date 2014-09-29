{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC.Internal.Container
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- Internal module to provide a set of functions to create,
-- control and manage LXC containers.
-- Normally you should import @System.LXC@ module only.
--
-----------------------------------------------------------------------------
module System.LXC.Internal.Container where

import Bindings.LXC.AttachOptions
import Bindings.LXC.Container
import Bindings.LXC.Sys.Types

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import Data.Maybe
import Data.Word

import Foreign
import Foreign.C

import System.LXC.Internal.AttachOptions
import System.LXC.Internal.Utils

import System.Exit
import System.Posix.Types (ProcessID, Fd)

type ContainerCreateFn = Ptr C'lxc_container -> CString -> CString -> Ptr C'bdev_specs -> CInt -> Ptr CString -> IO CBool
foreign import ccall "dynamic"
  mkCreateFn :: FunPtr ContainerCreateFn -> ContainerCreateFn

type ContainerCloneFn = Ptr C'lxc_container -> CString -> CString -> CInt -> CString -> CString -> C'uint64_t -> Ptr CString -> IO (Ptr C'lxc_container)
foreign import ccall "dynamic"
  mkCloneFn :: FunPtr ContainerCloneFn -> ContainerCloneFn

type ContainerBoolFn = Ptr C'lxc_container -> IO CBool
foreign import ccall "dynamic"
  mkBoolFn :: FunPtr ContainerBoolFn -> ContainerBoolFn

type ContainerStringFn = Ptr C'lxc_container -> IO CString
foreign import ccall "dynamic"
  mkStringFn :: FunPtr ContainerStringFn -> ContainerStringFn

type ContainerProcessIDFn = Ptr C'lxc_container -> IO C'pid_t
foreign import ccall "dynamic"
  mkProcessIDFn :: FunPtr ContainerProcessIDFn -> ContainerProcessIDFn

type ContainerStringBoolFn = Ptr C'lxc_container -> CString -> IO CBool
foreign import ccall "dynamic"
  mkStringBoolFn :: FunPtr ContainerStringBoolFn -> ContainerStringBoolFn

type ContainerBoolBoolFn = Ptr C'lxc_container -> CBool -> IO CBool
foreign import ccall "dynamic"
  mkBoolBoolFn :: FunPtr ContainerBoolBoolFn -> ContainerBoolBoolFn

type ContainerStartFn = Ptr C'lxc_container -> CInt -> Ptr CString -> IO CBool
foreign import ccall "dynamic"
  mkStartFn :: FunPtr ContainerStartFn -> ContainerStartFn

type ContainerShutdownFn = Ptr C'lxc_container -> CInt -> IO CBool
foreign import ccall "dynamic"
  mkShutdownFn :: FunPtr ContainerShutdownFn -> ContainerShutdownFn

type ContainerClearConfigFn = Ptr C'lxc_container -> IO ()
foreign import ccall "dynamic"
  mkClearConfigFn :: FunPtr ContainerClearConfigFn -> ContainerClearConfigFn

type ContainerGetRunningConfigItemFn = Ptr C'lxc_container -> CString -> IO CString
foreign import ccall "dynamic"
  mkGetRunningConfigItemFn :: FunPtr ContainerGetRunningConfigItemFn -> ContainerGetRunningConfigItemFn

type ContainerGetItemFn = Ptr C'lxc_container -> CString -> CString -> CInt -> IO CInt
foreign import ccall "dynamic"
  mkGetItemFn :: FunPtr ContainerGetItemFn -> ContainerGetItemFn

type ContainerSetItemFn = Ptr C'lxc_container -> CString -> CString -> IO CBool
foreign import ccall "dynamic"
  mkSetItemFn :: FunPtr ContainerSetItemFn -> ContainerSetItemFn

type ContainerGetInterfacesFn = Ptr C'lxc_container -> IO (Ptr CString)
foreign import ccall "dynamic"
  mkGetInterfacesFn :: FunPtr ContainerGetInterfacesFn -> ContainerGetInterfacesFn

type ContainerGetIPsFn = Ptr C'lxc_container -> CString -> CString -> CInt -> IO (Ptr CString)
foreign import ccall "dynamic"
  mkGetIPsFn :: FunPtr ContainerGetIPsFn -> ContainerGetIPsFn

type ContainerWaitFn = Ptr C'lxc_container -> CString -> CInt -> IO CBool
foreign import ccall "dynamic"
  mkWaitFn :: FunPtr ContainerWaitFn -> ContainerWaitFn

type ContainerSnapshotFn = Ptr C'lxc_container -> CString -> IO CInt
foreign import ccall "dynamic"
  mkSnapshotFn :: FunPtr ContainerSnapshotFn -> ContainerSnapshotFn

type ContainerSnapshotListFn = Ptr C'lxc_container -> Ptr (Ptr C'lxc_snapshot) -> IO CInt
foreign import ccall "dynamic"
  mkSnapshotListFn :: FunPtr ContainerSnapshotListFn -> ContainerSnapshotListFn

type ContainerConsoleGetFDFn = Ptr C'lxc_container -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "dynamic"
  mkConsoleGetFDFn :: FunPtr ContainerConsoleGetFDFn -> ContainerConsoleGetFDFn

type ContainerConsoleFn = Ptr C'lxc_container -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "dynamic"
  mkConsoleFn :: FunPtr ContainerConsoleFn -> ContainerConsoleFn

type ContainerAttachFn = Ptr C'lxc_container -> C_lxc_attach_exec_t -> Ptr () -> Ptr C'lxc_attach_options_t -> Ptr C'pid_t -> IO CInt
foreign import ccall "dynamic"
  mkAttachFn :: FunPtr ContainerAttachFn -> ContainerAttachFn

type ContainerAttachRunWaitFn = Ptr C'lxc_container -> Ptr C'lxc_attach_options_t -> CString -> Ptr CString -> IO CInt
foreign import ccall "dynamic"
  mkAttachRunWaitFn :: FunPtr ContainerAttachRunWaitFn -> ContainerAttachRunWaitFn

type SnapshotFreeFn = Ptr C'lxc_snapshot -> IO ()
foreign import ccall "dynamic"
  mkFreeFn :: FunPtr SnapshotFreeFn -> SnapshotFreeFn

-- | LXC container-related computations.
-- @'LXC' ~ 'ReaderT' ('String', 'Ptr' 'C'lxc_container') 'IO'@.
--
-- Run @'LXC' a@ computations using 'withContainer'.
newtype LXC a = LXC { runLXC :: ReaderT (String, Ptr C'lxc_container) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (String, Ptr C'lxc_container), MonadIO)

lxc :: (Ptr C'lxc_container -> IO a) -> LXC a
lxc f = LXC . ReaderT $ \(_, p) -> f p

-- | Run @'LXC' a@ computation for a given 'Container'.
--
-- * for the whole computation a single @lxc_container@ structure
-- will be allocated; it will be automatically freed at the end of
-- computation.
withContainer :: MonadIO m => Container -> LXC a -> m a
withContainer c m = liftIO $ do
  withC'lxc_container c $ \cc -> do
    runReaderT (runLXC m) $ (containerName c, cc)

-- | LXC error structure.
data LXCError = LXCError
  { lxcErrorString  :: String   -- ^ Error message.
  , lxcErrorNum     :: Int      -- ^ Error number.
  }
  deriving (Show)

-- | Pretty print LXC error message.
prettyLXCError :: LXCError -> String
prettyLXCError (LXCError msg num) = "Error " ++ show num ++ ": " ++ msg

-- | Options for 'clone' operation.
data CloneOption
  = CloneKeepName        -- ^ Do not edit the rootfs to change the hostname.
  | CloneKeepMacAddr     -- ^ Do not change the MAC address on network interfaces.
  | CloneSnapshot        -- ^ Snapshot the original filesystem(s).
  | CloneKeepBDevType    -- ^ Use the same bdev type.
  | CloneMaybeSnapshot   -- ^ Snapshot only if bdev supports it, else copy.
  | CloneMaxFlags        -- ^ Number of @LXC_CLONE_*@ flags.
  deriving (Eq, Ord)

-- | Options for 'create' operation.
data CreateOption
  = CreateQuiet          -- ^ Redirect @stdin@ to @\/dev\/zero@ and @stdout@ and @stderr@ to @\/dev\/null@.
  | CreateMaxFlags       -- ^ Number of @LXC_CREATE*@ flags.
  deriving (Eq, Ord)

-- | Turn 'CloneOption' into a bit flag.
cloneFlag :: Num a => CloneOption -> a
cloneFlag CloneKeepName       = c'LXC_CLONE_KEEPNAME
cloneFlag CloneKeepMacAddr    = c'LXC_CLONE_KEEPMACADDR
cloneFlag CloneSnapshot       = c'LXC_CLONE_SNAPSHOT
cloneFlag CloneKeepBDevType   = c'LXC_CLONE_KEEPBDEVTYPE
cloneFlag CloneMaybeSnapshot  = c'LXC_CLONE_MAYBE_SNAPSHOT
cloneFlag CloneMaxFlags       = c'LXC_CLONE_MAXFLAGS

-- | Turn 'CreateOption' into a bit flag.
createFlag :: Num a => CreateOption -> a
createFlag CreateQuiet    = c'LXC_CREATE_QUIET
createFlag CreateMaxFlags = c'LXC_CREATE_MAXFLAGS

-- | An LXC container snapshot.
data Snapshot = Snapshot
  { snapshotName            :: String         -- ^ Name of snapshot.
  , snapshotCommentPathname :: Maybe FilePath -- ^ Full path to snapshots comment file.
  , snapshotTimestamp       :: String         -- ^ Time snapshot was created.
  , snapshotLXCPath         :: FilePath       -- ^ Full path to @LXCPATH@ for snapshot.
  }
  deriving (Show)

-- | Container object.
data Container = Container
  { containerName       :: String         -- ^ Container name.
  , containerConfigPath :: Maybe String   -- ^ Container config path.
  }
  deriving (Show)

-- | Allocate a new @lxc_container@.
newC'lxc_container :: Container -> IO (Ptr C'lxc_container)
newC'lxc_container (Container name configPath) = do
  c <- withCString name $ \cname ->
        maybeWith withCString configPath $ \cconfigPath ->
          c'lxc_container_new cname cconfigPath
  when (c == nullPtr) $ error "failed to allocate new container"
  return c

peekC'lxc_container :: Ptr C'lxc_container -> IO (String -> Container)
peekC'lxc_container ptr = do
  configPath <- peek (p'lxc_container'config_path ptr) >>= maybePeek peekCString
  return $ \name -> Container name configPath

-- | Marshal 'Container' to @lxc_container@ using temporary storage.
withC'lxc_container :: Container -> (Ptr C'lxc_container -> IO a) -> IO a
withC'lxc_container c f = do
  cc <- newC'lxc_container c
  ret <- f cc
  dropRef cc
  return ret

-- | Container state.
data ContainerState
  = ContainerStopped            -- ^ Container is stopped.
  | ContainerStarting           -- ^ Container is starting.
  | ContainerRunning            -- ^ Container is running.
  | ContainerStopping           -- ^ Container is stopping.
  | ContainerAborting           -- ^ Container is aborting.
  | ContainerFreezing           -- ^ Container is freezing.
  | ContainerFrozen             -- ^ Container is frozen.
  | ContainerThawed             -- ^ Container is thawed.
  | ContainerOtherState String  -- ^ Container is in some other state.
  deriving (Eq, Show)

-- | Parse state as string representation.
parseState :: String -> ContainerState
parseState "STOPPED"  = ContainerStopped
parseState "STARTING" = ContainerStarting
parseState "RUNNING"  = ContainerRunning
parseState "STOPPING" = ContainerStopping
parseState "ABORTING" = ContainerAborting
parseState "FREEZING" = ContainerFreezing
parseState "FROZEN"   = ContainerFrozen
parseState "THAWED"   = ContainerThawed
parseState s          = ContainerOtherState s

-- | Get string representation of a state.
printState :: ContainerState -> String
printState ContainerStopped         = "STOPPED"
printState ContainerStarting        = "STARTING"
printState ContainerRunning         = "RUNNING"
printState ContainerStopping        = "STOPPING"
printState ContainerAborting        = "ABORTING"
printState ContainerFreezing        = "FREEZING"
printState ContainerFrozen          = "FROZEN"
printState ContainerThawed          = "THAWED"
printState (ContainerOtherState s)  = s

-- | Specifications for how to create a new backing store.
data BDevSpecs = BDevSpecs
  { bdevFSType                :: String         -- ^ Filesystem type.
  , bdevFSSize                :: Word64         -- ^ Filesystem size in bytes.
  , bdevZFSRootPath           :: FilePath       -- ^ ZFS root path.
  , bdevLVMVolumeGroupName    :: String         -- ^ LVM Volume Group name.
  , bdevLVMLogicalVolumeName  :: String         -- ^ LVM Logical Volume name.
  , bdevLVMThinPool           :: Maybe String   -- ^ LVM thin pool to use, if any.
  , bdevDirectory             :: FilePath       -- ^ Directory path.
  }
  deriving (Show)

-- | Marshal Haskell 'BDevSpecs' into C structure using temporary storage.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
withC'bdev_specs :: BDevSpecs -> (Ptr C'bdev_specs -> IO a) -> IO a
withC'bdev_specs specs f = do
  withCString (bdevFSType                        specs) $ \cFSType ->
    withCString (bdevZFSRootPath                 specs) $ \cZFSRootPath ->
      withCString (bdevLVMVolumeGroupName        specs) $ \cLVMVolumeGroupName ->
        withCString (bdevLVMLogicalVolumeName    specs) $ \cLVMLogicalVolumeName ->
          maybeWith withCString (bdevLVMThinPool specs) $ \cLVMThinPool ->
            withCString (bdevDirectory           specs) $ \cDirectory -> do
              let cspecs = C'bdev_specs
                              cFSType
                              (bdevFSSize specs)
                              (C'zfs_t cZFSRootPath)
                              (C'lvm_t
                                cLVMVolumeGroupName
                                cLVMLogicalVolumeName
                                cLVMThinPool)
                              cDirectory
              with cspecs f

type Field s a = Ptr s -> Ptr a

mkFn :: (FunPtr (Ptr s -> a) -> (Ptr s -> a)) -> Field s (FunPtr (Ptr s -> a)) -> Ptr s -> IO a
mkFn mk g s = do
  fn <- peek (g s)
  return $ mk fn s

boolFn :: Field C'lxc_container (FunPtr ContainerBoolFn) -> LXC Bool
boolFn g = lxc $ \c -> do
  fn <- mkFn mkBoolFn g c
  toBool <$> fn

stringBoolFn :: Field C'lxc_container (FunPtr ContainerStringBoolFn) -> Maybe String -> LXC Bool
stringBoolFn g s = lxc $ \c -> do
  fn <- mkFn mkStringBoolFn g c
  maybeWith withCString s $ \cs ->
    toBool <$> fn cs

boolBoolFn :: Field C'lxc_container (FunPtr ContainerBoolBoolFn) -> Bool -> LXC Bool
boolBoolFn g b = lxc $ \c -> do
  fn <- mkFn mkBoolBoolFn g c
  toBool <$> fn (if b then 1 else 0)

getItemFn :: Field C'lxc_container (FunPtr ContainerGetItemFn) -> String -> LXC (Maybe String)
getItemFn g s = lxc $ \c -> do
  fn <- mkFn mkGetItemFn g c
  withCString s $ \cs -> do
    -- call with NULL for retv to determine size of a buffer we need to allocate
    sz <- fn cs nullPtr 0
    if (sz < 0)
      then return Nothing
      else allocaBytes (fromIntegral sz) $ \cretv -> do
        -- we call fn second time to actually get item into cretv buffer
        fn cs cretv sz
        Just <$> peekCString cretv

setItemFn :: Field C'lxc_container (FunPtr ContainerSetItemFn) -> String -> Maybe String -> LXC Bool
setItemFn g k v = lxc $ \c -> do
  fn <- mkFn mkSetItemFn g c
  withCString k $ \ck ->
    maybeWith withCString v $ \cv ->
      toBool <$> fn ck cv

setItemFn' :: Field C'lxc_container (FunPtr ContainerSetItemFn) -> String -> String -> LXC Bool
setItemFn' g k v = setItemFn g k (Just v)

-- | Whether container wishes to be daemonized.
getDaemonize :: LXC Bool
getDaemonize = lxc $ \c -> toBool <$> peek (p'lxc_container'daemonize c)

-- | Get last container's error.
getLastError :: LXC (Maybe LXCError)
getLastError = lxc $ \c -> do
  cmsg <- peek (p'lxc_container'error_string c)
  msg  <- maybePeek peekCString cmsg
  num  <- fromIntegral <$> peek (p'lxc_container'error_num c)
  return $ LXCError <$> msg <*> pure num

-- | Determine if @\/var\/lib\/lxc\/\$name\/config@ exists.
--
-- @True@ if container is defined, else @False@.
isDefined :: LXC Bool
isDefined = boolFn p'lxc_container'is_defined

-- | Determine if container is running.
--
-- @True@ on success, else @False@.
isRunning :: LXC Bool
isRunning = boolFn p'lxc_container'is_running

-- | Determine state of container.
state :: LXC ContainerState
state = lxc $ \c -> do
  fn <- peek (p'lxc_container'state c)
  cs <- mkStringFn fn c  -- we do not need to free cs
  parseState <$> peekCString cs

-- | Freeze running container.
--
-- @True@ on success, else @False@.
freeze :: LXC Bool
freeze = boolFn p'lxc_container'freeze

-- | Thaw a frozen container.
--
-- @True@ on success, else @False@.
unfreeze :: LXC Bool
unfreeze = boolFn p'lxc_container'unfreeze

-- | Determine process ID of the containers init process.
initPID :: LXC (Maybe ProcessID)
initPID = lxc $ \c -> do
  fn <- mkFn mkProcessIDFn p'lxc_container'init_pid c
  pid <- fromIntegral <$> fn
  if (pid < 0)
    then return Nothing
    else return (Just pid)

-- | Load the specified configuration for the container.
loadConfig :: Maybe FilePath  -- ^ Full path to alternate configuration file, or @Nothing@ to use the default configuration file.
           -> LXC Bool        -- ^ @True@ on success, else @False@.
loadConfig = stringBoolFn p'lxc_container'load_config

-- | Start the container.
start :: Bool       -- ^ Use @lxcinit@ rather than @\/sbin\/init@.
      -> [String]   -- ^ Array of arguments to pass to init.
      -> LXC Bool   -- ^ @True@ on success, else @False@.
start useinit argv = lxc $ \c -> do
  fn <- mkFn mkStartFn p'lxc_container'start c
  case argv of
    [] -> toBool <$> fn (fromBool useinit) nullPtr
    _  -> do
      withMany withCString argv $ \cargv ->
        withArray0 nullPtr cargv $ \cargv' ->
          toBool <$> fn (fromBool useinit) cargv'

-- | Stop the container.
--
-- @True@ on success, else @False@.
stop :: LXC Bool
stop = boolFn p'lxc_container'stop

-- | Determine if the container wants to run disconnected from the terminal.
wantDaemonize :: Bool       -- ^ Value for the daemonize bit.
              -> LXC Bool   -- ^ @True@ if container wants to be daemonised, else @False@.
wantDaemonize = boolBoolFn p'lxc_container'want_daemonize

-- | Determine whether container wishes all file descriptors to be closed on startup.
wantCloseAllFDs :: Bool       -- ^ Value for the @close_all_fds@ bit.
                -> LXC Bool   -- ^ @True@ if container wants to be daemonised, else @False@.
wantCloseAllFDs = boolBoolFn p'lxc_container'want_close_all_fds

-- | Return current config file name.
configFileName :: LXC (Maybe FilePath)
configFileName = lxc $ \c -> do
  fn <- peek (p'lxc_container'config_file_name c)
  cs <- mkStringFn fn c
  s <- maybePeek peekCString cs
  when (isJust s) $ free cs
  return s

-- | Wait for container to reach a particular state.
--
-- * A timeout of @-1@ means wait forever.
-- A timeout @0@ means do not wait.
wait :: ContainerState  -- ^ State to wait for.
     -> Int             -- ^ Timeout in seconds.
     -> LXC Bool        -- ^ @True@ if state reached within timeout, else @False@.
wait s t = lxc $ \c -> do
  fn <- mkFn mkWaitFn p'lxc_container'wait c
  withCString (printState s) $ \cs ->
    toBool <$> fn cs (fromIntegral t)

-- | Set a key/value configuration option.
setConfigItem :: String     -- ^ Name of option to set.
              -> String     -- ^ Value to set.
              -> LXC Bool   -- ^ @True@ on success, else @False@.
setConfigItem = setItemFn' p'lxc_container'set_config_item

-- | Delete the container.
--
-- @True@ on success, else @False@.
--
-- * NOTE: Container must be stopped and have no dependent snapshots.
destroy :: LXC Bool
destroy = boolFn p'lxc_container'destroy

-- | Save configuaration to a file.
saveConfig :: FilePath    -- ^ Full path to file to save configuration in.
           -> LXC Bool    -- ^ @True@ on success, else @False@.
saveConfig s = stringBoolFn p'lxc_container'save_config (Just s)

-- | Rename a container.
rename :: String      -- ^ New name to be used for the container.
       -> LXC Bool    -- ^ @True@ on success, else @False@.
rename s = stringBoolFn p'lxc_container'rename (Just s)

-- | Request the container reboot by sending it @SIGINT@.
--
--  @True@ if reboot request successful, else @False@.
reboot :: LXC Bool
reboot = boolFn p'lxc_container'reboot

-- | Request the container shutdown by sending it @SIGPWR@.
shutdown :: Int         -- ^ Seconds to wait before returning false. (@-1@ to wait forever, @0@ to avoid waiting).
         -> LXC Bool    -- ^ @True@ if the container was shutdown successfully, else @False@.
shutdown n = lxc $ \c -> do
  fn <- mkFn mkShutdownFn p'lxc_container'shutdown c
  toBool <$> fn (fromIntegral n)

-- | Completely clear the containers in-memory configuration.
clearConfig :: LXC ()
clearConfig = lxc $ join . mkFn mkClearConfigFn p'lxc_container'clear_config

-- | Retrieve the value of a config item.
getConfigItem :: String             -- ^ Name of option to get.
              -> LXC (Maybe String) -- ^ The item or @Nothing@ on error.
getConfigItem = getItemFn p'lxc_container'get_config_item

-- | Retrieve the value of a config item from running container.
getRunningConfigItem :: String              -- ^ Name of option to get.
                     -> LXC (Maybe String)  -- ^ The item or @Nothing@ on error.
getRunningConfigItem k = lxc $ \c -> do
  fn <- mkFn mkGetRunningConfigItemFn p'lxc_container'get_running_config_item c
  withCString k $ \ck -> do
    cv <- fn ck
    v <- maybePeek peekCString cv
    when (isJust v) $ free cv
    return v

-- | Retrieve a list of config item keys given a key prefix.
getKeys :: String        -- ^ Key prefix.
        -> LXC [String]  -- ^ List of keys.
getKeys kp = concatMap lines . maybeToList <$> getItemFn p'lxc_container'get_keys kp

-- | Obtain a list of network interfaces.
getInterfaces :: LXC [String]
getInterfaces = lxc $ \c -> do
  cifs  <- join $ mkFn mkGetInterfacesFn p'lxc_container'get_interfaces c
  if (cifs == nullPtr)
    then return []
    else do
      cifs' <- peekArray0 nullPtr cifs
      ifs   <- mapM peekCString cifs'
      mapM_ free cifs'
      free cifs
      return ifs

-- | Determine the list of container IP addresses.
getIPs :: String        -- ^ Network interface name to consider.
       -> String        -- ^ Network family (for example @"inet"@, @"inet6"@).
       -> Word32        -- ^ IPv6 scope id (ignored if family is not "inet6").
       -> LXC [String]  -- ^ A list of network interfaces.
getIPs iface fam sid = lxc $ \c -> do
  fn <- mkFn mkGetIPsFn p'lxc_container'get_ips c
  withCString iface $ \ciface ->
    withCString fam $ \cfam -> do
      cips  <- fn ciface cfam (fromIntegral sid)
      if (cips == nullPtr)
        then return []
        else do
          cips' <- peekArray0 nullPtr cips
          ips   <- mapM peekCString cips'
          mapM_ free cips'
          free cips
          return ips

-- | Retrieve the specified cgroup subsystem value for the container.
getCGroupItem :: String              -- ^ @cgroup@ subsystem to retrieve.
              -> LXC (Maybe String)  -- ^ @cgroup@ subsystem value or @Nothing@ on error.
getCGroupItem = getItemFn p'lxc_container'get_cgroup_item

-- | Set the specified cgroup subsystem value for the container.
setCGroupItem :: String     -- ^ @cgroup@ subsystem to consider.
              -> String     -- ^ Value to set.
              -> LXC Bool   -- ^ @True@ on success, else @False@.
setCGroupItem = setItemFn' p'lxc_container'set_cgroup_item

-- | Clear a configuration item.
--
-- Analog of 'setConfigItem'.
clearConfigItem :: String     -- ^ Name of option to clear.
                -> LXC Bool   -- ^ @True@ on success, else @False@.
clearConfigItem s = stringBoolFn p'lxc_container'clear_config_item (Just s)

-- | Determine full path to the containers configuration file.
--
-- Each container can have a custom configuration path. However
-- by default it will be set to either the @LXCPATH@ configure
-- variable, or the lxcpath value in the @LXC_GLOBAL_CONF@ configuration
-- file (i.e. @\/etc\/lxc\/lxc.conf@).
--
-- The value for a specific container can be changed using
-- 'setConfigPath'.
getConfigPath :: LXC FilePath
getConfigPath = lxc $ \c -> do
  cs <- join $ mkFn mkStringFn p'lxc_container'get_config_path c
  s <- peekCString cs
  free cs
  return s

-- | Set the full path to the containers configuration file.
setConfigPath :: FilePath   -- ^ Full path to configuration file.
              -> LXC Bool    -- ^ @True@ on success, else @False@.
setConfigPath s = stringBoolFn p'lxc_container'set_config_path (Just s)

-- | Copy a stopped container.
clone :: Maybe String   -- ^ New name for the container. If @Nothing@, the same name is used and a new lxcpath MUST be specified.
      -> Maybe FilePath -- ^ lxcpath in which to create the new container. If @Nothing@, the original container's lxcpath will be used.
      -> [CloneOption]  -- ^ Additional 'CloneOption' flags to change the cloning behaviour.
      -> Maybe String   -- ^ Optionally force the cloned bdevtype to a specified plugin. By default the original is used (subject to snapshot requirements).
      -> Maybe String   -- ^ Information about how to create the new storage (i.e. fstype and fsdata).
      -> Maybe Word64   -- ^ In case of a block device backing store, an optional size. If @Nothing@, the original backing store's size will be used if possible. Note this only applies to the rootfs. For any other filesystems, the original size will be duplicated.
      -> [String]       -- ^ Additional arguments to pass to the clone hook script.
      -> LXC (Maybe Container)  -- ^ Newly-allocated copy of container $c$, or @Nothing@ on error.
clone newname lxcpath flags bdevtype bdevdata newsize hookargs = do
  oldname <- asks fst
  lxc $ \c -> do
    c' <- maybeWith withCString newname $ \cnewname ->
            maybeWith withCString lxcpath $ \clxcpath ->
              maybeWith withCString bdevtype $ \cbdevtype ->
                maybeWith withCString bdevdata $ \cbdevdata ->
                  withMany withCString hookargs $ \chookargs ->
                    withArray0 nullPtr chookargs $ \chookargs' -> do
                      fn <- mkFn mkCloneFn p'lxc_container'clone c
                      fn
                        cnewname
                        clxcpath
                        (mkFlags cloneFlag flags)
                        cbdevtype
                        cbdevdata
                        (fromMaybe 0 newsize)
                        chookargs'
    c'' <- maybePeek peekC'lxc_container c'
    when (isJust c'') $ do
      dropRef c'
      return ()
    return $ c'' <*> pure (fromMaybe oldname newname)

-- | Allocate a console tty for the container.
--
-- * The returned file descriptor is used to keep the tty
-- allocated. The caller should call close(2) on the returned file
-- descriptor when no longer required so that it may be allocated
-- by another caller.
consoleGetFD :: Maybe Int                   -- ^ Terminal number to attempt to allocate, or @Nothing@ to allocate the first available tty.
             -> LXC (Maybe (Int, Int, Int)) -- ^ Tuple /@<fd, ttynum, masterfd>@/ where @fd@ is file descriptor number, @ttynum@ is terminal number and @masterfd@ is file descriptor refering to the master side of the pty.
consoleGetFD ttynum = lxc $ \c -> do
  fn <- mkFn mkConsoleGetFDFn p'lxc_container'console_getfd c
  alloca $ \cttynum ->
    alloca $ \cmasterfd -> do
      poke cttynum (fromIntegral $ fromMaybe (-1) ttynum)
      fd       <- fromIntegral <$> fn cttynum cmasterfd
      ttynum'  <- fromIntegral <$> peek cttynum
      masterfd <- fromIntegral <$> peek cmasterfd
      if (fd < 0)
        then return Nothing
        else return $ Just (fd, ttynum', masterfd)

-- | Allocate and run a console tty.
console :: Maybe Int  -- ^ Terminal number to attempt to allocate, @Nothing@ to allocate the first available tty or @Just 0@ to allocate the console.
        -> Fd         -- ^ File descriptor to read input from.
        -> Fd         -- ^ File descriptor to write output to.
        -> Fd         -- ^ File descriptor to write error output to.
        -> Int        -- ^ The escape character (@1 == \'a\'@, @2 == \'b\'@, ...).
        -> LXC Bool   -- ^ @True@ on success, else @False@.
console ttynum stdin stdout stderr escape = lxc $ \c -> do
  fn <- mkFn mkConsoleFn p'lxc_container'console c
  toBool <$> fn (fromIntegral $ fromMaybe (-1) ttynum)
                (fromIntegral stdin)
                (fromIntegral stdout)
                (fromIntegral stderr)
                (fromIntegral escape)

-- | Create a sub-process attached to a container and run a function inside it.
attach :: AttachExecFn          -- ^ Function to run.
       -> AttachCommand         -- ^ Data to pass to @exec@ function.
       -> AttachOptions         -- ^ Attach options.
       -> LXC (Maybe ProcessID) -- ^ Process ID of process running inside container @c@ that is running @exec@ function, or @Nothing@ on error.
attach exec cmd opts = lxc $ \c -> do
  fn <- mkFn mkAttachFn p'lxc_container'attach c
  withC'lxc_attach_command_t cmd $ \ccmd ->
    withC'lxc_attach_options_t opts $ \copts ->
      alloca $ \cpid -> do
        ret <- fn (getAttachExecFn exec) (castPtr ccmd) copts cpid
        if (ret < 0)
          then return Nothing
          else Just . fromIntegral <$> peek cpid

-- | Run a program inside a container and wait for it to exit.
attachRunWait :: AttachOptions        -- ^ Attach options.
              -> String               -- ^ Full path inside container of program to run.
              -> [String]             -- ^ Array of arguments to pass to program.
              -> LXC (Maybe ExitCode) -- ^ @waitpid(2)@ status of exited process that ran program, or @Nothing@ on error.
attachRunWait opts prg argv = lxc $ \c -> do
  fn <- mkFn mkAttachRunWaitFn p'lxc_container'attach_run_wait c
  withCString prg $ \cprg ->
    withMany withCString argv $ \cargv ->
      withArray0 nullPtr cargv $ \cargv' ->
        withC'lxc_attach_options_t opts $ \copts -> do
          ret <- fromIntegral <$> fn copts cprg cargv'
          case ret of
            _ | ret < 0 -> return Nothing
            0           -> return $ Just ExitSuccess
            _           -> return $ Just (ExitFailure ret)

-- | Create a container snapshot.
--
-- Assuming default paths, snapshots will be created as
-- @\/var\/lib\/lxc\/\<c\>\/snaps\/snap\<n\>@
-- where @\<c\>@ represents the container name and @\<n\>@
-- represents the zero-based snapshot number.
snapshot :: Maybe FilePath  -- ^ Full path to file containing a description of the snapshot.
         -> LXC (Maybe Int) -- ^ @Nothing@ on error, or zero-based snapshot number.
snapshot path = lxc $ \c -> do
  fn <- mkFn mkSnapshotFn p'lxc_container'snapshot c
  maybeWith withCString path $ \cpath -> do
    n <- fn cpath
    if (n == -1)
      then return Nothing
      else return (Just $ fromIntegral n)

peekC'lxc_snapshot :: Ptr C'lxc_snapshot -> IO Snapshot
peekC'lxc_snapshot ptr = Snapshot
  <$> peekField peekCString             p'lxc_snapshot'name
  <*> peekField (maybePeek peekCString) p'lxc_snapshot'comment_pathname
  <*> peekField peekCString             p'lxc_snapshot'timestamp
  <*> peekField peekCString             p'lxc_snapshot'lxcpath
  where
    peekField g f = peek (f ptr) >>= g

-- | Obtain a list of container snapshots.
snapshotList :: LXC [Snapshot]
snapshotList = lxc $ \c -> do
  alloca $ \css -> do
    fn <- mkFn mkSnapshotListFn p'lxc_container'snapshot_list c
    n  <- fromIntegral <$> fn css
    if (n <= 0)
      then return []
      else do
        css'  <- peek css
        let css'' = take n $ iterate (flip advancePtr 1) css'
        css   <- mapM peekC'lxc_snapshot css''
        forM_ css'' $ join . mkFn mkFreeFn p'lxc_snapshot'free
        free css'
        return css

-- | Create a new container based on a snapshot.
--
-- The restored container will be a copy (not snapshot) of the snapshot,
-- and restored in the lxcpath of the original container.
--
-- * /WARNING:/ If new name is the same as the current container
-- name, the container will be destroyed. However, this will
-- fail if the snapshot is overlay-based, since the snapshots
-- will pin the original container.
-- * /NOTE:/ As an example, if the container exists as @\/var\/lib\/lxc\/c1@, snapname might be @"snap0"@
-- (representing @\/var\/lib\/lxc\/c1\/snaps\/snap0@). If new name is @c2@,
-- then @snap0@ will be copied to @\/var\/lib\/lxc\/c2@.
snapshotRestore :: String     -- ^ Name of snapshot.
                -> String     -- ^ Name to be used for the restored snapshot.
                -> LXC Bool   -- ^ @True@ on success, else @False@.
snapshotRestore = setItemFn' p'lxc_container'snapshot_restore

-- | Destroy the specified snapshot.
snapshotDestroy :: String     -- ^ Name of snapshot.
                -> LXC Bool   -- ^ @True@ on success, else @False@.
snapshotDestroy n = stringBoolFn p'lxc_container'snapshot_destroy (Just n)

-- | Determine if the caller may control the container.
--
-- @False@ if there is a control socket for the container monitor
-- and the caller may not access it, otherwise returns @True@.
mayControl :: LXC Bool
mayControl = boolFn p'lxc_container'may_control

-- | Add specified device to the container.
addDeviceNode :: FilePath       -- ^ Full path of the device.
              -> Maybe FilePath -- ^ Alternate path in the container (or @Nothing@ to use source path).
              -> LXC Bool       -- ^ @True@ on success, else @False@.
addDeviceNode = setItemFn p'lxc_container'add_device_node

-- | Remove specified device from the container.
removeDeviceNode :: FilePath       -- ^ Full path of the device.
                 -> Maybe FilePath -- ^ Alternate path in the container (or @Nothing@ to use source path).
                 -> LXC Bool       -- ^ @True@ on success, else @False@.
removeDeviceNode = setItemFn p'lxc_container'remove_device_node

-- | Create a container.
create :: String            -- ^ Template to execute to instantiate the root filesystem and adjust the configuration.
       -> Maybe String      -- ^ Backing store type to use (if @Nothing@, @dir@ type will be used by default).
       -> Maybe BDevSpecs   -- ^ Additional parameters for the backing store (for example LVM volume group to use).
       -> [CreateOption]    -- ^ 'CreateOption' flags. /Note: LXC 1.0 supports only @CreateQuiet@ option./
       -> [String]          -- ^ Arguments to pass to the template.
       -> LXC Bool          -- ^ @True@ on success. @False@ otherwise.
create t bdevtype bdevspecs flags argv = lxc $ \c -> toBool <$> do
  withMany withCString argv $ \cargv ->
    withArray0 nullPtr cargv $ \cargv' ->
       withCString t $ \ct ->
         maybeWith withCString bdevtype $ \cbdevtype ->
           maybeWith withC'bdev_specs bdevspecs $ \cbdevspecs -> do
             fn <- peek $ p'lxc_container'create $ c
             mkCreateFn fn
               (c)
               ct
               cbdevtype
               nullPtr
               (mkFlags createFlag flags)
               cargv'

-- | Add a reference to the specified container.
getRef :: Ptr C'lxc_container -> IO Bool
getRef c = toBool <$> c'lxc_container_get c

-- | Drop a reference to the specified container.
--
-- @Just False@ on success, @Just True@ if reference was successfully dropped
-- and container has been freed, and @Nothing@ on error.
dropRef :: Ptr C'lxc_container -> IO (Maybe Bool)
dropRef c = do
  n <- c'lxc_container_put c
  return $ case n of
             0 -> Just False
             1 -> Just True
             _ -> Nothing

-- | Obtain a list of all container states.
getWaitStates :: IO [ContainerState]
getWaitStates = do
  sz <- fromIntegral <$> c'lxc_get_wait_states nullPtr
  allocaArray sz $ \cstates -> do
    c'lxc_get_wait_states cstates
    cstates' <- peekArray sz cstates
    map parseState <$> mapM peekCString cstates'
    -- we do not need to free the strings themselves

-- | Get the value for a global config key.
getGlobalConfigItem :: String             -- ^ The name of the config key.
                    -> IO (Maybe String)  -- ^ String representing the current value for the key. @Nothing@ on error.
getGlobalConfigItem k = do
  withCString k $ \ck -> do
    cv <- c'lxc_get_global_config_item ck
    maybePeek peekCString cv

-- | Determine version of LXC.
getVersion :: IO String
getVersion = c'lxc_get_version >>= peekCString

listContainersFn :: (CString -> Ptr (Ptr CString) -> Ptr (Ptr (Ptr C'lxc_container)) -> IO CInt)
                 -> Maybe String
                 -> IO [Container]
listContainersFn f lxcpath = do
  maybeWith withCString lxcpath $ \clxcpath ->
    alloca $ \cnames ->
      alloca $ \ccontainers -> do
        n <- fromIntegral <$> f clxcpath cnames ccontainers
        if (n <= 0)
          then return []
          else do
            cnames'  <- peek cnames
            cnames'' <- peekArray n cnames'
            names    <- mapM peekCString cnames''
            mapM_ free cnames''
            free cnames'

            ccontainers'  <- peek ccontainers
            ccontainers'' <- peekArray n ccontainers'
            containers    <- mapM peekC'lxc_container ccontainers''
            mapM_ free ccontainers''
            free ccontainers'

            return $ zipWith ($) containers names


-- | Get a list of defined containers in a lxcpath.
listDefinedContainers :: Maybe String               -- ^ lxcpath under which to look.
                      -> IO [Container]   -- ^ List of <name, container> pairs.
listDefinedContainers = listContainersFn c'list_defined_containers

-- | Get a list of active containers for a given lxcpath.
listActiveContainers :: Maybe String               -- ^ Full @LXCPATH@ path to consider.
                     -> IO [Container]   -- ^ List of <name, container> pairs.
listActiveContainers = listContainersFn c'list_active_containers

-- | Get a complete list of all containers for a given lxcpath.
listAllContainers :: Maybe String               -- ^ Full @LXCPATH@ path to consider.
                  -> IO [Container]   -- ^ List of <name, container> pairs.
listAllContainers = listContainersFn c'list_all_containers

-- | Close log file.
logClose :: IO ()
logClose = c'lxc_log_close

