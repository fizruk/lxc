module LXC.Container where

import Bindings.LXC.Container
import Bindings.LXC.Sys.Types

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.List
import Data.Maybe
import Data.Word

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with, maybeWith, withMany)
import Foreign.Ptr (nullPtr, Ptr, FunPtr)
import Foreign.Storable

import System.Posix.Types (ProcessID)

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
cloneFlag :: CloneOption -> CInt
cloneFlag CloneKeepName       = c'LXC_CLONE_KEEPNAME
cloneFlag CloneKeepMacAddr    = c'LXC_CLONE_KEEPMACADDR
cloneFlag CloneSnapshot       = c'LXC_CLONE_SNAPSHOT
cloneFlag CloneKeepBDevType   = c'LXC_CLONE_KEEPBDEVTYPE
cloneFlag CloneMaybeSnapshot  = c'LXC_CLONE_MAYBE_SNAPSHOT
cloneFlag CloneMaxFlags       = c'LXC_CLONE_MAXFLAGS

-- | Turn 'CreateOption' into a bit flag.
createFlag :: CreateOption -> CInt
createFlag CreateQuiet    = c'LXC_CREATE_QUIET
createFlag CreateMaxFlags = c'LXC_CREATE_MAXFLAGS

-- | Collect flags in a single integer value.
mkFlags :: (a -> CInt) -> [a] -> CInt
mkFlags f = foldl' (.|.) 0 . map f

-- | Container object.
newtype Container = Container {
  getContainer :: Ptr C'lxc_container   -- ^ A pointer to @lxc_container@ structure.
}

data ContainerState
  = ContainerStopped
  | ContainerStarting
  | ContainerRunning
  | ContainerStopping
  | ContainerAborting
  | ContainerFreezing
  | ContainerFrozen
  | ContainerThawed
  | ContainerUnknownState
  deriving (Eq, Show)

parseState :: String -> ContainerState
parseState "STOPPED"  = ContainerStopped
parseState "STARTING" = ContainerStarting
parseState "RUNNING"  = ContainerRunning
parseState "STOPPING" = ContainerStopping
parseState "ABORTING" = ContainerAborting
parseState "FREEZING" = ContainerFreezing
parseState "FROZEN"   = ContainerFrozen
parseState "THAWED"   = ContainerThawed
parseState _          = ContainerUnknownState

printState :: ContainerState -> String
printState ContainerStopped       = "STOPPED"
printState ContainerStarting      = "STARTING"
printState ContainerRunning       = "RUNNING"
printState ContainerStopping      = "STOPPING"
printState ContainerAborting      = "ABORTING"
printState ContainerFreezing      = "FREEZING"
printState ContainerFrozen        = "FROZEN"
printState ContainerThawed        = "THAWED"
printState ContainerUnknownState  = "UNKNOWN"

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

-- | Allocate a new container.
mkContainer :: String           -- ^ Name to use for the container.
            -> Maybe FilePath   -- ^ Full path to configuration file to use.
            -> IO Container     -- ^ Newly allocated container.
mkContainer name configPath = do
  c <- withCString name $ \cname ->
          case configPath of
            Nothing -> c'lxc_container_new cname nullPtr
            Just s  -> withCString s $ c'lxc_container_new cname
  when (c == nullPtr) $ error "failed to allocate new container"
  return $ Container c

type Field s a = Ptr s -> Ptr a

mkFn :: (t -> Ptr s) -> (FunPtr (Ptr s -> a) -> (Ptr s -> a)) -> Field s (FunPtr (Ptr s -> a)) -> t -> IO a
mkFn unwrap mk g t = do
  let s = unwrap t
  fn <- peek (g s)
  return $ mk fn s

boolFn :: Field C'lxc_container (FunPtr ContainerBoolFn) -> Container -> IO Bool
boolFn g c = do
  fn <- mkFn getContainer mkBoolFn g c
  (== 1) <$> fn

stringBoolFn :: Field C'lxc_container (FunPtr ContainerStringBoolFn) -> Container -> Maybe String -> IO Bool
stringBoolFn g c s = do
  fn <- mkFn getContainer mkStringBoolFn g c
  maybeWith withCString s $ \cs ->
    (== 1) <$> fn cs

boolBoolFn :: Field C'lxc_container (FunPtr ContainerBoolBoolFn) -> Container -> Bool -> IO Bool
boolBoolFn g c b = do
  fn <- mkFn getContainer mkBoolBoolFn g c
  (== 1) <$> fn (if b then 1 else 0)

getItemFn :: Field C'lxc_container (FunPtr ContainerGetItemFn) -> Container -> String -> IO (Maybe String)
getItemFn g c s = do
  fn <- mkFn getContainer mkGetItemFn g c
  withCString s $ \cs -> do
    -- call with NULL for retv to determine size of a buffer we need to allocate
    sz <- fn cs nullPtr 0
    if (sz < 0)
      then return Nothing
      else allocaBytes (fromIntegral sz) $ \cretv -> do
        -- we call fn second time to actually get item into cretv buffer
        fn cs cretv sz
        Just <$> peekCString cretv

setItemFn :: Field C'lxc_container (FunPtr ContainerSetItemFn) -> Container -> String -> Maybe String -> IO Bool
setItemFn g c k v = do
  fn <- mkFn getContainer mkSetItemFn g c
  withCString k $ \ck ->
    maybeWith withCString v $ \cv ->
      (== 1) <$> fn ck cv

setItemFn' :: Field C'lxc_container (FunPtr ContainerSetItemFn) -> Container -> String -> String -> IO Bool
setItemFn' g c k v = setItemFn g c k (Just v)

-- | Determine if @\/var\/lib\/lxc\/\$name\/config@ exists.
--
-- @True@ if container is defined, else @False@.
isDefined :: Container -> IO Bool
isDefined = boolFn p'lxc_container'is_defined

-- | Determine if container is running.
--
-- @True@ on success, else @False@.
isRunning :: Container -> IO Bool
isRunning = boolFn p'lxc_container'is_running

-- | Determine state of container.
state :: Container -> IO ContainerState
state (Container c) = do
  fn <- peek (p'lxc_container'state c)
  cs <- mkStringFn fn c  -- we do not need to free cs
  parseState <$> peekCString cs

-- | Freeze running container.
--
-- @True@ on success, else @False@.
freeze :: Container -> IO Bool
freeze = boolFn p'lxc_container'freeze

-- | Thaw a frozen container.
--
-- @True@ on success, else @False@.
unfreeze :: Container -> IO Bool
unfreeze = boolFn p'lxc_container'unfreeze

-- | Determine process ID of the containers init process.
initPID :: Container -> IO (Maybe ProcessID)
initPID c = do
  fn <- mkFn getContainer mkProcessIDFn p'lxc_container'init_pid c
  pid <- fromIntegral <$> fn
  if (pid < 0)
    then return Nothing
    else return (Just pid)

-- | Load the specified configuration for the container.
loadConfig :: Container       -- ^ Container.
           -> Maybe FilePath  -- ^ Full path to alternate configuration file, or @Nothing@ to use the default configuration file.
           -> IO Bool         -- ^ @True@ on success, else @False@.
loadConfig = stringBoolFn p'lxc_container'load_config

-- | Start the container.
start :: Container  -- ^ Container.
      -> Int        -- ^ Use @lxcinit@ rather than @/sbin/init@.
      -> [String]   -- ^ Array of arguments to pass to init.
      -> IO Bool    -- ^ @True@ on success, else @False@.
start c n argv = do
  fn <- mkFn getContainer mkStartFn p'lxc_container'start c
  withMany withCString argv $ \cargv ->
    withArray0 nullPtr cargv $ \cargv' ->
      (== 1) <$> fn (fromIntegral n) cargv'

-- | Stop the container.
--
-- @True@ on success, else @False@.
stop :: Container -> IO Bool
stop = boolFn p'lxc_container'stop

-- | Determine if the container wants to run disconnected from the terminal.
wantDaemonize :: Container  -- ^ Container.
              -> Bool       -- ^ Value for the daemonize bit.
              -> IO Bool    -- ^ @True@ if container wants to be daemonised, else @False@.
wantDaemonize = boolBoolFn p'lxc_container'want_daemonize

-- | Determine whether container wishes all file descriptors to be closed on startup.
wantCloseAllFDs :: Container  -- ^ Container.
                -> Bool       -- ^ Value for the @close_all_fds@ bit.
                -> IO Bool    -- ^ @True@ if container wants to be daemonised, else @False@.
wantCloseAllFDs = boolBoolFn p'lxc_container'want_close_all_fds

-- | Return current config file name.
configFileName :: Container -> IO (Maybe FilePath)
configFileName (Container c) = do
  fn <- peek (p'lxc_container'config_file_name c)
  cs <- mkStringFn fn c
  if (cs == nullPtr)
    then return Nothing
    else do
      s <- peekCString cs
      free cs
      return $ Just s

-- | Wait for container to reach a particular state.
--
-- * A timeout of @-1@ means wait forever.
-- A timeout @0@ means do not wait.
wait :: Container       -- ^ Container.
     -> ContainerState  -- ^ State to wait for.
     -> Int             -- ^ Timeout in seconds.
     -> IO Bool         -- ^ @True@ if state reached within timeout, else @False@.
wait c s t = do
  fn <- mkFn getContainer mkWaitFn p'lxc_container'wait c
  withCString (printState s) $ \cs ->
    (== 1) <$> fn cs (fromIntegral t)

-- | Set a key/value configuration option.
setConfigItem :: Container  -- ^ Container.
              -> String     -- ^ Name of option to set.
              -> String     -- ^ Value to set.
              -> IO Bool    -- ^ @True@ on success, else @False@.
setConfigItem = setItemFn' p'lxc_container'set_config_item

-- | Delete the container.
--
-- @True@ on success, else @False@.
--
-- * NOTE: Container must be stopped and have no dependent snapshots.
destroy :: Container -> IO Bool
destroy = boolFn p'lxc_container'destroy

-- | Save configuaration to a file.
saveConfig :: Container   -- ^ Container.
           -> FilePath    -- ^ Full path to file to save configuration in.
           -> IO Bool     -- ^ @True@ on success, else @False@.
saveConfig c s = stringBoolFn p'lxc_container'save_config c (Just s)

-- | Rename a container.
rename :: Container   -- ^ Container.
       -> String      -- ^ New name to be used for the container.
       -> IO Bool     -- ^ @True@ on success, else @False@.
rename c s = stringBoolFn p'lxc_container'rename c (Just s)

-- | Request the container reboot by sending it @SIGINT@.
--
--  @True@ if reboot request successful, else @False@.
reboot :: Container -> IO Bool
reboot = boolFn p'lxc_container'reboot

-- | Request the container shutdown by sending it @SIGPWR@.
shutdown :: Container   -- ^ Container.
         -> Int         -- ^ Seconds to wait before returning false. (@-1@ to wait forever, @0@ to avoid waiting).
         -> IO Bool     -- ^ @True@ if the container was shutdown successfully, else @False@.
shutdown c n = do
  fn <- mkFn getContainer mkShutdownFn p'lxc_container'shutdown c
  (== 1) <$> fn (fromIntegral n)

-- | Completely clear the containers in-memory configuration.
clearConfig :: Container -> IO ()
clearConfig = join . mkFn getContainer mkClearConfigFn p'lxc_container'clear_config

-- | Retrieve the value of a config item.
getConfigItem :: Container          -- ^ Container.
              -> String             -- ^ Name of option to get.
              -> IO (Maybe String)  -- ^ The item or @Nothing@ on error.
getConfigItem = getItemFn p'lxc_container'get_config_item

-- | Retrieve the value of a config item from running container.
getRunningConfigItem :: Container           -- ^ Container.
                     -> String              -- ^ Name of option to get.
                     -> IO (Maybe String)   -- ^ The item or @Nothing@ on error.
getRunningConfigItem c k = do
  fn <- mkFn getContainer mkGetRunningConfigItemFn p'lxc_container'get_running_config_item c
  withCString k $ \ck -> do
    cv <- fn ck
    if (cv == nullPtr)
      then return Nothing
      else do
        v <- peekCString cv
        free cv
        return (Just v)

-- | Retrieve a list of config item keys given a key prefix.
getKeys :: Container    -- ^ Container.
        -> String       -- ^ Key prefix.
        -> IO [String]  -- ^ List of keys.
getKeys c kp = concatMap lines . maybeToList <$> getItemFn p'lxc_container'get_keys c kp

-- | Obtain a list of network interfaces.
getInterfaces :: Container -> IO [String]
getInterfaces c = do
  cifs  <- join $ mkFn getContainer mkGetInterfacesFn p'lxc_container'get_interfaces c
  if (cifs == nullPtr)
    then return []
    else do
      cifs' <- peekArray0 nullPtr cifs
      ifs   <- mapM peekCString cifs'
      mapM_ free cifs'
      free cifs
      return ifs

-- | Determine the list of container IP addresses.
getIPs :: Container     -- ^ Container.
       -> String        -- ^ Network interface name to consider.
       -> String        -- ^ Network family (for example @"inet"@, @"inet6"@).
       -> Word32        -- ^ IPv6 scope id (ignored if family is not "inet6").
       -> IO [String]   -- ^ A list of network interfaces.
getIPs c iface fam sid = do
  fn <- mkFn getContainer mkGetIPsFn p'lxc_container'get_ips c
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
getCGroupItem :: Container          -- ^ Container.
              -> String             -- ^ @cgroup@ subsystem to retrieve.
              -> IO (Maybe String)  -- ^ @cgroup@ subsystem value or @Nothing@ on error.
getCGroupItem = getItemFn p'lxc_container'get_cgroup_item

-- | Set the specified cgroup subsystem value for the container.
setCGroupItem :: Container  -- ^ Container.
              -> String     -- ^ @cgroup@ subsystem to consider.
              -> String     -- ^ Value to set.
              -> IO Bool    -- ^ @True@ on success, else @False@.
setCGroupItem = setItemFn' p'lxc_container'set_cgroup_item

-- | Clear a configuration item.
--
-- Analog of 'setConfigItem'.
clearConfigItem :: Container  -- ^ Container.
                -> String     -- ^ Name of option to clear.
                -> IO Bool    -- ^ @True@ on success, else @False@.
clearConfigItem c s = stringBoolFn p'lxc_container'clear_config_item c (Just s)

-- | Determine full path to the containers configuration file.
--
-- Each container can have a custom configuration path. However
-- by default it will be set to either the @LXCPATH@ configure
-- variable, or the lxcpath value in the @LXC_GLOBAL_CONF@ configuration
-- file (i.e. @\/etc\/lxc\/lxc.conf@).
--
-- The value for a specific container can be changed using
-- 'setConfigPath'.
getConfigPath :: Container -> IO FilePath
getConfigPath c = do
  cs <- join $ mkFn getContainer mkStringFn p'lxc_container'get_config_path c
  s <- peekCString cs
  free cs
  return s

-- | Set the full path to the containers configuration file.
setConfigPath :: Container  -- ^ Container.
              -> FilePath   -- ^ Full path to configuration file.
              -> IO Bool    -- ^ @True@ on success, else @False@.
setConfigPath c s = stringBoolFn p'lxc_container'set_config_path c (Just s)

-- | Copy a stopped container.
clone :: Container      -- ^ Original container.
      -> Maybe String   -- ^ New name for the container. If @Nothing@, the same name is used and a new lxcpath MUST be specified.
      -> Maybe FilePath -- ^ lxcpath in which to create the new container. If @Nothing@, the original container's lxcpath will be used.
      -> [CloneOption]  -- ^ Additional 'CloneOption' flags to change the cloning behaviour.
      -> Maybe String   -- ^ Optionally force the cloned bdevtype to a specified plugin. By default the original is used (subject to snapshot requirements).
      -> Maybe String   -- ^ Information about how to create the new storage (i.e. fstype and fsdata).
      -> Maybe Word64   -- ^ In case of a block device backing store, an optional size. If @Nothing@, the original backing store's size will be used if possible. Note this only applies to the rootfs. For any other filesystems, the original size will be duplicated.
      -> [String]       -- ^ Additional arguments to pass to the clone hook script.
      -> IO Container
clone c newname lxcpath flags bdevtype bdevdata newsize hookargs = do
  c' <- maybeWith withCString newname $ \cnewname ->
          maybeWith withCString lxcpath $ \clxcpath ->
            maybeWith withCString bdevtype $ \cbdevtype ->
              maybeWith withCString bdevdata $ \cbdevdata ->
                withMany withCString hookargs $ \chookargs ->
                  withArray0 nullPtr chookargs $ \chookargs' -> do
                    fn <- peek $ p'lxc_container'clone $ getContainer c
                    mkCloneFn fn
                      (getContainer c)
                      cnewname
                      clxcpath
                      (mkFlags cloneFlag flags)
                      cbdevtype
                      cbdevdata
                      (fromMaybe 0 newsize)
                      chookargs'
  when (c' == nullPtr) $ error "failed to clone a container"
  return $ Container c'

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
snapshotRestore :: Container  -- ^ Container.
                -> String     -- ^ Name of snapshot.
                -> String     -- ^ Name to be used for the restored snapshot.
                -> IO Bool    -- ^ @True@ on success, else @False@.
snapshotRestore = setItemFn' p'lxc_container'snapshot_restore

-- | Determine if the caller may control the container.
--
-- @False@ if there is a control socket for the container monitor
-- and the caller may not access it, otherwise returns @True@.
mayControl :: Container -> IO Bool
mayControl = boolFn p'lxc_container'may_control

-- | Add specified device to the container.
addDeviceNode :: Container      -- ^ Container.
              -> FilePath       -- ^ Full path of the device.
              -> Maybe FilePath -- ^ Alternate path in the container (or @Nothing@ to use source path).
              -> IO Bool        -- ^ @True@ on success, else @False@.
addDeviceNode = setItemFn p'lxc_container'add_device_node

-- | Remove specified device from the container.
removeDeviceNode :: Container      -- ^ Container.
                 -> FilePath       -- ^ Full path of the device.
                 -> Maybe FilePath -- ^ Alternate path in the container (or @Nothing@ to use source path).
                 -> IO Bool        -- ^ @True@ on success, else @False@.
removeDeviceNode = setItemFn p'lxc_container'remove_device_node

-- | Create a container.
create :: Container         -- ^ Container (with lxcpath, name and a starting configuration set).
       -> String            -- ^ Template to execute to instantiate the root filesystem and adjust the configuration.
       -> Maybe String      -- ^ Backing store type to use (if @Nothing@, @dir@ type will be used by default).
       -> Maybe BDevSpecs   -- ^ Additional parameters for the backing store (for example LVM volume group to use).
       -> [CreateOption]    -- ^ 'CreateOption' flags. /Note: LXC 1.0 supports only @CreateQuiet@ option./
       -> [String]          -- ^ Arguments to pass to the template.
       -> IO Bool           -- ^ @True@ on success. @False@ otherwise.
create c t bdevtype bdevspecs flags argv = do
  r <- withMany withCString argv $ \cargv ->
          withArray0 nullPtr cargv $ \cargv' ->
             withCString t $ \ct ->
               maybeWith withCString bdevtype $ \cbdevtype ->
                 maybeWith withC'bdev_specs bdevspecs $ \cbdevspecs -> do
                   fn <- peek $ p'lxc_container'create $ getContainer c
                   mkCreateFn fn
                     (getContainer c)
                     ct
                     cbdevtype
                     nullPtr
                     (mkFlags createFlag flags)
                     cargv'
  return (r == 1)
