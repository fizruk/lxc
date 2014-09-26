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
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray0)
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

-- | Stop the container.
--
-- @True@ on success, else @False@.
stop :: Container -> IO Bool
stop = boolFn p'lxc_container'stop

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

-- | Clear a configuration item.
--
-- Analog of 'setConfigItem'.
clearConfigItem :: Container  -- ^ Container.
                -> String     -- ^ Name of option to clear.
                -> IO Bool    -- ^ @True@ on success, else @False@.
clearConfigItem c s = stringBoolFn p'lxc_container'clear_config_item c (Just s)

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

-- | Determine if the caller may control the container.
--
-- @False@ if there is a control socket for the container monitor
-- and the caller may not access it, otherwise returns @True@.
mayControl :: Container -> IO Bool
mayControl = boolFn p'lxc_container'may_control

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
