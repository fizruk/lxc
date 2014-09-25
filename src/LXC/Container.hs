module LXC.Container where

import Bindings.LXC.Container
import Bindings.LXC.Sys.Types

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.List (foldl')

import Foreign.C.Types
import Foreign.C.String (withCString, newCString, CString)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (nullPtr, Ptr, FunPtr)
import Foreign.Storable

type ContainerCreateFn = Ptr C'lxc_container -> CString -> CString -> Ptr C'bdev_specs -> CInt -> Ptr CString -> IO CBool
foreign import ccall "dynamic"
  mkCreateFn :: FunPtr ContainerCreateFn -> ContainerCreateFn

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
  = CreateQuiet          -- ^ Redirect @stdin@ to @/dev/zero@ and @stdout@ and @stderr@ to @/dev/null@.
  | CreateMaxFlags       -- ^ Number of @LXC_CREATE*@ flags.
  deriving (Eq, Ord)

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

-- | Create a container.
create :: Container         -- ^ Container (with lxcpath, name and a starting configuration set).
       -> String            -- ^ Template to execute to instantiate the root filesystem and adjust the configuration.
       -> bDevType          -- ^ Backing store type to use (if @NULL@, @dir@ will be used).
       -> bDevSpecs         -- ^ Additional parameters for the backing store (for example LVM volume group to use).
       -> [CreateOption]    -- ^ 'CreateOption' options. /Note: LXC 1.0 supports only @CreateQuiet@ option./
       -> [String]          -- ^ Arguments to pass to the template.
       -> IO Bool           -- ^ @True@ on success. @False@ otherwise.
create c t bdevtype bdevspecs flags argv = do
  cargv <- mapM newCString argv
  r     <- withArray0 nullPtr cargv $ \cargv' ->
              withCString t $ \ct -> do
                fn <- peek $ p'lxc_container'create $ getContainer c
                mkCreateFn fn
                  (getContainer c)
                  ct
                  nullPtr
                  nullPtr
                  (mkFlags createFlag flags)
                  cargv'
  return (r == 1)
