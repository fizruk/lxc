module LXC.Container where

import Bindings.LXC.Container

import Control.Applicative
import Control.Monad

import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr, Ptr)

data CloneOption
  = CloneKeepName        -- ^ Do not edit the rootfs to change the hostname.
  | CloneKeepMacAddr     -- ^ Do not change the MAC address on network interfaces.
  | CloneSnapshot        -- ^ Snapshot the original filesystem(s).
  | CloneKeepBDevType    -- ^ Use the same bdev type.
  | CloneMaybeSnapshot   -- ^ Snapshot only if bdev supports it, else copy.
  | CloneMaxFlags        -- ^ Number of @LXC_CLONE_*@ flags.
  deriving (Eq, Ord)

data CreateOption
  = CreateQuiet          -- ^ Redirect @stdin@ to @/dev/zero@ and @stdout@ and @stderr@ to @/dev/null@.
  | CreateMaxFlags       -- ^ Number of @LXC_CREATE*@ flags.
  deriving (Eq, Ord)

newtype Container = Container { getContainer :: Ptr C'lxc_container }

-- | Create a new container.
--
new :: String           -- ^ Name to use for the container.
    -> Maybe FilePath   -- ^ Full path to configuration file to use.
    -> IO Container     -- ^ Newly allocated container.
new name configPath = do
  c <- withCString name $ \cname ->
          case configPath of
            Nothing -> c'lxc_container_new cname nullPtr
            Just s  -> withCString s $ c'lxc_container_new cname
  when (c == nullPtr) $ error "failed to allocate new container"
  return $ Container c
