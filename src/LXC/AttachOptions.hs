module LXC.AttachOptions where

import Bindings.LXC.AttachOptions

import Data.Int
import Data.Maybe

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import LXC.Internal.Utils

import System.Posix.Types

newtype AttachExecFn = AttachExecFn { getAttachExecFn :: C_lxc_attach_exec_t }

-- | LXC environment policy.
data AttachEnvPolicy
  = AttachKeepEnv     -- ^ Retain the environment.
  | AttachClearEnv    -- ^ Clear the environment.
  deriving (Eq, Show)

fromAttachEnvPolicy :: Num a => AttachEnvPolicy -> a
fromAttachEnvPolicy AttachKeepEnv   = c'LXC_ATTACH_KEEP_ENV
fromAttachEnvPolicy AttachClearEnv  = c'LXC_ATTACH_CLEAR_ENV

data AttachFlag
  = AttachMoveToCGroup      -- ^ Move to cgroup. On by default.
  | AttachDropCapabilities  -- ^ Drop capabilities. On by default.
  | AttachSetPersonality    -- ^ Set personality. On by default
  | AttachLSMExec           -- ^ Execute under a Linux Security Module. On by default.
  | AttachRemountProcSys    -- ^ Remount /proc filesystem. Off by default.
  | AttachLSMNow            -- ^ FIXME: unknown. Off by default.
  | AttachDefault           -- ^ Mask of flags to apply by default.
  | AttachLSM               -- ^ All Linux Security Module flags.
  deriving (Eq, Show)

fromAttachFlag :: Num a => AttachFlag -> a
fromAttachFlag AttachMoveToCGroup     = c'LXC_ATTACH_MOVE_TO_CGROUP
fromAttachFlag AttachDropCapabilities = c'LXC_ATTACH_DROP_CAPABILITIES
fromAttachFlag AttachSetPersonality   = c'LXC_ATTACH_SET_PERSONALITY
fromAttachFlag AttachLSMExec          = c'LXC_ATTACH_LSM_EXEC
fromAttachFlag AttachRemountProcSys   = c'LXC_ATTACH_REMOUNT_PROC_SYS
fromAttachFlag AttachLSMNow           = c'LXC_ATTACH_LSM_NOW
fromAttachFlag AttachDefault          = c'LXC_ATTACH_DEFAULT
fromAttachFlag AttachLSM              = c'LXC_ATTACH_LSM

-- | LXC attach options for 'LXC.Container.attach'.
--
-- * /NOTE:/ for @stdin@, @stdout@ and @stderr@ descriptors
-- @dup2()@ will be used before calling @exec_function@,
-- (assuming not @0@, @1@ and @2@ are specified) and the
-- original fds are closed before passing control
-- over. Any @O_CLOEXEC@ flag will be removed after that.
data AttachOptions = AttachOptions
  { attachFlags         :: [AttachFlag]       -- ^ Any combination of 'AttachFlag' flags.
  , attachNamespaces    :: Int                -- ^ The namespaces to attach to (CLONE_NEW... flags).
  -- | Initial personality (@Nothing@ to autodetect).
  --
  -- * This may be ignored if @lxc@ is compiled without personality support
  , attachPersonality   :: Maybe Int64
  -- | Inital current directory, @Nothing@ to use @cwd@.
  --
  -- If the current directory does not exist in the container, the
  -- root directory will be used instead because of kernel defaults.
  , attachInitialCWD    :: Maybe FilePath
  -- | The user-id to run as.
  --
  -- * /NOTE:/ Set to @-1@ for default behaviour (init uid for userns
  -- containers or @0@ (super-user) if detection fails).
  , attachUID           :: UserID
  -- |The group-id to run as.
  --
  -- * /NOTE:/ Set to @-1@ for default behaviour (init gid for userns
  -- containers or @0@ (super-user) if detection fails).
  , attachGID           :: GroupID
  , attachEnvPolicy     :: AttachEnvPolicy    -- ^ Environment policy.
  , attachExtraEnvVars  :: [String]           -- ^ Extra environment variables to set in the container environment.
  , attachExtraKeepEnv  :: [String]           -- ^ Names of environment variables in existing environment to retain in container environment.
  , attachStdinFD       :: Fd                 -- ^ @stdin@ file descriptor.
  , attachStdoutFD      :: Fd                 -- ^ @stdout@ file descriptor.
  , attachStderrFD      :: Fd                 -- ^ @stderr@ file descriptor.
  }
  deriving (Show)

-- | Default attach options to use.
defaultAttachOptions :: AttachOptions
defaultAttachOptions = AttachOptions
  { attachFlags         = [AttachDefault]
  , attachNamespaces    = -1
  , attachPersonality   = Nothing
  , attachInitialCWD    = Nothing
  , attachUID           = -1
  , attachGID           = -1
  , attachEnvPolicy     = AttachKeepEnv
  , attachExtraEnvVars  = []
  , attachExtraKeepEnv  = []
  , attachStdinFD       = 0
  , attachStdoutFD      = 1
  , attachStderrFD      = 2
  }

-- | Representation of a command to run in a container.
data AttachCommand = AttachCommand
  { attachProgram :: String   -- ^ The program to run (passed to @execvp@).
  , attachArgv    :: [String] -- ^ The @argv@ of that program, including the program itself as the first element.
  }

withC'lxc_attach_options_t :: AttachOptions -> (Ptr C'lxc_attach_options_t -> IO a) -> IO a
withC'lxc_attach_options_t a f = do
  alloca $ \ca ->
    maybeWith withCString (attachInitialCWD a) $ \cinitialCWD ->
      withMany withCString (attachExtraEnvVars a) $ \cextraEnvVars ->
        withArray0 nullPtr cextraEnvVars $ \cextraEnvVars' ->
          withMany withCString (attachExtraKeepEnv a) $ \cextraKeepEnv ->
            withArray0 nullPtr cextraKeepEnv $ \cextraKeepEnv' -> do
              poke (p'lxc_attach_options_t'attach_flags   ca) (mkFlags fromAttachFlag . attachFlags              $ a)
              poke (p'lxc_attach_options_t'namespaces     ca) (fromIntegral . attachNamespaces                   $ a)
              poke (p'lxc_attach_options_t'personality    ca) (fromIntegral . fromMaybe (-1) . attachPersonality $ a)
              poke (p'lxc_attach_options_t'initial_cwd    ca) cinitialCWD
              poke (p'lxc_attach_options_t'uid            ca) (fromIntegral . attachUID                          $ a)
              poke (p'lxc_attach_options_t'gid            ca) (fromIntegral . attachGID                          $ a)
              poke (p'lxc_attach_options_t'env_policy     ca) (fromAttachEnvPolicy . attachEnvPolicy             $ a)
              poke (p'lxc_attach_options_t'extra_env_vars ca) cextraEnvVars'
              poke (p'lxc_attach_options_t'extra_keep_env ca) cextraKeepEnv'
              poke (p'lxc_attach_options_t'stdin_fd       ca) (fromIntegral . attachStdinFD                      $ a)
              poke (p'lxc_attach_options_t'stdout_fd      ca) (fromIntegral . attachStdoutFD                     $ a)
              poke (p'lxc_attach_options_t'stderr_fd      ca) (fromIntegral . attachStderrFD                     $ a)
              f ca

withC'lxc_attach_command_t :: AttachCommand -> (Ptr C'lxc_attach_command_t -> IO a) -> IO a
withC'lxc_attach_command_t a f = do
  alloca $ \ca ->
    withCString (attachProgram a) $ \cprogram ->
      withMany withCString (attachArgv a) $ \cargv ->
        withArray0 nullPtr cargv $ \cargv' -> do
          poke (p'lxc_attach_command_t'program ca) cprogram
          poke (p'lxc_attach_command_t'argv    ca) cargv'
          f ca

-- | Run a command in the container.
attachRunCommand :: AttachExecFn
attachRunCommand = AttachExecFn p'lxc_attach_run_command

-- | Run a shell command in the container.
attachRunShell :: AttachExecFn
attachRunShell = AttachExecFn p'lxc_attach_run_shell

