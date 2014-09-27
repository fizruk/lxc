module LXC.AttachOptions where

import Bindings.LXC.AttachOptions

import Data.Int

import System.Posix.Types

-- | LXC environment policy.
data AttachEnvPolicy
  = AttachKeepEnv     -- ^ Retain the environment.
  | AttachClearEnv    -- ^ Clear the environment.
  deriving (Eq, Show)

data AttachFlag
  = AttachMoveToCGroup      -- ^ Move to cgroup. On by default.
  | AttachDropCapabilities  -- ^ Drop capabilities. On by default.
  | AttachSetPersonality    -- ^ Set personality. On by default
  | AttachLSMExec           -- ^ Execute under a Linux Security Module. On by default.
  | AttachRemountProcSys    -- ^ Remount /proc filesystem. Off by default.
  | AttachLSMNow            -- ^ FIXME: unknown. Off by default.
  | AttachDefault           -- ^ Mask of flags to apply by default.
  deriving (Eq, Show)

-- | LXC attach options for 'LXC.Container.attach'.
--
-- * /NOTE:/ for @stdin@, @stdout@ and @stderr@ descriptors
-- @dup2()@ will be used before calling @exec_function@,
-- (assuming not @0@, @1@ and @2@ are specified) and the
-- original fds are closed before passing control
-- over. Any @O_CLOEXEC@ flag will be removed after that.
data AttachOptions = AttachOptions
  { attachFlags         :: [AttachFlag]       -- ^ Any combination of 'AttachFlag' flags.
  , attachNamespace     :: Int                -- ^ The namespaces to attach to (CLONE_NEW... flags).
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
  , attachNamespace     = -1
  , attachPersonality   = Just (-1)
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

