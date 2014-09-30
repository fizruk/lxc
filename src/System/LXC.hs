-----------------------------------------------------------------------------
-- |
-- Module      :  System.LXC
-- Copyright   :  (c) Nickolay Kudasov 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  nickolay.kudasov@gmail.com
--
-- Create, control and manage LXC containers through Haskell API.
-- You can get more info about LXC at <https://help.ubuntu.com/lts/serverguide/lxc.html> and <https://linuxcontainers.org>.
--
-- Most of container-related functions (e.g. 'start', 'attach', 'destroy') perform in 'System.LXC.Container.LXC' monad.
-- To run @'System.LXC.Container.LXC' a@ computation you need to specify a container using 'withContainer' function.
-- When working with a single container it might be handy to have an alias like this:
--
-- @
-- let containerName = withContainer (Container "container-name" configPath)
-- @
--
-- You can start using Haskell LXC API bindings similar to a command line tool from GHCi:
--
-- @
-- $ ghci
-- >>> import System.LXC
-- >>> let trusty = withContainer (Container "trusty" Nothing)
-- >>> trusty $ create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
-- Using image from local cache
-- Unpacking the rootfs
--
-- ---
-- You just created an Ubuntu container (release=trusty, arch=amd64, variant=default)
-- The default username\/password is: ubuntu \/ ubuntu
-- To gain root privileges, please use sudo.
--
-- True
-- >>> trusty $ start False []
-- True
-- >>> trusty state
-- ContainerRunning
-- >>> trusty $ attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
-- Hello, world!
-- Just ExitSuccess
-- >>> trusty stop
-- True
-- >>> Just trustySnapC <- trusty $ clone (Just "trusty-snap") Nothing [CloneSnapshot] Nothing Nothing Nothing []
-- >>> let trustySnap = withContainer trustySnapC
-- >>> trustySnap $ start False []
-- True
-- >>> trustySnap getInterfaces
-- ["eth0","lo"]
-- >>> trustySnap $ getIPs "eth0" "inet" 0
-- ["10.0.3.135"]
-- >>> trustySnap $ shutdown (-1)
-- True
-- >>> trustySnap state
-- ContainerStopped
-- @
--
-- For more examples, please see @examples\/@ folder.
--
-----------------------------------------------------------------------------
module System.LXC (
  module System.LXC.Container,
  module System.LXC.AttachOptions
) where

import System.LXC.Container
import System.LXC.AttachOptions
