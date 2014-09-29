module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class

import System.Exit
import System.LXC

-- | Dump container state.
dumpState :: LXC ()
dumpState = do
  s <- state
  liftIO $ do
    putStr "Container state: "
    print s

-- | Dump container IPs by interface.
dumpIPs :: LXC ()
dumpIPs = do
  ifs <- getInterfaces
  liftIO $ putStrLn "Container IPs:"
  forM_ ifs $ \i -> do
    liftIO $ do
      putStr "- "
      putStrLn i
    ips <- getIPs i "inet" (-1)
    liftIO $ do
      forM_ ips $ \ip -> do
        putStr "  * "
        putStrLn ip

-- | Ping github.com from inside the container every second and wait until success.
waitForNetwork :: LXC ()
waitForNetwork = do
  ret <- attachRunWait defaultAttachOptions "ping" ["ping", "-c", "1", "github.com"]
  case ret of
    Just ExitSuccess -> return ()
    _ -> do
      liftIO $ threadDelay (10^6)  -- sleep 1 sec
      waitForNetwork

-- | Create Ubuntu Trusty Tahr container and perform some actions with it.
--
-- Sample output:
--
-- $ runghc examples/test.hs
-- Using image from local cache
-- Unpacking the rootfs
--
-- ---
-- You just created an Ubuntu container (release=trusty, arch=amd64, variant=default)
-- The default username/password is: ubuntu / ubuntu
-- To gain root privileges, please use sudo.
--
-- ping: unknown host github.com
-- ping: unknown host github.com
-- ping: unknown host github.com
-- ping: unknown host github.com
-- PING github.com (192.30.252.130) 56(84) bytes of data.
-- 64 bytes from github.com (192.30.252.130): icmp_seq=1 ttl=61 time=300 ms
--
-- --- github.com ping statistics ---
-- 1 packets transmitted, 1 received, 0% packet loss, time 0ms
-- rtt min/avg/max/mdev = 300.847/300.847/300.847/0.000 ms
-- Hello, world!
-- Container IPs:
-- - eth0
--  * 10.0.3.177
-- - lo
--  * 127.0.0.1
-- Container state: ContainerStopped
main :: IO ()
main = withContainer (Container "trusty" Nothing) $ do
  create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
  start False []
  wait ContainerRunning (-1)
  waitForNetwork
  attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
  dumpIPs
  stop
  dumpState
