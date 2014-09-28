module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class

import System.Exit
import System.LXC

dumpState :: LXC ()
dumpState = do
  s <- state
  liftIO $ do
    putStr "Container state: "
    print s

dumpInterfaces :: LXC ()
dumpInterfaces = do
  ifs <- getInterfaces
  liftIO $ putStrLn "Container IPs:"
  forM_ ifs $ \i -> do
    liftIO $ do
      putStr "- "
      print i
    ips <- getIPs i "inet" (-1)
    liftIO $ do
      forM_ ips $ \ip -> do
        putStr "  * "
        print ip

waitForNetwork :: LXC ()
waitForNetwork = do
  ret <- attachRunWait defaultAttachOptions "ping" ["ping", "-c", "1", "github.com"]
  case ret of
    Just ExitSuccess -> return ()
    _ -> do
      liftIO $ threadDelay (10^6)  -- sleep 1 sec
      waitForNetwork

main :: IO ()
main = withContainer (Container "trusty" Nothing) $ do
  create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
  start False []
  wait ContainerRunning (-1)
  waitForNetwork
  attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
  dumpInterfaces
  stop
  dumpState
