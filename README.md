lxc
===

[![Build Status](https://travis-ci.org/fizruk/lxc.svg?branch=master)](https://travis-ci.org/fizruk/lxc)

High level Haskell bindings to LXC (Linux containers).

The library provides Haskell LXC API, wrapping [bindings-lxc package](http://hackage.haskell.org/package/bindings-lxc). 

## Requirements

Before installation make sure you have LXC installed on your system with header files and static library.

Although there is `lxc-dev` package in standard Ubuntu repositories,
you might want to use `ppa:ubuntu-lxc/stable` repository instead:

```
$ sudo apt-get install software-properties-common python-software-properties
$ sudo add-apt-repository ppa:ubuntu-lxc/stable
$ sudo apt-get update
$ sudo apt-get install lxc-dev
```

## Installation

Get the latest stable version from Hackage:

```
$ cabal install lxc
```

or clone this repository:

```
$ git clone https://github.com/fizruk/lxc.git
$ cd lxc
$ cabal install
```

## Documentation

Haddock documentation is available at http://fizruk.github.io/lxc/docs/

## Usage

Most of container-related functions (e.g. `start`, `attach`, `destroy`) perform in a `LXC` monad.
To run `LXC a` computation you need to specify a container using `withContainer` function.
When working with a single container it might be handy to have an alias like this:

```haskell
let containerName = withContainer (Container "container-name" configPath)
```

You can start using Haskell LXC API bindings similar to a command line tool from GHCi:

```
$ ghci
>>> import System.LXC
>>> let trusty = withContainer (Container "trusty" Nothing)
>>> trusty $ create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
Using image from local cache
Unpacking the rootfs

---
You just created an Ubuntu container (release=trusty, arch=amd64, variant=default)
The default username/password is: ubuntu / ubuntu
To gain root privileges, please use sudo.

True
>>> trusty $ start False []
True
>>> trusty state
ContainerRunning
>>> trusty $ attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
Hello, world!
Just ExitSuccess
>>> trusty stop
True
>>> Just trustySnapC <- trusty $ clone (Just "trusty-snap") Nothing [CloneSnapshot] Nothing Nothing Nothing []
>>> let trustySnap = withContainer trustySnapC
>>> trustySnap $ start False []
True
>>> trustySnap getInterfaces
["eth0","lo"]
>>> trustySnap $ getIPs "eth0" "inet" 0
["10.0.3.135"]
>>> trustySnap $ shutdown (-1)
True
>>> trustySnap state
ContainerStopped
```

For more examples, please see `examples/` folder.

## Contributing

Contributions and bug reports are welcome!

Please feel free to contact me via GitHub or on the #haskell IRC channel on irc.freenode.net.

-Nickolay Kudasov
