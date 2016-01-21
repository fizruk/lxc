0.3.2
---
* Relax upper bound for `base`
* Fix warnings

0.3.1.1
---
* Loose `mtl` and `transformers` dependencies for `lxc` to play nice with other libraries

0.3.1
---
* Add `snapshotDestroy` to `System.LXC.Container`
* Improve documentation slightly

0.3
---
* Introduce `LXC` monad
* Add `transformers` and `mtl` dependencies
* Change container-related functions to use `LXC` monad
* Change `listContainers` functions to return `[Container]` instead of `[(String, Ptr C'lxc_container)]`
* Remove `mkContainer` function
* Remove `getRef` and `dropRef` from `System.LXC.Container`
* Make `Container` a pure Haskell data structure
* Add `examples/`
* Add `C'lxc_container` marshalling helpers (e.g. `withC'lxc_container`) to `Internal`
* Fix potential segfaults in `snapshotList` and `list*Containers`

0.2
---
* Handle LXC errors through `getLastError` function
* Add `getDaemonize` function
* Fix `start` function (and changed type)
* Add `Show` instance for `BDevSpecs`
* Update documentation (haddock and README)

0.1.1
---
* Fix `bindings-lxc` dependency
* Expose `System.LXC.Internal.AttachOptions` module
