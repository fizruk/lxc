0.3.1.1
---
* loose `mtl` and `transformers` dependencies for `lxc` to play nice with other libraries

0.3.1
---
* added `snapshotDestroy` to `System.LXC.Container`
* minor documentation improvements

0.3
---
* introduced `LXC` monad
* `transformers` and `mtl` added to dependencies
* convert container-related functions to use `LXC` monad
* `listContainers` functions now return `[Container]` instead of `[(String, Ptr C'lxc_container)]`
* removed `mkContainer` function
* remove `getRef` and `dropRef` from `System.LXC.Container`
* `Container` is now pure Haskell data structure
* added `examples/`
* `Internal` gains `C'lxc_container` marshalling helpers (e.g. `withC'lxc_container`)
* fixed potential segfaults in `snapshotList` and `list*Containers`

0.2
---
* LXC errors through `getLastError` function
* `getDaemonize` function
* `start` function fixed (and changed type)
* `Show` instance for `BDevSpecs`
* updated documentation (haddock and README)

0.1.1
---
* fixed package (`bindings-lxc` dependency)
* exposed `System.LXC.Internal.AttachOptions` module
