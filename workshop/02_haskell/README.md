# `rules_haskell` basics

## Generally useful links

Official, easily remembered website - [haskell.build](https://haskell.build/) that links to the other things below.

`rules_haskell` rules [documentation](https://release.api.haskell.build/)

[Introduction, core knowledge, and common use cases](https://rules-haskell.readthedocs.io/en/latest/)

## Getting started

### tl;dr
The easiest way is to run
```sh
curl https://haskell.build/start | sh
```
to get a barebones `rules_haskell` setup.

## Toolchains ([documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#picking-a-compiler))

### tl;dr
`rules_haskell` **does not** attempt to use a ghc from the environment by design, in order to increase hermeticity.

In other words, having a `ghc` globally installed is not required, and having one does not mean it will be used in builds.

To your `WORKSPACE`, add (already done by the startup script)
```python
rules_haskell_toolchains(
    version = "<version>",
)
```
where `<version>` is the `ghc` version you want to use for the project.

Only the platforms that have a “binary package” on the GHC download page are supported.

If you're on a Mac, you might need to set the `BAZEL_USE_CPP_ONLY_TOOLCHAIN` env variable to `1`.

### Overview
We use the universal "toolchains" mechanism that Bazel provides. This is how Bazel allows users (in this case you) to plug into code written by rule authors
(in this case `rules_haskell`) and specify which tools (e.g. `ghc`) should be used within the implementation of rules.

`rules_haskell` currently provides two options for getting `ghc`
1. let `rules_haskell` download a binary distribution of `ghc` (similar to what Stack does) ([documentation](https://release.api.haskell.build/haskell/toolchain.html#rules_haskell_toolchains))
2. use `nix` to provide `ghc` ([documentation](https://release.api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs))

These rules **register** a ghc that Bazel **can** use. The process that Bazel determines what ghc to **actually** use is called [toolchain resolution](https://docs.bazel.build/versions/main/toolchains.html#toolchain-resolution).
It selects a ghc that matches various criteria, e.g. what OS we're currently on.

Although you should not need to learn in-depth about platforms and toolchains, you can find [documentation for them here](https://docs.bazel.build/versions/main/platforms-intro.html).

## Most commonly used rules

### `haskell_library`([documentation](https://release.api.haskell.build/haskell/defs.html#haskell_library))

### `haskell_binary`([documentation](https://release.api.haskell.build/haskell/defs.html#haskell_binary))

### `haskell_test`([documentation](https://release.api.haskell.build/haskell/defs.html#haskell_test))

### `haskell_toolchain_library`([documentation](https://release.api.haskell.build/haskell/defs.html#haskell_toolchain_library))
There are some libraries that ship with ghc, e.g. `base`, `text`. You can make the "shipped with ghc version" directly available without using `stack_snapshot`
with this rule:
```python
haskell_toolchain_library(name = "base")
```
Note that this is done in the `BUILD.bazel` file, not in the `WORKSPACE` file.

We're using the `@stackage` provided libs here for consistency with what `gazelle_cabal` does.

## `haskell_module`s
With `haskell_library|test|binary`, changing one file from their `srcs` list means the entire compilation for the rule will be redone.

We know it's possible to do better than that - if we have this dependency graph:
```
A --> B --> C
```
changing `B` does not require a recompilation of `C`, but using `haskell_library` without `haskell_module`s will do so anyway.

The `haskell_module` rule is `rules_haskell`s solution to the problem.
It allows us to be more granular by specifying cross-module dependencies to Bazel, causing recompilation only when needed.

I recommend you check out [this blogpost](https://www.tweag.io/blog/2022-06-23-haskell-module/) about `haskell_module`
which already acts as a good introduction to the rule and build parallelism in `rules_haskell`.

Unfortunately, since this feature is still considered experimental, rendered documentation is not available online for it, but you can
still check it out in the source for `rules_haskell`([documentation](https://github.com/tweag/rules_haskell/blob/a7daf4993d23a358965452ab4cac82d6f47ed9b9/haskell/experimental/defs.bzl#L70))

## Adding dependencies

### Comparison with stack.yaml (and `cabal.project`?)

### Adding stackage dependencies
If you want to access stackage dependencies, you must first "make them available" (in a similar vein to what `stack` does with `stack.yaml`)
by listing them in the `packages` field of the `stack_snapshot` rule in your `WORKSPACE` file:
```python
stack_snapshot(
    name = "stackage",
    packages = [ "formatting", "text", ],
    ...
)
```

Having done so, they are now available as targets in the `stackage`(this being the `name` field of the `stack_snapshot` rule) external repository,
i.e. you can refer to them like so:
```python
haskell_binary(
    name = "app",
    srcs = ["app/Main.hs"],
    deps = [..., "@stackage//:formatting", "@stackage//:text", ...],
)
```

We can pick a specific version to use instead:
```python
stack_snapshot(
    name = "stackage",
    packages = [ "formatting-7.1.3", "text", ],
    ...
)
```

### Stackage snapshot pinning ([documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html?highlight=stack_snapshot_json#pinning))
Once we have a `stack_snapshot` invocation, we can `bazel run @stackage-unpinned//:pin` to generate a stackage snapshot json file to use as a pin for the version of our packages.

This has at least three benefits:
* speed up Bazel runs - without a pin, Bazel invokes Stack every time
* avoid network issues - stack does network access
* avoid versioning issues - publishing a revision on Hackage doesn't change the "version"

Note that if we change the version of a package we're using or add a new package we'll need to rerun the `:pin` target.


### Adding dependencies that are not on stackage, from a git repo, etc

See [vendoring-packages](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#vendoring-packages).

## Adding compiler flags in different places

### tl;dr
Add ghc options to the `ghcopts` field in your corresponding library/binary/test rule.

### Overview

A lot of the rules have the `ghcopts` attribute, for example:
* `haskell_library` has [`ghcopts`](https://release.api.haskell.build/haskell/defs.html#haskell_library-ghcopts), which applies ghc options when compiling this library
* `rules_haskell_toolchains` has [`ghcopts`](https://release.api.haskell.build/haskell/toolchain.html#rules_haskell_toolchains), which applies ghc options whenever you use
  the ghc registered with it


## Repl

### tl;dr
Run `bazel run <normal-target-label>@repl`, e.g. `bazel run //:mylib@repl`.

### Overview
There is a [`haskell_repl`](https://release.api.haskell.build/haskell/defs.html#haskell_repl) rule if you want to declare a target that launches `ghci`.
You will usually not need to do this as a lot of the targets, including `haskell_library`, `haskell_binary` and `haskell_test`, implicitly (via them being macros)
create repl targets.

We previously saw these in our `01_simple` project:
```sh
jovyan@2578e182d38e:~/workshop/01_simple$ bazel query '//...'
//:test-repl
//:test@repl
//:test
//:mylib-repl
//:mylib@repl
//:hello-repl
//:hello@repl
//:hello
//:mylib
//:base
```
They're labelled with `@repl`. If you `bazel run` them, you get dropped into a repl with that targets dependencies loaded:
```haskell
jovyan@2578e182d38e:~/workshop/01_simple$ bazel run //:mylib@repl
INFO: Analyzed target //:mylib@repl (32 packages loaded, 9013 targets configured).
INFO: Found 1 target...
Target //:mylib@repl up-to-date:
  bazel-bin/mylib@repl@repl
INFO: Elapsed time: 1.688s, Critical Path: 0.04s
INFO: 14 processes: 14 internal.
INFO: Running command line: bazel-bin/mylib@repl@repl
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling MyLib            ( MyLib.hs, interpreted )
Ok, one module loaded.
Ok, one module loaded.
Loaded GHCi configuration from /home/jovyan/.cache/bazel/_bazel_jovyan/a97e5522e7ee0e7091ad348df8243be8/execroot/simple/bazel-out/k8-fastbuild/bin/ghci-repl-script-mylib@repl
*MyLib> MyLib.say "pesho"
pesho
```
The `-repl` ones are the old naming convention and they do the same thing as the `@repl` variants - you can ignore them.

## HLS
The HLS story is currently not great. There is some documentation on setting it up, along with plans to improve it:
* Setup guide - https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#configuring-ide-integration-with-ghcide
* Work to improve - https://github.com/tweag/rules_haskell/issues/1489
