# `gazelle`

Gazelle is a tool to automate writing `BUILD` files (and some of `WORKSPACE`), based on already existing information such as
* imports in source files
* project files (e.g. `*.cabal`)
.

It's extremely useful, as
* Bazel doesn't allow dynamic target discovery, hence targets need to be declared in BUILD files before we run Bazel
* When we want more granular targets (e.g. module level), writing them all out becomes a pain.
  This is big issue in practice, as we almost always want more granularity, to parallelize builds and avoid recompilation when possible.

Other than that, it often supports additional cleanup, e.g. deleting a rule based on when a component in a "project file" is deleted.

Gazelle supports, and was originally written for, golang out of the box. Additionally, it is easily extensible, hence `gazelle_cabal` and `gazelle_haskell_modules` now exist too.

Its setup and usage is documented in [the github readme](https://github.com/bazelbuild/bazel-gazelle/blob/master/README.rst).
There is also a [blogpost by Tweag which covers golang usage](https://www.tweag.io/blog/2021-09-08-rules_go-gazelle/).

## `gazelle_cabal`

The `gazelle_cabal` extension deals with reading cabal files and generating rules for the different components in there.

I suggest you check out [the documentation](https://github.com/tweag/gazelle_cabal/blob/main/README.md), as it is relatively complete and has a good "getting started" section.

## `gazelle_haskell_modules`
The `gazelle_cabal` extension deals with splitting up pre-existing `haskell_library|test|binary` rules to instead be `haskell_module` based. As such, it works well
in tandem with `gazelle_cabal`:
```
       gazelle_cabal                  gazelle_haskell_modules
.cabal -------------> haskell_library -----------------------> haskell_library+haskell_module(s)
```
It also has a bunch of other convenient features (e.g. file autodetection ala hpack, deleting modules whose underlying sources are deleted).

I suggest you check out [the documentation](https://github.com/tweag/gazelle_haskell_modules/blob/main/README.md), as it is relatively complete and has a good "getting started" section.
