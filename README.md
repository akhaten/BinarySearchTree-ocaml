# BinarySearchTree-ocaml

A simple modular implementation of binary search trees in OCaml.

## Using

The project can be compiled and tested using [dune](https://dune.build/), the official build system for OCaml.

**To build the project :**

```
dune build
```

**To execute unit tests :**

```
dune exec test
```

**To generate the documentation :**

```
dune build @doc
```

the documentation can then be found in `_build/default/_doc/_html`

**To load the library inside utop :**

```
dune utop
```

and then type `open Bstree;;`