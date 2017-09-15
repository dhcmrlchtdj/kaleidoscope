# Camlp4 version

```
$ opam install llvm ctypes-foreign
$ ocamlbuild toy.byte
$ ocamlbuild -use-ocamlfind toy.byte -package llvm -package llvm.executionengine
```
