# Kaleidoscope

Implementation of [Kaleidoscope](http://llvm.org/docs/tutorial/OCamlLangImpl1.html) in OCaml without Camlp4.

---

## setup

```
$ brew install llvm cmake
$ opam install llvm jbuilder ctypes-foreign
```

---

## ch2

- 到 ch2 都只是 lexer 和 parser
    - 没有 interpreter 或者 compiler （之后也不会有
    - ch3 会写 codegen
- ch2 主要就是 recursive descent parsing
    - 用 PEG 的话会不会更简单些呢
- 中间解析 binary 用上了 Operator-Precedence Parsing
    - 具体点应该是 precedence climbing method
    - 不过，个人感觉还是 Shunting Yard 最直观

---

## ch3

- [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)
- [OCaml LLVM bindings](https://llvm.moe/)
- 好像 LLVM 默认开启了 constant folding optimisations，`4+5` 这种直接优化了
