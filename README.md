# Kaleidoscope

Implementation of [Kaleidoscope](http://llvm.org/docs/tutorial/OCamlLangImpl1.html) in OCaml without Camlp4.

---

## setup

```
$ brew install llvm cmake
$ opam install llvm ctypes-foreign jbuilder
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
    - 更新不够及时的文档
    - 直接去翻 ~/.opam/system/lib/llvm 下面的 mli 文件更准确
- 新版的 LLVM 默认开启了 constant folding optimisation

---

## ch4

- [LLVM passes](http://llvm.org/docs/Passes.html)
- [how to write a pass](http://llvm.org/docs/WritingAnLLVMPass.html)
- 这个教程本身非常旧了，代码和新的库对应不上……
    - `Llvm_target.DataLayout.add_to_pass_manager` 已经被删掉了，不知道 `Llvm_executionengine.data_layout` 得到的 layout 要怎么用
    - `Llvm_executionengine.run_function` 也不存在，只有 `Llvm_executionengine.add_module`
    - 改写了一些，能执行一些优化，不过看起来好像效果比较差的样子
    - 没有使用 execution engine，所以都称不上 jit 吧。新版不知道怎么针对函数做 jit。😂
