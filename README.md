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
    - 修复了部分代码，能进行 optimization
    - JIT 没跑起来。`Llvm_executionengine.run_function` 这个 API 都已经删除了。没 LLVM 基础，API 文档看得很懵。

---

## ch5

- 在 LLVM IR 里实现分支跳转
- Φ (Phi) function

---

## ch6

- 感觉这章的代码处理有问题，按自己理解改了
- 定义的时候，加上 `unary / binary`，比如 `unary !`
- 处理的时候，binary 额外需要在 precedence 里加上优先级
- 调用的时候，把符号修改成函数，`!x` 变成 `unary! (x)`

---

## ch7

- 在 LLVM IR 里实现赋值
- stack variable

---

## summary

- 总体感觉，这个教程的帮助不大……
- parser 的部分，这里的场景比较简单，代码风格也不太好
- codegen 的部分，感觉对熟悉 LLVM IR 帮助不大
- 程序语义方面完全没涉及
