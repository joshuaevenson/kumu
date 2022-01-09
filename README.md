# kumu - Hawaiian for "basic"
_Small, familiar, fast_

Adapted from [clog](todo) but with a large number of enhancements for production use

- [x] no global variables, `kuvm` passed to all functions
- [x] arrow function expressions: `arg => expr, arg => { ... }`
- [x] arrow function blocks: `{ a, b => exp }, { a, b => { ... } }`
- [x] `break`, `continue` support
- [x] remove `print` statement
- [x] bug fix native call no frame adjustment
- [x] native call error handling
- [x] string `"\n\r\t`" escape, number `0x`, number `___._e_`
- [x] native class support
- [x] math class: `sin()`, `cos()`, `tan()`, `pi`, `imod()`
- [x] repl USE_READLINE
- [x] strings: `s.count`, `string.format(fmt,...)`
- [x] intern `count` string for speed
- [x] arrays: `x=[1,2,3]; y=arr.count; v=arr[1]; arr[2]=v;`
- [x] `arr.arrays: map(e => k)`, `arr.reduce(v0, { v,e => n });`
- [x] REPL print array
- [x] VC++ build warnings
- [x] tables: `t=table(); t.key=val; v=t.key; t.iter({k,v => ...});`
- [x] stack balance on native calls
- [x] `printf` take format string
- [x] format handle `%d`, `%g`, and `%x`
- [x] `string.format()` bug fix 
- [x] REPL `.gc` command
- [x] support for [nanovg](todo) and [nanovgMetal](todo)
