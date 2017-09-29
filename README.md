# MyScheme
A simple Scheme compiler &amp; virtual machine

# Features

- [x] First-class procedure
- [x] Proper tail call
- [x] `Eval` function
- [x] Quasiquotation
- [x] First-class continuation (`call/cc`)
- [x] Hygienic macro (`syntax-rules`)
- [x] Local macro (local `define-syntax`, `let-syntax` and `letrec-syntax`)
- [ ] Multiple values

# Run

- ChezScheme：
```
$ cd src
$ scheme --script main.scm filename...
```

# Bibliography

- *Lisp in Small Pieces*, Christian Queinnec
- *Macro-by-Example*, Eugene Kohlbecker, et al
- *Macros that work*, William Clinger, et al