# MyScheme
A simple Scheme compiler &amp; virtual machine

# Features

- First-class procedure
- Proper tail call
- `Eval` function
- Quasiquotation
- First-class continuation (`call/cc`)
- Hygienic macro (`syntax-rules`)

# TODO
- Local macros
- Multiple values

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