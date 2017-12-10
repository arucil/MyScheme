# MyScheme

This is a Scheme interpreter written in Scheme, composed of a compiler and a virtual machine.  

# Features

Most R5RS features are implemented.

- [x] First-class procedure
- [x] Proper tail call
- [x] `Eval` function
- [x] Quasiquotation
- [x] First-class continuation (`call/cc`)
- [x] Hygienic macro (`syntax-rules`)
- [x] Local macro (local `define-syntax`, `let-syntax` and `letrec-syntax`)
- [x] Multiple values

# Run

## Using ChezScheme

Run Scheme source files:

```bash
$ cd src && scheme --script main.scm filename...
```

or play with REPL:  

```bash
$ cd src && scheme

# in ChezScheme ...
> (load "runtime.scm") ;; load REPL facility
> (repl)
;; starting MyScheme REPL ...
myscheme> 
```

# Bibliography

- *Lisp in Small Pieces*, Christian Queinnec
- *Macro-by-Example*, Eugene Kohlbecker, et al
- *Macros that work*, William Clinger, et al