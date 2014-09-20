# monroe - Minimal nrepl for Emacs

Monroe is [nREPL](https://github.com/clojure/tools.nrepl) mode for
Emacs, focused on simplicity, easy distribution and seamless
integration with Emacs inferior lisp.

If you are not familiar with nREPL, it is protocol for accessing
Clojure REPL over network.

## Installation

Just download official release, unpack it and put in folder where
Emacs can access it (usually *$HOME/.emacs.d* or any folder listed in
'load-path' variable).

In Emacs run:

```
M-: (require 'monroe)
M-x monroe
```
and follow the questions.
