# monroe - Minimal nrepl for Emacs

Monroe is [nREPL](https://github.com/clojure/tools.nrepl) client for
Emacs, focused on simplicity, easy distribution and small emulation of
Emacs inferior lisp.

If you are not familiar with nREPL, it is protocol for accessing
Clojure REPL over network.

## Installation

Just download official release, unpack it and put in folder where
Emacs can access it (usually *$HOME/.emacs.d* or any folder listed in
'load-path' variable).

In your
[Emacs init file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html),
put:

```
(require 'monroe)
(add-hook 'clojure-mode-hook 'clojure-enable-monroe)
```

Then, in Emacs:

```
M-x monroe
```

and follow the questions about nREPL server location and port.

## Bug reports & patches

Feel free to report any issues you find or you have suggestions for improvements.
