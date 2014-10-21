# Monroe - Clojure nREPL client for Emacs

<p align="center">
<img src="https://raw.github.com/sanel/monroe/master/images/shot.png"
     alt="Monroe" title="Monroe screenshot">
</p>

Monroe is [nREPL](https://github.com/clojure/tools.nrepl) client for
Emacs, focused on simplicity and easy distribution primarily targeting
Clojure users.

If you are not familiar with nREPL, it is protocol for accessing
Clojure [REPL](http://en.wikipedia.org/wiki/Read-eval-print_loop) over
the network.

In short, Monroe aims to have:

* easy access to Clojure REPL via nREPL protocol
* simple installation without any dependencies, except Emacs
* REPL with colors and history support
* generic approach - you can use it with other languages than Clojure
* only REPL for interaction with shortcuts - no funny windows or buffers with errors,
  messages and other distractions

On other hand, Monroe is not:

* Clojure IDE like [Cider](https://github.com/clojure-emacs/cider)
* Kitchen sink that will do Clojure work for you

## Installation

Make sure you have
[clojure-mode.el](https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode.el)
installed first.

Download Monroe release (or **monroe.el** file directly from this
repository) and put in folder where Emacs can access it (usually
*$HOME/.emacs.d* or any folder listed in Emacs *load-path* variable).

In your
[Emacs init file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html),
put:

```el
(require 'monroe)
(add-hook 'clojure-mode-hook 'clojure-enable-monroe)
```

Then, in Emacs:

<kbd>M-x monroe [RET]</kbd>

and follow the question about nREPL server location and port.

## Keys and shortcuts

### Monroe shortcuts for code buffer

These shortcuts are valid from code buffer, where you edit Clojure
code (where *monroe-interaction-mode* is activated).

Keys                | Description
--------------------|----------------------------
<kbd>C-c C-c</kbd>  | Evaluate expression at point.
<kbd>C-c C-r</kbd>  | Evaluate region.
<kbd>C-c C-k</kbd>  | Evaluate buffer.
<kbd>C-c C-d</kbd>  | Describe symbol at point, showing documentation in REPL window.
<kbd>C-c C-n</kbd>  | Evaluate namespace.
<kbd>C-c C-b</kbd>  | Interrupt running job.

### Monroe shortcuts for REPL buffer

These shortcuts are valid in REPL buffer; also, most of the standard
*comint-mode* shortcuts should work without problems.

Keys                | Description
--------------------|----------------------------
<kbd>C-c C-c</kbd>  | Interrupt running job.

## Bug reports & patches

Feel free to report any issues you find or you have suggestions for improvements.
