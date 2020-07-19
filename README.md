# Racket and Geiser talk to each other

This package provides support for using
[Racket](http://racket-lang.org) in Emacs with
[Geiser](http://geiser.nongnu.org).

Provided geiser-core is installed in your system, if this package's
directory is in your load path, just add `(require 'geiser-racket)` to
your initialisation files and then `M-x run-racket` to start a REPL.
Files with the `.rkt` and `.ssl` extensions should be automatically
recognized as racket-flavoured geiser buffers.
