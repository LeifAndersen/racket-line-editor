# Line editor for Racket

A simple line-editor, inspired by
[linenoise](https://github.com/antirez/linenoise), written for Racket.

## Installation

To install from a git checkout, change directory to the base of the
checkout and run

    $ raco pkg install --link -n line-editor

This will first install the package, and then attempt to compile it.
If the compilation fails at this point, or you make changes to the
package and wish to recompile it, run the following command instead of
trying to reinstall the package:

    $ raco setup line-editor
