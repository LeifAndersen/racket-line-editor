#lang info
(define collection "line-editor")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/line-editor.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(leif))

(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"))
