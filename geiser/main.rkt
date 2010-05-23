;;; main.rkt -- exported interface for emacs

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 21:14

#lang racket/base

(provide geiser:eval
         geiser:compile
         geiser:load-file
         geiser:compile-file
         geiser:macroexpand
         geiser:completions
         geiser:module-completions
         geiser:symbol-location
         geiser:module-location
         geiser:module-exports
         geiser:autodoc
         geiser:help)

(require geiser/eval
         geiser/modules
         geiser/completions
         geiser/locations
         geiser/autodoc)

(define (geiser:eval lang)
  (lambda (form spec) (eval-in form spec lang)))
(define geiser:compile compile-in)
(define geiser:load-file load-file)
(define geiser:compile-file compile-file)
(define geiser:autodoc autodoc)
(define geiser:help get-help)
(define geiser:completions symbol-completions)
(define geiser:module-completions module-completions)
(define geiser:symbol-location symbol-location)
(define geiser:module-location module-location)
(define geiser:module-exports module-exports)
(define geiser:macroexpand macroexpand)

;;; main.rkt ends here
