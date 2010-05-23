;;; user.rkt -- global bindings visible to geiser users

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 22:24

#lang racket/base

(provide enter!)

(require geiser/enter geiser/eval (for-syntax scheme/base))

(define-syntax (enter! stx)
  (syntax-case stx ()
    [(enter! mod)
     (if (or (not (syntax-e #'mod))
             (module-path? (syntax->datum #'mod)))
         #'(do-enter! 'mod)
         (raise-syntax-error
          #f
          "not a valid module path, and not #f"
          stx
          #'mod))]
    [_ (raise-syntax-error
        #f
        "bad syntax; should be `(enter! <module-path-or-#f>)'"
        stx)]))

(define orig-namespace (current-namespace))

(define (do-enter! mod)
  (if mod
      (begin
        (enter-module mod)
        (let ([ns (module->namespace mod)])
          (current-namespace ns)
          (namespace-require 'geiser/user)))
      (current-namespace orig-namespace)))


(define orig-loader (current-load/use-compiled))

(define (init)
  (compile-enforce-module-constants #f)
  (current-load/use-compiled (module-loader orig-loader))
  (current-prompt-read (compose (make-repl-reader (current-prompt-read))
                                current-namespace)))

(init)

;;; user.rkt ends here
