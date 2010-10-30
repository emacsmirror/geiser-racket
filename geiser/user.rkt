;;; user.rkt -- global bindings visible to geiser users

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 22:24

#lang racket/base

(provide enter!)

(require geiser/main geiser/enter geiser/eval (for-syntax racket/base))

(define top-namespace (current-namespace))

(define (enter! mod stx)
  (cond ((not mod) (current-namespace top-namespace))
        ((module-path? mod)
         (enter-module mod)
         (current-namespace (module->namespace mod)))
        (else (raise-syntax-error
               #f
               "not a valid module path, and not #f"
               stx
               mod))))

(define orig-loader (current-load/use-compiled))

(define orig-reader (current-prompt-read))

(define (geiser-eval)
  (define geiser-main (module->namespace 'geiser/main))
  (let* ((mod (read))
         (lang (read))
         (form (read)))
    (datum->syntax #f
                   (list 'quote
                         (cond ((equal? form '(unquote apply))
                                (let* ((proc (eval (read) geiser-main))
                                       (args (read)))
                                  ((geiser:eval lang) `(,proc ,@args) mod)))
                               (else ((geiser:eval lang) form mod)))))))

(define (geiser-read)
  (let ((form (orig-reader)))
    (syntax-case form ()
      ((uq cmd) (eq? 'unquote (syntax-e #'uq))
       (case (syntax-e #'cmd)
         ((enter) (enter! (read) #'cmd))
         ((geiser-eval) (geiser-eval))
         ((geiser-no-values) (datum->syntax #f (void)))
         (else form)))
      (_ form))))

(define (init)
  (compile-enforce-module-constants #f)
  (current-load/use-compiled (module-loader orig-loader))
  (current-prompt-read
   (compose (make-repl-reader geiser-read) current-namespace)))

(init)
