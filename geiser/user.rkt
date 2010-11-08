;;; user.rkt -- global bindings visible to geiser users

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 22:24

#lang racket/base

(provide init-geiser-repl run-geiser-repl enter!)

(require (for-syntax racket/base)
	 geiser/main geiser/enter geiser/eval geiser/modules)

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
(define geiser-loader (module-loader orig-loader))

(define geiser-send-null (make-parameter #f))

(define (geiser-eval)
  (define geiser-main (module->namespace 'geiser/main))
  (geiser-send-null #t)
  (let* ([mod (read)]
         [lang (read)]
         [form (read)])
    (datum->syntax #f
                   (list 'quote
                         (cond [(equal? form '(unquote apply))
                                (let* ([proc (eval (read) geiser-main)]
                                       [args (read)])
                                  ((geiser:eval lang) `(,proc ,@args) mod))]
                               [else ((geiser:eval lang) form mod)])))))

(define (geiser-read)
  (if (geiser-send-null)
      (begin (geiser-send-null #f)
	     (write-char #\nul))
      (printf "racket@~a> " (namespace->module-name (current-namespace))))
  (flush-output)
  (let* ([in (current-input-port)]
	 [form ((current-read-interaction) (object-name in) in)])
    (syntax-case form ()
      [(uq cmd) (eq? 'unquote (syntax-e #'uq))
       (case (syntax-e #'cmd)
         ((enter) (enter! (read) #'cmd))
         ((geiser-eval) (geiser-eval))
         ((geiser-no-values) (datum->syntax #f (void)))
         (else form))]
      [_ form])))

(define geiser-prompt-read (make-repl-reader geiser-read))

(define (init-geiser-repl)
  (compile-enforce-module-constants #f)
  (current-load/use-compiled geiser-loader)
  (current-prompt-read geiser-prompt-read))

(define (run-geiser-repl in out enforce-module-constants)
  (parameterize [(compile-enforce-module-constants enforce-module-constants)
                 (current-input-port in)
                 (current-output-port out)
                 (current-error-port out)
                 (current-load/use-compiled geiser-loader)
                 (current-prompt-read geiser-prompt-read)]
    (read-eval-print-loop)))
