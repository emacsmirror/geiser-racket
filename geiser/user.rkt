;;; user.rkt -- global bindings visible to geiser users

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 22:24

#lang racket/base

(provide init-geiser-repl run-geiser-server start-geiser)

(require (for-syntax racket/base)
         mzlib/thread
         racket/tcp
         geiser
         geiser/enter
         geiser/eval
         geiser/modules)

(define top-namespace (current-namespace))

(define (enter! mod stx)
  (cond [(not mod) (current-namespace top-namespace)]
        [(module-path? mod)
         (enter-module mod)
         (current-namespace (module->namespace mod))]
        [else (raise-syntax-error
               #f
               "not a valid module path, and not #f"
               stx
               mod)]))

(define orig-loader (current-load/use-compiled))
(define geiser-loader (module-loader orig-loader))

(define (geiser-eval)
  (define geiser-main (module->namespace 'geiser))
  (let* ([mod (read)]
         [lang (read)]
         [form (read)])
    (datum->syntax #f
                   (list 'quote
                         (cond [(equal? form '(unquote apply))
                                (let* ([proc (eval (read) geiser-main)]
                                       [args (read)])
                                  (eval-in `(,proc ,@args) mod lang))]
                               [else ((geiser:eval lang) form mod)])))))

(define ((geiser-read prompt))
  (prompt)
  (flush-output)
  (let* ([in (current-input-port)]
	 [form ((current-read-interaction) (object-name in) in)])
    (syntax-case form ()
      [(uq cmd) (eq? 'unquote (syntax-e #'uq))
       (case (syntax-e #'cmd)
         [(start-geiser) (datum->syntax #f `(list 'port ,(start-geiser)))]
         [(enter) (enter! (read) #'cmd)]
         [(geiser-eval) (geiser-eval)]
         [(geiser-no-values) (datum->syntax #f (void))]
         [else form])]
      [_ form])))

(define geiser-prompt
  (lambda ()
    (printf "racket@~a> " (namespace->module-name (current-namespace)))))

(define (geiser-prompt-read prompt)
  (make-repl-reader (geiser-read prompt)))

(define (init-geiser-repl)
  (compile-enforce-module-constants #f)
  (current-load/use-compiled geiser-loader)
  (current-prompt-read (geiser-prompt-read geiser-prompt)))

(define (run-geiser-repl in out enforce-module-constants)
  (parameterize [(compile-enforce-module-constants enforce-module-constants)
                 (current-input-port in)
                 (current-output-port out)
                 (current-error-port out)
                 (current-load/use-compiled geiser-loader)
                 (current-prompt-read (geiser-prompt-read geiser-prompt))]
    (read-eval-print-loop)))

(define server-channel (make-channel))

(define (run-geiser-server port enforce-module-constants)
  (run-server port
              (lambda (in out)
                (run-geiser-repl in out enforce-module-constants))
              #f
              void
              (lambda (p _ __)
                (let ([lsner (tcp-listen p)])
                  (let-values ([(_ p __ ___) (tcp-addresses lsner #t)])
                    (channel-put server-channel p)
                    lsner)))))

(define (start-geiser (port 0) (enforce-module-constants #f))
  (thread (lambda () (run-geiser-server port enforce-module-constants)))
  (channel-get server-channel))
