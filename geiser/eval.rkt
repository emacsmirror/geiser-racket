;;; eval.rkt -- evaluation

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Apr 26, 2009 00:44

#lang racket

(provide eval-in
         compile-in
         load-file
         compile-file
         macroexpand
         make-repl-reader)

(require scheme/enter geiser/modules geiser/autodoc)

(define last-result (void))

(define namespace->module-name
  (compose module-path-name->name namespace->module-path-name))

(define last-namespace (make-parameter (current-namespace)))

(define (exn-key e)
  (vector-ref (struct->vector e) 0))

(define current-marks (make-parameter (current-continuation-marks)))

(define (get-real-context e)
  (let ([ec (continuation-mark-set->context (exn-continuation-marks e))]
        [cc (continuation-mark-set->context (current-marks))])
    (filter-not (lambda (c) (member c cc)) ec)))

(define (display-exn-context c)
  (define (maybe-display p x) (when x (display p) (display x)) x)
  (when (and (pair? c) (cdr c))
    (let ([sloc (cdr c)])
      (and (maybe-display "" (srcloc-source sloc))
           (maybe-display ":" (srcloc-line sloc))
           (maybe-display ":" (srcloc-column sloc)))
      (maybe-display ": " (car c))
      (newline))))

(define (set-last-error e)
  (set! last-result `((error (key . ,(exn-key e)))))
  (display (exn-message e))
  (newline) (newline)
  (for-each display-exn-context (get-real-context e)))

(define (write-value v)
  (with-output-to-string
    (lambda () (write v))))

(define (set-last-result . vs)
  (set! last-result `((result  ,@(map write-value vs)))))

(define (call-with-result thunk)
  (set-last-result (void))
  (let ([output
         (with-output-to-string
           (lambda ()
             (parameterize ([current-marks (current-continuation-marks)])
               (with-handlers ([exn? set-last-error])
                 (call-with-values thunk set-last-result)))))])
    (append last-result `((output . ,output)))))

(define (eval-in form spec lang)
  (call-with-result
   (lambda ()
     (update-signature-cache spec form)
     (eval form (module-spec->namespace spec lang)))))

(define compile-in eval-in)

(define (load-file file)
  (call-with-result
   (lambda ()
     (load-module file (current-output-port) (last-namespace))
     (update-signature-cache file))))

(define compile-file load-file)

(define (macroexpand form . all)
  (let ([all (and (not (null? all)) (car all))])
    (with-output-to-string
      (lambda ()
        (pretty-print (syntax->datum ((if all expand expand-once) form)))))))

(define (make-repl-reader builtin-reader)
  (lambda (ns)
    (last-namespace ns)
    (printf "racket@~a" (namespace->module-name ns))
    (builtin-reader)))

;;; eval.rkt ends here
