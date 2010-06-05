;;; enter.rkt -- custom module loaders

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 21:53

#lang racket/base

(require syntax/modcode
         (for-syntax scheme/base))

(provide get-namespace enter-module module-loader module-loaded?)

(define-struct mod (name timestamp depends))

(define loaded (make-hash))

(define (module-loaded? path)
  (with-handlers ((exn? (lambda (_) #f)))
    (let ((rp (module-path-index-resolve (module-path-index-join path #f))))
      (hash-has-key? loaded (resolved-module-path-name rp)))))

(define (enter-module mod)
  (dynamic-require mod #f)
  (check-latest mod))

(define (module-loader orig)
  (enter-load/use-compiled orig #f))

(define (notify re? path)
  (when re?
    (fprintf (current-error-port) " [re-loading ~a]\n" path)))

(define inhibit-eval (make-parameter #f))

(define (get-namespace mod)
  (parameterize ([inhibit-eval #t])
    (module->namespace mod)))

(define ((enter-load/use-compiled orig re?) path name)
  (when (inhibit-eval)
    (raise (make-exn:fail "namespace not found"
                          (current-continuation-marks))))
  (if name
      ;; Module load:
      (let ([code (get-module-code path "compiled" compile
                                   (lambda (ext loader?)
                                     (load-extension ext)
                                     #f)
                                   #:notify (lambda (chosen)
                                              (notify re? chosen)))]
            [path (normal-case-path
                   (simplify-path
                    (path->complete-path path
                                         (or (current-load-relative-directory)
                                             (current-directory)))))])
        ;; Record module timestamp and dependencies:
        (let ([mod (make-mod name
                             (get-timestamp path)
                             (if code
                                 (apply append
                                        (map cdr
                                             (module-compiled-imports code)))
                                 null))])
          (hash-set! loaded path mod))
        ;; Evaluate the module:
        (eval code))
      ;; Not a module:
      (begin
        (notify re? path)
        (orig path name))))

(define (get-timestamp path)
  (file-or-directory-modify-seconds path #f (lambda () -inf.0)))

(define (check-latest mod)
  (let ([mpi (module-path-index-join mod #f)]
        [done (make-hash)])
    (let loop ([mpi mpi])
      (let* ([rpath (module-path-index-resolve mpi)]
             [path (resolved-module-path-name rpath)])
        (when (path? path)
	  (let ([path (normal-case-path path)])
            (unless (hash-ref done path #f)
              (hash-set! done path #t)
              (let ([mod (hash-ref loaded path #f)])
                (when mod
                  (for-each loop (mod-depends mod))
                  (let ([ts (get-timestamp path)])
                    (when (ts . > . (mod-timestamp mod))
                      (let ([orig (current-load/use-compiled)])
                        (parameterize ([current-load/use-compiled
                                        (enter-load/use-compiled orig #f)]
                                       [current-module-declare-name rpath])
                          ((enter-load/use-compiled orig #t)
                           path
                           (mod-name mod)))))))))))))))

;;; enter.rkt ends here
