;;; server.rkt -- REPL server

;; Copyright (c) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Nov 06, 2010 15:15

#lang racket/base

(require geiser/user mzlib/thread)
(provide run-geiser-server start-geiser)

(define (run-geiser-server port enforce-module-constants)
  (run-server port
              (lambda (in out)
                (run-geiser-repl in out enforce-module-constants))
              #f))

(define (start-geiser (port 1969) (enforce-module-constants #f))
  (thread (lambda () (run-geiser-server port enforce-module-constants))))
