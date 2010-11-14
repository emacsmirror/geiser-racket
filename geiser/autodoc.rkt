;;; autodoc.rkt -- suport for autodoc echo

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun May 03, 2009 14:45

#lang racket

(provide autodoc update-signature-cache get-help)

(require geiser/utils geiser/modules geiser/locations scheme/help)

(define (get-help symbol mod)
  (with-handlers ([exn? (lambda (_)
                          (eval `(help ,symbol)))])
    (eval `(help ,symbol #:from ,(ensure-module-spec mod)))))

(define (autodoc ids)
  (if (not (list? ids))
      '()
      (map (lambda (id) (or (autodoc* id) (list id))) ids)))

(define (autodoc* id)
  (define (val)
    (with-handlers ([exn? (const "")])
      (format "~.a" (namespace-variable-value id))))
  (and
   (symbol? id)
   (let* ([loc (symbol-location* id)]
          [name (car loc)]
          [path (cdr loc)]
          [sgns (and path (find-signatures path name id))])
     (and sgns
          `(,id
            (name . ,name)
            (value . ,(if (list? sgns) "" (val)))
            (args ,@(if (list? sgns) (map format-signature sgns) '()))
            (module . ,(module-path-name->name path)))))))

(define (format-signature sign)
  (if (signature? sign)
      `((required ,@(signature-required sign))
        (optional ,@(signature-optional sign)
                  ,@(let ((rest (signature-rest sign)))
                      (if rest (list "...") '())))
        (key ,@(signature-keys sign)))
      '()))

(define signatures (make-hash))

(struct signature (required optional keys rest))

(define (find-signatures path name local-name)
  (let ([path (if (path? path) (path->string path) path)])
    (hash-ref! (hash-ref! signatures
                          path
                          (lambda () (parse-signatures path)))
               name
               (lambda () (infer-signatures local-name)))))

(define (parse-signatures path)
  (let ([result (make-hasheq)])
    (with-handlers ([exn? (lambda (e) result)])
      (with-input-from-file path
        (lambda ()
          (parameterize ([read-accept-reader #t])
            (let loop ([stx (read-syntax path)])
              (cond [(eof-object? stx) void]
                    [(syntax->datum stx) =>
                     (lambda (datum)
                       (parse-datum! datum result)
                       (loop (read-syntax path)))]
                    [else void]))))))
    result))

(define (parse-datum! datum store)
  (with-handlers ([exn? (lambda (_) void)])
    (match datum
      [`(module ,name ,lang (#%module-begin . ,forms))
       (for-each (lambda (f) (parse-datum! f store)) forms)]
      [`(module ,name ,lang . ,forms)
       (for-each (lambda (f) (parse-datum! f store)) forms)]
      [`(define ((,name . ,formals) . ,_) . ,_)
       (add-signature! name formals store)]
      [`(define (,name . ,formals) . ,_)
       (add-signature! name formals store)]
      [`(define ,name (lambda ,formals . ,_))
       (add-signature! name formals store)]
      [`(define ,name (case-lambda ,clauses ...))
       (for-each (lambda (c) (add-signature! name (car c) store))
                 (reverse clauses))]
      [`(,(or 'struct 'define-struct) ,name ,(? symbol? _)
         ,(list formals ...) . ,_)
       (add-signature! name formals store)]
      [`(,(or 'struct 'define-struct) ,name ,(list formals ...) . ,_)
       (add-signature! name formals store)]
      [`(define-for-syntax (,name . ,formals) . ,_)
       (add-signature! name formals store)]
      [`(define-for-syntax ,name (lambda ,formals . ,_))
       (add-signature! name formals store)]
      [`(define-syntax-rule (,name . ,formals) . ,_)
       (add-signature! name formals store)]
      [`(define-syntax ,name (syntax-rules ,specials . ,clauses))
       (for-each (lambda (c) (add-syntax-signature! name (cdar c) store))
                 (reverse clauses))]
      [`(define-syntax ,name (lambda ,_ (syntax-case ,_ . ,clauses)))
       (for-each (lambda (c) (add-syntax-signature! name (cdar c) store))
                 (reverse clauses))]
      [_ void])))

(define (add-signature! name formals store)
  (when (symbol? name)
    (hash-set! store
               name
               (cons (parse-formals formals)
                     (hash-ref store name '())))))

(define (add-syntax-signature! name formals store)
  (when (symbol? name)
    (hash-set! store
               name
               (cons (signature formals '() '() #f)
                     (hash-ref store name '())))))

(define (parse-formals formals)
  (let loop ([formals formals] [req '()] [opt '()] [keys '()])
    (cond [(null? formals)
           (signature (reverse req) (reverse opt) (reverse keys) #f)]
          [(symbol? formals)
           (signature (reverse req) (reverse opt) (reverse keys) formals)]
          [(pair? (car formals)) (loop (cdr formals)
                                       req
                                       (cons (car formals) opt)
                                       keys)]
          [(keyword? (car formals)) (let* ((kname (car formals))
                                           (arg-id (cadr formals))
                                           (name (if (pair? arg-id)
                                                     (list kname
                                                           (cadr arg-id))
                                                     (list kname))))
                                      (loop (cddr formals)
                                            req
                                            opt
                                            (cons name keys)))]
          [else (loop (cdr formals) (cons (car formals) req) opt keys)])))

(define (infer-signatures name)
  (with-handlers ([exn:fail:syntax? (const `(,(signature '(...) '() '() #f)))]
                  [exn:fail:contract:variable? (const #f)])
    (let ([v (namespace-variable-value name)])
      (if (procedure? v)
          (arity->signatures (procedure-arity v))
          'variable))))

(define (arity->signatures arity)
  (define (args count) (build-list count (const '_)))
  (define (arity->signature arity)
    (cond [(number? arity)
           (signature (args arity) '() '() #f)]
          [(arity-at-least? arity)
           (signature (args (arity-at-least-value arity)) '() '() 'rest)]))
  (define (conseq? lst)
    (cond [(< (length lst) 2) (number? (car lst))]
          [(and (number? (car lst))
                (number? (cadr lst))
                (eqv? (+ 1 (car lst)) (cadr lst)))
           (conseq? (cdr lst))]
          [else #f]))
  (cond [(and (list? arity) (conseq? arity))
         (let ((mi (apply min arity))
               (ma (apply max arity)))
           (list (signature (args mi) (args (- ma mi)) '() #f)))]
        [(list? arity) (map arity->signature arity)]
        [else (list (arity->signature arity))]))

(define (update-signature-cache path . form)
  (when (and (string? path)
             (or (null? form)
                 (and (list? (car form))
                      (not (null? (car form)))
                      (memq (caar form)
                            '(define-syntax-rule struct
                               define-syntax define set! define-struct)))))
    (hash-remove! signatures path)))
