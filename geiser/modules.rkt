;;; modules.rkt -- module metadata

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed May 06, 2009 02:35

#lang racket

(provide load-module
         ensure-module-spec
         module-spec->namespace
         namespace->module-path-name
         module-path-name->name
         module-spec->path-name
         module-list
         module-exports)

(require srfi/13 syntax/modresolve syntax/modcode geiser/enter)

(define (ensure-module-spec spec)
  (cond [(symbol? spec) spec]
        [(not (string? spec)) #f]
        [else `(file ,spec)]))

(define (module-spec->namespace spec (lang #f) (current #t))
  (define (try-lang)
    (and lang
         (with-handlers ([exn? (const #f)])
           (load-module lang #f (current-namespace))
           (module->namespace lang))))
  (or (get-namespace spec)
      (try-lang)
      (and current (current-namespace))))

(define nowhere (open-output-nowhere))

(define (load-module spec (port #f) (ns #f))
  (parameterize ([current-error-port (or port nowhere)])
    (enter-module (ensure-module-spec spec))
    (when (namespace? ns)
      (current-namespace ns))))

(define (namespace->module-path-name ns)
  (let ([rmp (variable-reference->resolved-module-path
              (eval '(#%variable-reference) ns))])
    (and (resolved-module-path? rmp)
         (resolved-module-path-name rmp))))

(define (module-spec->path-name spec)
  (and (symbol? spec)
       (or (get-path spec)
           (register-path spec
                          (namespace->module-path-name
                           (module-spec->namespace spec) #f #f)))))

(define (module-path-name->name path)
  (cond [(path? path)
         (let* ([path (path->string path)]
                [cpaths (map (compose path->string path->directory-path)
                             (current-library-collection-paths))]
                [prefix-len (lambda (p)
                              (let ((pl (string-length p)))
                                (if (= pl (string-prefix-length p path))
                                    pl
                                    0)))]
                [lens (map prefix-len cpaths)]
                [real-path (substring path (apply max lens))])
           (if (absolute-path? real-path)
               (call-with-values (lambda () (split-path path))
                 (lambda (_ basename __) (path->string basename)))
               (regexp-replace "\\.[^./]*$" real-path "")))]
        [(eq? path '#%kernel) "(kernel)"]
        [(string? path) path]
        [(symbol? path) (symbol->string path)]
        [else ""]))

(define (skippable-dir? path)
  (call-with-values (lambda () (split-path path))
    (lambda (_ basename __)
      (member (path->string basename) '(".svn" "compiled")))))

(define path->symbol (compose string->symbol path->string))

(define (path->entry path)
  (let ([ext (filename-extension path)])
    (and ext
         (or (bytes=? ext #"rkt") (bytes=? ext #"ss"))
         (not (bytes=? (bytes-append #"main" ext) (path->bytes path)))
         (let* ([path (path->string path)]
                [len (- (string-length path) (bytes-length ext) 1)])
           (substring path 0 len)))))

(define (visit-module-path path kind acc)
  (define (register e p)
    (register-path (string->symbol e) (build-path (current-directory) p))
    (cons e acc))
  (define (find-main ext)
    (let ([m (build-path path (string-append "main." ext))])
      (and (file-exists? m) m)))
  (case kind
    [(file) (let ([entry (path->entry path)])
              (if (not entry) acc (register entry path)))]
    [(dir) (cond [(skippable-dir? path) (values acc #f)]
                 [(or (find-main "rkt") (find-main "ss")) =>
                  (curry register (path->string path))]
                 [else acc])]
    [else acc]))

(define (find-modules path acc)
  (if (directory-exists? path)
      (parameterize ([current-directory path])
        (fold-files visit-module-path acc))
      acc))

(define (known-modules)
  (sort (foldl find-modules '() (current-library-collection-paths)) string<?))

(define registered (make-hash))

(define (get-path mod) (hash-ref registered mod #f))

(define (register-path mod path)
  (hash-set! registered mod path)
  path)

(define module-cache #f)

(define (update-module-cache)
  (when (not module-cache) (set! module-cache (known-modules))))

(define (module-list)
  (update-module-cache)
  module-cache)

(define (module-exports mod)
  (define (value id) (dynamic-require mod id (const #f)))
  (define (contracted id)
    (let ([v (value id)])
      (if (has-contract? v)
          (cons id (contract-name (value-contract v)))
          id)))
  (define (extract-ids ls)
    (append-map (lambda (idls)
                  (map car (cdr idls)))
                ls))
  (define (classify-ids ids)
    (let loop ([ids ids] [procs '()] [vars '()])
      (cond [(null? ids)
             `((procs ,@(map contracted (reverse procs)))
               (vars ,@(map contracted (reverse vars))))]
            [(procedure? (value (car ids)))
             (loop (cdr ids) (cons (car ids) procs) vars)]
            [else (loop (cdr ids) procs (cons (car ids) vars))])))
  (let-values ([(reg syn)
                (module-compiled-exports
                 (get-module-code (resolve-module-path mod #f)))])
    (let ([syn (map contracted (extract-ids syn))]
          [reg (extract-ids reg)])
      `((syntax ,@syn) ,@(classify-ids reg)))))

(define (startup)
 (thread update-module-cache)
 (void))

(startup)

;;; modules.rkt ends here
