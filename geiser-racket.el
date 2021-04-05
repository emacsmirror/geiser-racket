;;; geiser-racket.el --- Support for Racket in Geiser  -*- lexical-binding: t; -*-

;; Author: Jose Antonio Ortega Ruiz (jao@gnu.org)
;; Maintainer: Jose Antonio Ortega Ruiz (jao@gnu.org)
;; Keywords: languages, racket, scheme, geiser
;; Homepage: https://gitlab.com/emacs-geiser/racket
;; Package-Requires: ((emacs "26.1") (geiser "0.12"))
;; SPDX-License-Identifier: BSD-3-Clause
;; Version: 1.0

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2020, 2021 Jose Antonio Ortega Ruiz


;;; Commentary:

;; This package extends the `geiser' core package to support the
;; Racket "scheme" implementation.


;;; Code:

(require 'geiser-impl)
(require 'geiser-edit)
(require 'geiser-doc)
(require 'geiser-eval)
(require 'geiser-image)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser)

(require 'compile)

(eval-when-compile (require 'cl-lib))


;;; Customization:

(defgroup geiser-racket nil
  "Customization for Geiser's Racket flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-racket-binary
    (cond ((eq system-type 'windows-nt) "Racket.exe")
          (t "racket"))
  "Name to use to call the racket executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-gracket-binary
    (cond ((eq system-type 'windows-nt) "GRacket-text.exe")
          (t "gracket-text"))
  "Name to use to call the gracket executable when starting a REPL.
This executable is used by `run-gracket', and, if
`geiser-racket-use-gracket-p' is set to t, by `run-racket'."
  :type '(choice string (repeat string))
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-collects nil
  "A list of paths to be added to racket's collection directories."
  :type '(repeat file)
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-init-file "~/.racket-geiser"
  "Initialization file with user code for the racket REPL."
  :type 'string
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-use-gracket-p nil
  "Whether to use the gracket binary to start Racket REPLs."
  :type 'boolean
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-extra-keywords
    '("provide" "require" "unless" "when" "with-handlers")
  "Extra keywords highlighted in Racket buffers."
  :type '(repeat string)
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-case-sensitive-p t
  "Non-nil means keyword highlighting is case-sensitive."
  :type 'boolean
  :group 'geiser-racket)


;;; REPL support:

(defun geiser-racket--real-binary ()
  "Decide what racket binary to use."
  (if geiser-racket-use-gracket-p
      geiser-racket-gracket-binary
    geiser-racket-binary))

(defun geiser-racket--binary ()
  "Combine real binary with any given flags."
  (let ((binary (geiser-racket--real-binary)))
    (if (listp binary) (car binary) binary)))

(defvar geiser-racket-scheme-dir
  (expand-file-name "src" (file-name-directory load-file-name))
  "Directory where the Racket geiser package is installed.")

(defun geiser-racket--parameters ()
  "Return a list with all parameters needed to start racket.
This function uses `geiser-racket-init-file' if it exists."
  (let ((init-file (and (stringp geiser-racket-init-file)
                        (expand-file-name geiser-racket-init-file)))
        (binary (geiser-racket--real-binary))
        (rackdir geiser-racket-scheme-dir))
    `("-i" "-q" "-S" ,rackdir
      ,@(apply 'append (mapcar (lambda (p) (list "-S" p))
                               geiser-racket-collects))
      ,@(and (listp binary) (cdr binary))
      ,@(and init-file (file-readable-p init-file) (list "-f" init-file))
      "-f" ,(expand-file-name "geiser/startup.rkt" rackdir))))

(defconst geiser-racket--prompt-regexp "\\(mzscheme\\|racket\\)@[^ ]*> ")


;;; Remote REPLs

;;;###autoload
(defun geiser-racket-connect ()
  "Start a Racket REPL connected to a remote process.

The remote process needs to be running a REPL server started
using start-geiser, a procedure in the geiser/server module."
  (interactive)
  (geiser-connect 'racket))



;;; Evaluation support:

(defconst geiser-racket--module-re
  "^(module[+*]? +\\([^ ]+\\)\\W+\\([^ ]+\\)?")

(defun geiser-racket--explicit-module ()
  "Detect an explicit racket module declaration, if any."
  (save-excursion
    (geiser-syntax--pop-to-top)
    (and (looking-at geiser-racket--module-re)
         (let ((mod (match-string-no-properties 1))
               (lang (match-string-no-properties 2)))
           (cons (geiser-syntax--form-from-string mod)
                 (geiser-syntax--form-from-string lang))))))

(defun geiser-racket--language ()
  "Detect a #lang stanza, if any."
  (or (cdr (geiser-racket--explicit-module))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#lang +\\([^ ]+\\)" nil t)
            (geiser-syntax--form-from-string (match-string-no-properties 1))))
      "#f"))

(defun geiser-racket--implicit-module (&optional pos)
  "Detect the module of a #lang stanza, if any, returning position if POS is t."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#lang " nil t)
      (if pos (progn (end-of-line) (list (point))) (buffer-file-name)))))

(defun geiser-racket--eval-bounds ()
  "Look for a module and return its end position."
  (geiser-racket--implicit-module t))

(defun geiser-racket--find-module ()
  "Try to find the module declared in a visited racket file."
  (let ((bf (geiser-racket--implicit-module))
        (sub (car (geiser-racket--explicit-module))))
    (cond ((and (not bf) (not sub)) nil)
          ((and (not bf) sub) sub)
          (sub `(submod (file ,bf) ,sub))
          (t bf))))

(defun geiser-racket--enter-command (module)
  "Format an ,enter command for entering the given MODULE."
  (when (or (stringp module) (listp module))
    (cond ((zerop (length module)) ",enter #f")
          ((or (listp module)
               (file-name-absolute-p module))
           (format ",enter %S" module))
          (t (format ",enter %s" module)))))

(defun geiser-racket--geiser-procedure (proc &rest args)
  "Format an eval procedure command for PROC with ARGS."
  (cl-case proc
    ((eval compile)
     (format ",geiser-eval %s %s %s"
             (or (car args) "#f")
             (geiser-racket--language)
             (mapconcat 'identity (cdr args) " ")))
    ((load-file compile-file)
     (format ",geiser-load %S" (geiser-racket--find-module)))
    ((no-values) ",geiser-no-values")
    (t (format ",apply geiser:%s (%s)" proc (mapconcat 'identity args " ")))))

(defun geiser-racket--get-module (&optional module)
  "Return the current module symbol, using MODULE as a hint."
  (cond ((null module) (or (geiser-racket--find-module) :f))
        ((symbolp module) module)
        ((and (stringp module) (file-name-absolute-p module)) module)
        ((stringp module) (make-symbol module))
        (t nil)))

(defun geiser-racket--symbol-begin (_module)
  "Position of the beginning of a symbol."
  (save-excursion (skip-syntax-backward "^'-()>") (point)))

(defun geiser-racket--import-command (module)
  "Format a require statement for MODULE."
  (and (stringp module)
       (not (zerop (length module)))
       (format "(require %s)" module)))

(defun geiser-racket--exit-command ()
  "Send a REPL exit command, return current process."
  (comint-send-eof)
  (get-buffer-process (current-buffer)))

(defconst geiser-racket--binding-forms
  '("for" "for/list" "for/hash" "for/hasheq" "for/and" "for/or"
    "for/lists" "for/first" "for/last" "for/fold"
    "for:" "for/list:" "for/hash:" "for/hasheq:" "for/and:" "for/or:"
    "for/lists:" "for/first:" "for/last:" "for/fold:"
    "define-syntax-rule"))

(defconst geiser-racket--binding-forms*
  '("for*" "for*/list" "for*/lists" "for*/hash" "for*/hasheq" "for*/and"
    "for*/or" "for*/first" "for*/last" "for*/fold"
    "for*:" "for*/list:" "for*/lists:" "for*/hash:" "for*/hasheq:" "for*/and:"
    "for*/or:" "for*/first:" "for*/last:" "for*/fold:"))

;;; External help

(defsubst geiser-racket--get-help (symbol module)
  "Ask the Racket REPL for help on SYMBOL for the given MODULE."
  (geiser-eval--send/wait `(:scm ,(format ",help %s %s" symbol module))))

(defun geiser-racket--external-help (id module)
  "Display help for identifier ID in module MODULE."
  (message "Looking up manual for '%s'..." id)
  (let* ((ret (geiser-racket--get-help id (format "%S" module)))
         (out (geiser-eval--retort-output ret))
         (ret (if (and out (string-match " but provided by:\n +\\(.+\\)\n" out))
                  (geiser-racket--get-help id (match-string 1 out))
                ret)))
    (unless (string-match "^Sending to web browser.+"
                          (geiser-eval--retort-output ret))
      (minibuffer-message "%s not found" (current-message)))
    t))


;;; Error display

(defconst geiser-racket--file-rxs
  '(nil
    "path:\"?\\([^>\"\n]+\\)\"?>"
    "module: \"\\([^>\"\n]+\\)\""))

(defconst geiser-racket--geiser-file-rx
  (format "^ *%s/geiser" (regexp-quote geiser-racket-scheme-dir)))

(defun geiser-racket--purge-trace ()
  "Clean up displayed stack trace."
  (save-excursion
    (while (re-search-forward geiser-racket--geiser-file-rx nil t)
      (kill-whole-line))))

(defun geiser-racket--display-error (_module key msg)
  "Display an error returned from an evaluation with the given KEY and MSG."
  (when key
    (insert "Error: ")
    (geiser-doc--insert-button key nil 'racket)
    (newline 2))
  (when msg
    (let ((p (point)))
      (insert msg)
      (let ((end (point)))
        (goto-char p)
        (when key (geiser-racket--purge-trace))
        (mapc 'geiser-edit--buttonize-files geiser-racket--file-rxs)
        (goto-char end)
        (newline))))
  (if (and msg (string-match "\\(.+\\)$" msg)) (match-string 1 msg) key))



(defun geiser-racket--guess ()
  "Try to ascertain whether a buffer is racket code."
  (or (save-excursion
        (goto-char (point-min))
        (re-search-forward "#lang " nil t))
      (geiser-racket--explicit-module)))


;;; Keywords and syntax

(defvar geiser-racket-font-lock-forms
  '(("^#lang\\>" . 0)
    ("\\[\\(else\\)\\>" . 1)
    ("(\\(define/match\\)\\W+[[(]?\\(\\w+\\)+\\b"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)))
  "Additional font lock for Racket.")

(defun geiser-racket--keywords ()
  "Return a list of non-scheme Racket keywords."
  (append geiser-racket-font-lock-forms
          (geiser-syntax--simple-keywords geiser-racket-extra-keywords)))

(geiser-syntax--scheme-indent
 (begin0 1)
 (case-lambda: 0)
 (class* defun)
 (compound-unit/sig 0)
 (define: defun)
 (for 1)
 (for* 1)
 (for*/and 1)
 (for*/first 1)
 (for*/fold 2)
 (for*/hash 1)
 (for*/hasheq 1)
 (for*/hasheqv 1)
 (for*/last 1)
 (for*/list 1)
 (for*/lists 2)
 (for*/or 1)
 (for*/product 1)
 (for*/set 1)
 (for*/seteq 1)
 (for*/seteqv 1)
 (for*/sum 1)
 (for*/vector 1)
 (for/and 1)
 (for/first 1)
 (for/fold 2)
 (for/hash 1)
 (for/hasheq 1)
 (for/hasheqv 1)
 (for/last 1)
 (for/list 1)
 (for/lists 2)
 (for/or 1)
 (for/product 1)
 (for/set 1)
 (for/seteq 1)
 (for/seteqv 1)
 (for/sum 1)
 (for/vector 1)
 (instantiate 2)
 (interface 1)
 (lambda/kw 1)
 (lambda: 1)
 (let*-values: 1)
 (let+ 1)
 (let-values: 1)
 (let/cc: 1)
 (let: 1)
 (letrec-values: 1)
 (letrec: 1)
 (local 1)
 (match-let 1)
 (match-let-values 1)
 (match/values 1)
 (mixin 2)
 (module defun)
 (module+ defun)
 (module* defun)
 (parameterize-break 1)
 (quasisyntax/loc 1)
 (send* 1)
 (splicing-let 1)
 (splicing-let-syntax 1)
 (splicing-let-syntaxes 1)
 (splicing-let-values 1)
 (splicing-letrec 1)
 (splicing-letrec-syntax 1)
 (splicing-letrec-syntaxes 1)
 (splicing-letrec-syntaxes+values 1)
 (splicing-letrec-values 1)
 (splicing-local 1)
 (shared 1)
 (struct 1)
 (syntax-id-rules defun)
 (syntax/loc 1)
 (type-case defun)
 (unit defun)
 (unit/sig 2)
 (with-handlers 1)
 (with-handlers: 1))


;;; REPL Startup

(defvar geiser-racket-minimum-version "5.3")

(defun geiser-racket--version (binary)
  "Use BINARY path to retrieve Racket's version."
  (car (process-lines binary "-e" "(display (version))")))

(defvar geiser-racket--image-cache-dir nil)

(defun geiser-racket--startup (_remote)
  "Start the Racket REPL."
  (set (make-local-variable 'compilation-error-regexp-alist)
       `(("^ *\\([^:(\t\n]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
  (compilation-setup t)
  (if geiser-image-cache-dir
      (geiser-eval--send/wait
       `(:eval (image-cache ,geiser-image-cache-dir) geiser/user))
    (setq geiser-racket--image-cache-dir
          (geiser-eval--send/result '(:eval (image-cache) geiser/user)))))

(defun geiser-racket--image-cache-dir ()
  "Return the directory to use to cache images."
  (or geiser-image-cache-dir geiser-racket--image-cache-dir))


;;; Additional commands

(defvar geiser-racket--submodule-history ())

(defun geiser-racket--submodule-form (name)
  "Format a string denoting a submodule with the given NAME."
  (format "module[+*]? %s"
          (cond ((eq 1 name) "")
                ((numberp name)
                 (read-string "Submodule name: " nil
                              'geiser-racket--submodule-history))
                ((stringp name) name)
                (t ""))))

(defun geiser-racket-toggle-submodules (&optional name)
  "Toggle visibility of submodule forms.

Use a prefix to be asked for a submodule NAME."
  (interactive "p")
  (geiser-edit--toggle-visibility (geiser-racket--submodule-form name)))

(defun geiser-racket-show-submodules (&optional name)
  "Unconditionally show all submodule forms.

Use a prefix to be asked for a submodule NAME."
  (interactive "p")
  (cond ((eq 1 name) (geiser-edit--show-all))
        (t (geiser-edit--show (geiser-racket--submodule-form name)))))

(defun geiser-racket-hide-submodules (&optional name)
  "Unconditionally hides all visible submodules.

Use a prefix to be asked for a submodule NAME."
  (interactive "p")
  (geiser-edit--hide (geiser-racket--submodule-form name)))


;;; Implementation definition:

(define-geiser-implementation racket
  (unsupported-procedures '(callers callees generic-methods))
  (binary geiser-racket--binary)
  (minimum-version geiser-racket-minimum-version)
  (version-command geiser-racket--version)
  (arglist geiser-racket--parameters)
  (repl-startup geiser-racket--startup)
  (prompt-regexp geiser-racket--prompt-regexp)
  (marshall-procedure geiser-racket--geiser-procedure)
  (find-module geiser-racket--get-module)
  (enter-command geiser-racket--enter-command)
  (import-command geiser-racket--import-command)
  (exit-command geiser-racket--exit-command)
  (find-symbol-begin geiser-racket--symbol-begin)
  (eval-bounds geiser-racket--eval-bounds)
  (display-error geiser-racket--display-error)
  (external-help geiser-racket--external-help)
  (check-buffer geiser-racket--guess)
  (keywords geiser-racket--keywords)
  (image-cache-dir geiser-racket--image-cache-dir)
  (case-sensitive geiser-racket-case-sensitive-p)
  (binding-forms geiser-racket--binding-forms)
  (binding-forms* geiser-racket--binding-forms*))

(geiser-impl--add-to-alist 'regexp "\\.ss\\'" 'racket t)

;;;###autoload
(geiser-activate-implementation 'racket)

;;;###autoload
(geiser-impl--add-to-alist 'regexp "\\.rkt[dl]?\\'" 'racket t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;;###autoload
(autoload 'run-racket "geiser-racket" "Start a Geiser Racket REPL." t)

(provide 'geiser-racket)
;;; geiser-racket.el ends here
