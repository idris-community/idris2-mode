;;; idris2-mode.el --- Major mode for editing Idris2 code -*- lexical-binding: t -*-

;; Copyright (C) 2013

;; Author:
;; URL: https://github.com/idris2-hackers/idris2-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24") (prop-menu "0.1") (cl-lib "0.5"))
;; Version: 0.9.18


;;; Commentary:

;; This is an Emacs mode for editing Idris2 code. It requires the latest
;; version of Idris2, and some features may rely on the latest Git version of
;; Idris2.

;;; Code:

(require 'prop-menu)
(require 'eldoc)

(require 'idris2-core)
(require 'idris2-settings)
(require 'idris2-syntax)
(require 'idris2-simple-indent)
(require 'idris2-repl)
(require 'idris2-commands)
(require 'idris2-warnings)
(require 'idris2-common-utils)
(require 'idris2-ipkg-mode)

(defun idris2-mode-context-menu-items (plist)
  "Compute menu items from PLIST that are specific to editing text in `idris2-mode'."
  (let ((ref (plist-get plist 'idris2-ref))
        (ref-style (plist-get plist 'idris2-ref-style)))
    (when (and ref (equal ref-style :metavar))
      (list (list "Extract lemma"
                  (let ((location (point)))
                    (lambda ()
                      (interactive)
                      (save-excursion
                        (goto-char location)
                        (idris2-make-lemma)))))
            (list "Fill with case block"
                  (let ((location (point)))
                    (lambda ()
                      (interactive)
                      (save-excursion
                        (goto-char location)
                        (idris2-make-cases-from-hole)))))))))

(defvar idris2-mode-map (let ((map (make-sparse-keymap)))
                          (cl-loop for keyer
                                   in '(idris2-define-loading-keys
                                        idris2-define-docs-keys
                                        idris2-define-editing-keys
                                        idris2-define-general-keys
                                        idris2-define-ipkg-keys
                                        idris2-define-ipkg-opening-keys)
                                   do (funcall keyer map))
                          map)
  "Keymap used in Idris2 mode.")

(easy-menu-define idris2-mode-menu idris2-mode-map
  "Menu for the Idris2 major mode"
  `("Idris2"
    ["New Project" idris2-start-project t]
    "-----------------"
    ["Load file" idris2-load-file t]
    ["Choose packages" idris2-set-idris2-load-packages t]
    ["Compile and execute" idris2-compile-and-execute]
    ["View compiler log" idris2-view-compiler-log (get-buffer idris2-log-buffer-name)]
    ["Quit inferior idris2 process" idris2-quit t]
    "-----------------"
    ["Add initial match clause to type declaration" idris2-add-clause t]
    ;;co: not in Idris2 yet
    ;;["Add missing cases" idris2-add-missing t]
    ["Case split pattern variable" idris2-case-split t]
    ["Add with block" idris2-make-with-block t]
    ["Extract lemma from hole" idris2-make-lemma t]
    ["Solve hole with case expression" idris2-make-cases-from-hole t]
    ["Attempt to solve hole" idris2-proof-search t]
    ["Generate definition" idris2-generate-def t]
    ["Get next definition" idris2-generate-def-next t]
    ["Display type" idris2-type-at-point t]
    ["Goto definition" idris2-jump-to-def t]
    "-----------------"
    ["Open package" idris2-open-package-file t]
    ["Build package" idris2-ipkg-build t]
    ["Install package" idris2-ipkg-install t]
    ["Clean package" idris2-ipkg-clean t]
    "-----------------"
    ["Get documentation" idris2-docs-at-point t]
    ["Search for type" idris2-type-search t]
    ["Apropos" idris2-apropos t]
    ["Browse namespace" idris2-browse-namespace t]
    ["Pretty-print to HTML or LaTeX" idris2-pretty-print t]
    "-----------------"
    ("Interpreter options" :active idris2-process
     ["Show implicits" (idris2-set-option :show-implicits t)
      :visible (not (idris2-get-option :show-implicits))]
     ["Hide implicits" (idris2-set-option :show-implicits nil)
      :visible (idris2-get-option :show-implicits)]
     ["Show error context" (idris2-set-option :error-context t)
      :visible (not (idris2-get-option :error-context))]
     ["Hide error context" (idris2-set-option :error-context nil)
      :visible (idris2-get-option :error-context)])
    ["Customize idris2-mode" (customize-group 'idris2) t]
    ["Customize fonts and colors" (customize-group 'idris2-faces) t]))


;;;###autoload
(define-derived-mode idris2-mode prog-mode "Idris2"
  "Major mode for Idris2
     \\{idris2-mode-map}
Invokes `idris2-mode-hook'."
  :syntax-table idris2-syntax-table
  :group 'idris2
  (set (make-local-variable 'font-lock-defaults)
       (idris2-font-lock-defaults))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "--")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'syntax-propertize-function) 'idris2-syntax-propertize-function)

  ;; REPL completion for Idris2 source
  (set (make-local-variable 'completion-at-point-functions) '(idris2-complete-at-point))

  ;; imenu support
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (set (make-local-variable 'imenu-generic-expression)
       '(("Data" "^\\s-*data\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*record\\s-+\\(\\sw+\\)" 1)
         ("Data" "^\\s-*codata\\s-+\\(\\sw+\\)" 1)
         ("Postulates" "^\\s-*postulate\\s-+\\(\\sw+\\)" 1)
         ("Classes" "^\\s-*class\\s-+\\(\\sw+\\)" 1)
         (nil "^\\s-*\\(\\sw+\\)\\s-*:" 1)
         ("Namespaces" "^\\s-*namespace\\s-+\\(\\sw\\|\\.\\)" 1)))

  ;; eldoc support
  (set (make-local-variable 'eldoc-documentation-function) 'idris2-eldoc-lookup)

  ;; Filling of comments and docs
  (set (make-local-variable 'fill-paragraph-function) 'idris2-fill-paragraph)
  ;; Make dirty if necessary
  (add-hook (make-local-variable 'after-change-functions) 'idris2-possibly-make-dirty)
  (setq mode-name `("Idris2"
                    (:eval (if idris2-rex-continuations "!" ""))
                    " "
                    (:eval (if (idris2-current-buffer-dirty-p)
                               "(Not loaded)"
                             "(Loaded)"))))
  ;; Extra hook for LIDR files (to set up extra highlighting, etc)
  (when (idris2-lidr-p)
    (run-hooks 'idris2-mode-lidr-hook))
  (set (make-local-variable 'prop-menu-item-functions)
       '(idris2-context-menu-items idris2-mode-context-menu-items)))

;; Automatically use idris2-mode for .idr and .lidr files.
;;;###autoload
(push '("\\.idr$" . idris2-mode) auto-mode-alist)
;;;###autoload
(push '("\\.lidr$" . idris2-mode) auto-mode-alist)

;;; Handy utilities for other modes
(eval-after-load 'flycheck
  '(eval
    '(progn
       (flycheck-define-checker idris2
         "An Idris2 syntax and type checker."
         :command ("idris2"
                 "--check" "--nocolor" "--warnpartial"
                 ;; Compute the command-line options similarly to inferior-idris2
                 (eval (idris2-compute-flags))
                 source)
         :error-patterns
         ((warning line-start (file-name) ":" line ":" column ":Warning - "
                   (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl)))))
          (error line-start (file-name) ":" line ":" column ":"
                 (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl))))))
         :modes idris2-mode)

       (add-to-list 'flycheck-checkers 'idris2))))

(provide 'idris2-mode)
;;; idris2-mode.el ends here
