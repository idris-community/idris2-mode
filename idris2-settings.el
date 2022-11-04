;;; idris2-settings.el --- Contains settings for idris2-mode

;; Copyright (C) 2013 Hannes Mehnert and David Raymond Christiansen

;; Author: Hannes Mehnert <hannes@mehnert.org> and David Raymond Christiansen <david@davidchristiansen.dk>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris2-core)
(require 'idris2-keys)

;;;; Main settings

(defgroup idris2 nil "Idris2 mode" :prefix 'idris2 :group 'languages)

(defcustom idris2-interpreter-path "idris2"
  "The path to the Idris2 interpreter"
  :type 'file
  :group 'idris2)

(defcustom idris2-interpreter-flags '()
  "The command line arguments passed to the Idris2 interpreter"
  :type '(repeat string)
  :group 'idris2)

(defcustom idris2-source-locations '()
  "A hack to specify location(s) of idris2 source. This is used for finding definitions of idris2 symbols that are not in the current source path for use with idris2-jump-to-def, etc. (It's a hack because it should be related to the libraries included in the project being compiled, but it is not. If you switch projects, you will have to update this)"
  :type '(repeat file)
  :group 'idris2)

(defcustom idris2-warnings-printing (list 'warnings-tree)
  "How to print warnings: tree view ('warnings-tree) in REPL ('warnings-repl)"
  :group 'idris2
  :type '(repeat symbol)
  :options '(warnings-tree warnings-repl))

(defcustom idris2-pretty-printer-width 100
  "The default width to use for pretty-printing."
  :group 'idris2
  :type '(choice (integer :tag "Columns")
                 (const :tag "Unlimited" nil)))


(defcustom idris2-show-help-text t
  "Show explanatory text in idris2-mode's auxiliary buffers if
  non-nil. Advanced users may wish to disable this."
  :group 'idris2
  :type 'boolean)

(defcustom idris2-stay-in-current-window-on-compiler-error nil
  "Stay in current window if type checking fails."
  :group 'idris2
  :type 'boolean)

(defcustom idris2-semantic-source-highlighting t
  "If non-nil, use the Idris2 compiler's semantic source
information to highlight Idris2 code. If `debug', log failed
  highlighting to buffer `*Messages*'."
  :group 'idris2
  :type '(choice (boolean :tag "Enable")
                 (const :tag "Debug" debug)))

(defcustom idris2-log-events nil
  "If non-nil, communications between Emacs and Idris2 are logged.

The log is placed in `idris2-event-buffer-name'."
  :group 'idris2
  :type 'boolean)

;;; Faces
(defface idris2-active-term-face
  '((((background light))
     :background "lightgray")
    (((background dark))
     :background "darkgray"))
  "The face to highlight active terms"
  :group 'idris2-faces)

(defface idris2-semantic-type-face
  '((((background light))
      :foreground "blue")
    (((background dark))
      :foreground "cornflower blue"))
  "The face to be used to highlight types"
  :group 'idris2-faces)

(defface idris2-semantic-data-face
  '((((background light))
      :foreground "red")
    (((background dark))
      :foreground "firebrick1"))
  "The face to be used to highlight data and constructors"
  :group 'idris2-faces)

(defface idris2-semantic-function-face
  '((((background light))
      :foreground "darkgreen")
    (((background dark))
      :foreground "#A6E22E"))
  "The face to be used to highlight defined functions"
  :group 'idris2-faces)

(defface idris2-semantic-postulate-face
  '((t (:inherit idris2-unsafe-face :weight semi-bold)))
  "The face to be used to highlight postulated values"
  :group 'idris2-faces)

(defface idris2-semantic-bound-face
  '((((background light))
     :foreground "purple")
    (((background dark))
     :foreground "MediumPurple1"))
  "The face to be used to highlight bound variables"
  :group 'idris2-faces)

(defface idris2-semantic-implicit-face
  '((t (:underline t)))
  "The face to be used to highlight implicit arguments"
  :group 'idris2-faces)

(defface idris2-semantic-namespace-face
  '((t (:italic t)))
  "The face to be used to highlight namespace declarations"
  :group 'idris2-faces)

(defface idris2-semantic-module-face
  '((t :inherit idris2-semantic-namespace-face))
  "The face to be used to highlight namespace declarations"
  :group 'idris2-faces)

(defface idris2-quasiquotation-face nil
  "The face to be used to highlight quasiquotations in Idris2 source code"
  :group 'idris2-faces)

(defface idris2-antiquotation-face nil
  "The face to be used to highlight antiquotations in Idris2 source code"
  :group 'idris2-faces)

(defface idris2-loaded-region-face nil
  "The face to use for the currently-loaded region of a
buffer. Since semantic highlighting has been added, this face
defaults to nothing, but is provided for users who prefer the old
behavior."
  :group 'idris2-faces)

(defface idris2-inline-doc-face
  '((t :inherit font-lock-doc-face))
  "The face shown for Idris2Doc while editing Idris2 files."
  :group 'idris2-faces)

(defface idris2-link-face
  '((t :inherit button))
  "The face shown for Web links in Idris2 documentation."
  :group 'idris2-faces)

(defface idris2-info-title-face
  '((t :inherit header-line))
  "Face for Idris2 headers and titles."
  :group 'idris2-faces)

;;; Mode hooks
(defcustom idris2-mode-hook '(turn-on-idris2-simple-indent
                             turn-on-eldoc-mode)
  "Hook to run upon entering Idris2 mode. You should choose at most one indentation style."
  :type 'hook
  :options '(turn-on-idris2-simple-indent
             turn-on-eldoc-mode)
  :group 'idris2)

(defcustom idris2-mode-lidr-hook '()
  "Hook to run after opening a literate Idris2 file. Use this to customize the display of non-code text."
  :type 'hook
  :group 'idris2)

(defcustom idris2-info-mode-hook ()
  "Hook to run when setting up Idris2 info buffers."
  :type 'hook
  :options ()
  :group 'idris2)

(defcustom idris2-repl-mode-hook ()
  "Hook to run when setting up the Idris2 REPL."
  :type 'hook
  :options ()
  :group 'idris2)

(defcustom idris2-compiler-notes-mode-hook ()
  "Hook to run when setting up the compiler notes buffers."
  :type 'hook
  :options ()
  :group 'idris2)

(defgroup idris2-hole-list nil
  "Options related to the Idris2 hole list buffer."
  :group 'idris2)

(defcustom idris2-hole-list-mode-hook ()
  "Hook to run when setting up the list of holes."
  :type 'hook
  :options ()
  :group 'idris2-hole-list)

(defcustom idris2-hole-show-on-load t
  "Show the current holes on successful load."
  :type 'boolean
  :group 'idris2)

(defcustom idris2-hole-list-show-expanded t
  "Show the hole list fully expanded by default. This may be useful on wide monitors
with lots of space for the hole buffer."
  :type 'boolean
  :group 'idris2-hole-list)

(defcustom idris2-enable-elab-prover nil
  "Whether or not to enable the interactive prover for elaborator reflection.
Disabled by default until Idris2 0.9.19 because it requires a
change to ordinary prover interaction."
  :type 'boolean
  :group 'idris2)

;;;; Other hooks

(autoload 'idris2-set-current-pretty-print-width "idris2-commands.el")
(defcustom idris2-run-hook '(idris2-set-current-pretty-print-width)
  "A hook to run when Idris2 is started."
  :type 'hook
  :group 'idris2
  :options '(idris2-set-current-pretty-print-width))

;;;; REPL settings

(defgroup idris2-repl nil "Idris2 REPL" :prefix 'idris2 :group 'idris2)

(defcustom idris2-repl-banner-functions '(idris2-repl-insert-logo
                                         idris2-repl-animate-banner
                                         idris2-repl-text-banner)
  "A list of functions that can attempt to insert a banner into
the REPL. If a function cannot insert a banner (for instance, if
it is supposed to insert a graphical banner but the current Emacs
has no image support), it returns `nil'. The functions in this
list are run in order, until one returns non-`nil'.
Set to `nil' for no banner."
  :type 'hook
  :group 'idris2-repl
  :options '(idris2-repl-insert-logo
             idris2-repl-animate-banner
             idris2-repl-text-banner))

(defcustom idris2-repl-show-idris2-version t
  "Whether to show the Idris2 version on REPL startup."
  :type 'boolean
  :group 'idris2-repl)

(defface idris2-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the Idris2 REPL."
  :group 'idris2-repl)

(defface idris2-repl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for Idris2 output in the Idris2 REPL."
  :group 'idris2-repl)

(defface idris2-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the Idris2 REPL."
  :group 'idris2-repl)

(defface idris2-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the Idris2 REPL."
  :group 'idris2-repl)

(defcustom idris2-repl-history-file "~/.idris2/idris2-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'idris2-repl)

(defcustom idris2-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'idris2-repl)

(defcustom idris2-repl-history-file-coding-system
  'utf-8-unix
  "*The coding system for the history file."
  :type 'symbol
  :group 'idris2-repl)

(defcustom idris2-repl-prompt-style 'short
  "What sort of prompt to show. 'long shows the Idris2 REPL prompt, while 'short shows a shorter one."
  :options '(short long)
  :type 'symbol
  :group 'idris2-repl)

(defcustom idris2-repl-show-repl-on-startup t
  "If non-`nil', show the REPL window when Idris2 starts. If `nil', only do this when `idris2-repl' was called interactively."
  :type 'boolean
  :group 'idris2-repl)

(defgroup idris2-info-buffer nil "Idris2 info buffer" :group 'idris2)

(defcustom idris2-info-buffer-focus nil
  "If `nil', keep focus in current buffer and only make the info buffer visible.
If non-`nil', pop to the Idris2 info buffer if it was not visible."
  :type 'boolean
  :group 'idris2-info-buffer)

(provide 'idris2-settings)
