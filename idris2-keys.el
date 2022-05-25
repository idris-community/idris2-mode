;;; idris2-keys.el --- Hooks to define Idris2 keybindings -*- lexical-binding: t -*-

;; Copyright (C) 2014 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>

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

;;; We don't need to (require 'idris2-commands) because the RHS of keybindings
;;; is always just a quoted symbol

;;; Code:

(require 'idris2-core)

;;; Keymap-construction hooks

;; These are provided as hooks rather than being hard-coded to facilitate
;; their replacement and cut down on copy-paste.

(defun idris2-define-loading-keys (map)
  "Define the keys related to loading files in the keymap MAP."
  (define-key map (kbd "C-c C-l") 'idris2-load-file)
  (define-key map (kbd "C-c C-n") 'idris2-load-forward-line)
  (define-key map (kbd "C-c C-p") 'idris2-load-backward-line))

(defun idris2-define-docs-keys (map)
  "Define the keys related to documentation lookup in the keymap MAP."
  (define-key map (kbd "C-c C-t") 'idris2-type-at-point)
  (define-key map (kbd "C-c C-d C-d") 'idris2-docs-at-point)
  (define-key map (kbd "C-c C-d d") 'idris2-docs-at-point)
  (define-key map (kbd "C-c C-d C-a") 'idris2-apropos)
  (define-key map (kbd "C-c C-d a") 'idris2-apropos)
  (define-key map (kbd "C-c C-d C-t") 'idris2-type-search)
  (define-key map (kbd "C-c C-d t") 'idris2-type-search))

(defun idris2-define-editing-keys (map)
  "Define the keys related to editing Idris2 code in the keymap MAP."
  (define-key map (kbd "C-c C-c") 'idris2-case-dwim)
  ;; co: not in Idris2 yet
  ;; (define-key map (kbd "C-c C-m") 'idris2-add-missing)
  (define-key map (kbd "C-c C-e") 'idris2-make-lemma)
  (define-key map (kbd "C-c C-s") 'idris2-add-clause)
  (define-key map (kbd "C-c C-w") 'idris2-make-with-block)
  (define-key map (kbd "C-c C-a") 'idris2-proof-search)
  ;; co: not in Idris2 yet
  (define-key map (kbd "C-c C-r") 'idris2-refine)
  (define-key map (kbd "RET") 'idris2-newline-and-indent)
  ;; Not using `kbd' due to oddness about backspace and delete
  (define-key map [delete] 'idris2-delete-forward-char)
  (define-key map (kbd "C-d") 'idris2-delete-forward-char)
  (define-key map (kbd "M-n") 'idris2-next-error)
  (define-key map (kbd "M-p") 'idris2-previous-error))

(defun idris2-define-general-keys (map)
  "Define keys that are generally useful for all Idris2 modes in the keymap MAP."
  (define-key map (kbd "C-c C-z") 'idris2-pop-to-repl)
  (define-key map (kbd "<mouse-3>") 'prop-menu-show-menu)
  (define-key map (kbd "C-c C-SPC") 'prop-menu-by-completing-read))

(defun idris2-define-active-term-keys (map)
  "Define keys for manipulating active terms in the keymap MAP."
  (define-key map (kbd "C-c C-m n") 'idris2-normalize-term)
  (define-key map (kbd "C-c C-m i") 'idris2-show-term-implicits)
  (define-key map (kbd "C-c C-m h") 'idris2-hide-term-implicits)
  (define-key map (kbd "C-c C-m c") 'idris2-show-core-term))

(defun idris2-define-ipkg-keys (map)
  "Define keys for working with the current package in the keymap MAP."
  (define-key map (kbd "C-c C-b b") 'idris2-ipkg-build)
  (define-key map (kbd "C-c C-b C-b") 'idris2-ipkg-build)
  (define-key map (kbd "C-c C-b c") 'idris2-ipkg-clean)
  (define-key map (kbd "C-c C-b C-c") 'idris2-ipkg-clean)
  (define-key map (kbd "C-c C-b i") 'idris2-ipkg-install)
  (define-key map (kbd "C-c C-b C-i") 'idris2-ipkg-install))

(defun idris2-define-ipkg-editing-keys (map)
  "Define keys used only for editing packages in the keymap MAP."
  (define-key map (kbd "C-c C-f") 'idris2-ipkg-insert-field))

(defun idris2-define-ipkg-opening-keys (map)
  "Define keys used to find or open a package file in the keymap MAP."
  (define-key map (kbd "C-c C-b C-p") 'idris2-open-package-file)
  (define-key map (kbd "C-c C-b p") 'idris2-open-package-file))

(defun idris2-define-evil-keys ()
  "Define keys for evil-mode."
  (when (fboundp 'evil-leader/set-key-for-mode)
    (evil-leader/set-key-for-mode 'idris2-mode
                                  "r" 'idris2-load-file
                                  "t" 'idris2-type-at-point
                                  "d" 'idris2-add-clause
                                  "l" 'idris2-make-lemma
                                  "c" 'idris2-case-split
                                  "w" 'idris2-make-with-block
                                  ;; co: Not in idris2 yet
                                  ;; "m" 'idris2-add-missing
                                  "p" 'idris2-proof-search
                                  "h" 'idris2-docs-at-point)))

(provide 'idris2-keys)
