;;; idris2-hole-list.el --- List Idris2 holes in a buffer -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'prop-menu)

(require 'idris2-core)
(require 'idris2-keys)
(require 'idris2-warnings-tree)
(require 'idris2-settings)

(defvar idris2-hole-list-buffer-name (idris2-buffer-name :holes)
  "The name of the buffer containing Idris2 holes")

(defun idris2-hole-list-quit ()
  "Quit the Idris2 hole list"
  (interactive)
  (idris2-kill-buffer idris2-hole-list-buffer-name))

(defvar idris2-hole-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "q") 'idris2-hole-list-quit)
    (define-key map (kbd "RET") 'idris2-compiler-notes-default-action-or-show-details)
    (define-key map (kbd "<mouse-2>") 'idris2-compiler-notes-default-action-or-show-details/mouse)
    ;; Allow buttons to be clicked with the left mouse button in the hole list
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris2-define-docs-keys
                  idris2-define-general-keys
                  idris2-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris2-hole-list-mode-menu idris2-hole-list-mode-map
  "Menu for the Idris2 hole list buffer"
  `("Idris2 Holes"
    ["Show term interaction widgets" idris2-add-term-widgets t]
    ["Close hole list buffer" idris2-hole-list-quit t]
    "------------------"
    ["Customize idris2-hole-list-mode" (customize-group 'idris2-hole-list) t]
    ["Customize fonts and colors" (customize-group 'idris2-faces) t]))

(define-derived-mode idris2-hole-list-mode fundamental-mode "Idris2 Holes"
  "Major mode used for transient Idris2 hole list buffers
   \\{idris2-hole-list-mode-map}
Invoces `idris2-hole-list-mode-hook'."
  (setq-local prop-menu-item-functions '(idris2-context-menu-items)))

(defun idris2-hole-list-buffer ()
  "Return the Idris2 hole buffer, creating one if there is not one"
  (get-buffer-create idris2-hole-list-buffer-name))

(defun idris2-hole-list-buffer-visible-p ()
  (if (get-buffer-window idris2-hole-list-buffer-name 'visible) t nil))

(defun idris2-hole-list-show (hole-info)
  (if (null hole-info)
      (progn (message "No holes found!")
             (idris2-hole-list-quit))
    (with-current-buffer (idris2-hole-list-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (idris2-hole-list-mode)
      (insert (propertize "Holes" 'face 'idris2-info-title-face) "\n\n")
      (when idris2-show-help-text
        (insert "This buffer displays the unsolved holes from the currently-loaded code. ")
        (insert (concat "Press the "
                        (if idris2-enable-elab-prover "[E]" "[P]")
                        " buttons to solve the holes interactively in the prover."))
        (let ((fill-column 80))
          (fill-region (point-min) (point-max)))
        (insert "\n\n"))

      (dolist (tree (mapcar #'idris2-tree-for-hole hole-info))
        (idris2-tree-insert tree "")
        (insert "\n\n"))
      (message "Press q to close")
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer (idris2-hole-list-buffer))))

(defun idris2-hole-tree-printer (tree)
  "Print TREE, formatted for holes."
  (idris2-propertize-spans (idris2-repl-semantic-text-props (idris2-tree.highlighting tree))
    (insert (idris2-tree.item tree)))
  (when (idris2-tree.button tree)
    (insert " ")
    (apply #'insert-button (idris2-tree.button tree))
    (insert (idris2-tree.after-button tree))))

;;; Prevent circularity error
(autoload 'idris2-prove-hole "idris2-commands.el")

(defun idris2-tree-for-hole (hole)
  "Generate a tree for HOLE.

HOLE should be a three-element list consisting of the
hole name, its premises, and its conclusion."
  (cl-destructuring-bind (name premises conclusion) hole
    (make-idris2-tree :item name
                      :button (if idris2-enable-elab-prover
                                  `("[E]"
                                    help-echo "Elaborate interactively"
                                    action ,#'(lambda (_)
                                                (interactive)
                                                (idris2-prove-hole name t)))
                                `("[P]"
                                  help-echo "Open in prover"
                                  action ,#'(lambda (_)
                                              (interactive)
                                              (idris2-prove-hole name))))
                      :highlighting `((0 ,(length name) ((:decor :metavar))))
                      :print-fn #'idris2-hole-tree-printer
                      :collapsed-p (not idris2-hole-list-show-expanded) ; from customize
                      :preserve-properties '(idris2-tt-term)
                      :kids (list (idris2-tree-for-hole-details name premises conclusion)))))

(defun idris2-tree-for-hole-details (name premises conclusion)
  (let* ((name-width (1+ (apply #'max 0 (length name)
                                (mapcar #'(lambda (h) (length (car h)))
                                        premises))))
         (divider-marker nil)
         (contents (with-temp-buffer
                     (dolist (h premises)
                       (cl-destructuring-bind (name type formatting) h
                         (cl-dotimes (_ (- name-width (length name))) (insert " "))
                         (idris2-propertize-spans (idris2-repl-semantic-text-props
                                                   `((0 ,(length name) ((:decor :bound)))))
                           (insert name))
                         (insert " : ")
                         (let ((start (point)))
                           (idris2-propertize-spans (idris2-repl-semantic-text-props formatting)
                             (insert type))
                           (insert "\n")
                           ;; Indent the term to match the tree and
                           ;; its binder, if it is more than one line.
                           (let ((term-end-marker (copy-marker (point))))
                             (beginning-of-line)
                             (forward-line -1)
                             (while (< start (point))
                               ;; Preserve the term annotation, to not break active terms
                               (let ((tm (get-text-property (point) 'idris2-tt-term)))
                                 (insert-before-markers
                                  (propertize (make-string (+ 3 name-width) ? )
                                              'idris2-tt-term tm)))
                               (forward-line -1))
                             (goto-char term-end-marker)))))
                     (setq divider-marker (point-marker))
                     (cl-destructuring-bind (type formatting) conclusion
                       (when premises
                         (insert " ")
                         (idris2-propertize-spans (idris2-repl-semantic-text-props
                                                   `((0 ,(length name) ((:decor :metavar)))))
                           (insert name))
                         (insert " : "))
                       (idris2-propertize-spans (idris2-repl-semantic-text-props formatting)
                         (insert type)))
                     (when premises
                       (let ((width (apply #'max 0
                                           (mapcar #'length
                                                   (split-string (buffer-string) "\n")))))
                         (goto-char (marker-position divider-marker))
                         (dotimes (_ (1+ width)) (insert "-"))
                         (insert "\n")))
                     (buffer-string))))
    (make-idris2-tree :item contents
                      :active-p nil
                      :highlighting '()
                      :preserve-properties '(idris2-tt-term))))

(provide 'idris2-hole-list)
