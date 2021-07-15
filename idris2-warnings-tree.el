;;; idris2-warnings-tree.el --- Tree view of warnings reported by idris2 in buffers -*- lexical-binding: t -*-

;; Copyright (C) 2014 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>
;; (modified slime-compiler-notes-tree.el by Helmut Eller <heller@common-lisp.net>)

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

;;; Code:
(require 'cl-lib)
(require 'prop-menu)

(require 'idris2-core)
(require 'idris2-warnings)
(require 'idris2-common-utils)

(defvar idris2-notes-buffer-name (idris2-buffer-name :notes)
  "The name of the buffer containing Idris2 errors")

(defun idris2-list-compiler-notes ()
  "Show the compiler notes in tree view."
  (interactive)
  (with-temp-message "Preparing compiler note tree..."
    (let ((notes (reverse idris2-raw-warnings))
          (buffer (get-buffer-create idris2-notes-buffer-name)))
      (with-current-buffer buffer
        (idris2-compiler-notes-mode)
        (setq buffer-read-only nil)
        (erase-buffer)
        (if (null notes)
            nil
          (let ((root (idris2-compiler-notes-to-tree notes)))
            (idris2-tree-insert root "")
            (insert "\n")
            (message "Press q to close, return or mouse on error to navigate to source")
            (setq buffer-read-only t)
            (goto-char (point-min))
            notes
            (display-buffer (idris2-buffer-name :notes))))))))

(defvar idris2-tree-printer 'idris2-tree-default-printer)

(defun idris2-tree-for-note (note)
  (let* ((buttonp (> (length (nth 0 note)) 0)) ;; if empty source location
         (button-text `(,(format "%s line %s col %s:" (nth 0 note) (nth 1 note) (nth 2 note))
                        help-echo "go to source location"
                        action ,#'(lambda (_)
                                    (idris2-show-source-location (nth 0 note)
                                                                (nth 1 note)
                                                                (nth 2 note))))))
    (make-idris2-tree :item (nth 3 note)
                     :highlighting (if (> (length note) 4) (nth 4 note) '())
                     :button (if buttonp button-text nil)
                     :after-button (if buttonp "\n" nil)
                     :plist (list 'note note)
                     :print-fn idris2-tree-printer
                     :preserve-properties '(idris2-tt-term))))

(defun idris2-compiler-notes-to-tree (notes)
  (make-idris2-tree :item (format "Errors (%d)" (length notes))
                   :kids (mapcar #'idris2-tree-for-note notes)
                   :preserve-properties '(idris2-tt-term)))

(defvar idris2-compiler-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'idris2-notes-quit)
    ;;; Allow buttons to be clicked with the left mouse button in the compiler notes
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris2-define-docs-keys
                  idris2-define-general-keys
                  idris2-define-active-term-keys)
             do (funcall keyer map))
    map)
  "Keymap used in Idris2 Compiler Notes mode.")

(easy-menu-define idris2-compiler-notes-mode-menu idris2-compiler-notes-mode-map
  "Menu for Idris2 compiler notes buffers"
  `("Idris2 Notes"
    ["Show term interaction widgets" idris2-add-term-widgets t]
    ["Close Idris2 info buffer" idris2-notes-quit t]))

(defun idris2-notes-quit ()
  (interactive)
  (idris2-kill-buffer :notes))

(define-derived-mode idris2-compiler-notes-mode fundamental-mode "Compiler-Notes"
  "Idris2 compiler notes
     \\{idris2-compiler-notes-mode-map}
Invokes `idris2-compiler-notes-mode-hook'."
  (setq-local prop-menu-item-functions '(idris2-context-menu-items)))

(defun idris2-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (idris2-tree-at-point))
         (note (plist-get (idris2-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (idris2-tree-leaf-p tree))
           (idris2-tree-toggle tree))
          (t
           (idris2-show-source-location (nth 0 note) (nth 1 note) (nth 2 note))))))

(defun idris2-show-source-location (filename lineno col &optional is-same-window)
  (idris2-goto-source-location filename lineno col is-same-window))

(defun idris2-get-fullpath-from-idris2-file (filename)
  "Returns the full filepath of a filename receives from the inferior idris2 process"
  (concat (file-name-as-directory idris2-process-current-working-directory) filename)
  )

(defun idris2-goto-location (fullpath)
  "Opens buffer for filename"
  (if (file-exists-p fullpath)
    (or (get-buffer fullpath)
	(get-file-buffer fullpath)
	(find-file-noselect fullpath)))
  )

(defun idris2-goto-source-location-full (fullpath lineno col is-same-window)
  "Move to the source location FILENAME LINENO COL. Filename must
be a full path. Otherwise works just like
idris2-goto-source-location"
  (let ((buf (idris2-goto-location fullpath)))
    (set-buffer buf)
    (pop-to-buffer buf (if is-same-window '(display-buffer-same-window) t))
    (pcase-let* ((old-start (point-min)) ; The start and end taking
                 (old-end (point-max))   ; narrowing into account
                 (`(,new-location . ,widen-p)
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (let* ((start (line-beginning-position lineno))
                             (location (goto-char (+ start col))))
                        ;; If the location is invisible, offer to make it visible
                        (if (or (< location old-start) (> location old-end))
                            (if (y-or-n-p "Location is not visible. Widen? ")
                                (cons location t)
                              (cons nil nil))
                          (cons location nil)))))))
      (when new-location
        (when widen-p (widen))
        (goto-char new-location)))))

(defun idris2-goto-source-location (filename lineno col is-same-window)
  "Move to the source location FILENAME LINENO COL. If the buffer
containing the file is narrowed and the location is hidden, show
a preview and offer to widen."
  (idris2-goto-source-location-full (idris2-get-fullpath-from-idris2-file filename) lineno col is-same-window))

;;;;;; Tree Widget

(cl-defmacro with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2))
  (let ((struct-var (cl-gensym "struct")))
    `(let ((,struct-var ,struct))
       (cl-symbol-macrolet
           ,(mapcar (lambda (slot)
                      (cl-etypecase slot
                        (symbol `(,slot (,(intern (concat (symbol-name conc-name) (symbol-name slot))) ,struct-var)))
                        (cons `(,(car slot) (,(intern (concat (symbol-name conc-name) (symbol-name (cadr slot))))
                                               ,struct-var)))))
                    slots)
         . ,body))))

(cl-defstruct (idris2-tree (:conc-name idris2-tree.))
  item
  highlighting
  (print-fn #'idris2-tree-default-printer :type function)
  (kids '() :type (or list function))
  (collapsed-p nil :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list)
  (active-p t :type boolean)
  (button nil :type list)
  (after-button "" :type string)
  (preserve-properties '() :type list))

(defun idris2-tree-leaf-p (tree)
  ;; Evaluate the kids to see if we are at a leaf
  (when (functionp (idris2-tree.kids tree))
    (setf (idris2-tree.kids tree) (funcall (idris2-tree.kids tree))))
  (cl-assert (listp (idris2-tree.kids tree)))
  (null (idris2-tree.kids tree)))

(defun idris2-tree-default-printer (tree)
  (when (idris2-tree.button tree)
    (apply #'insert-button (idris2-tree.button tree))
    (insert (idris2-tree.after-button tree)))
  (idris2-propertize-spans (idris2-repl-semantic-text-props (idris2-tree.highlighting tree))
    (insert (idris2-tree.item tree))))

(defun idris2-tree-decoration (tree)
  (cond ((idris2-tree-leaf-p tree) "--")
	((idris2-tree.collapsed-p tree) "[+]")
	(t "- +")))

(defun idris2-tree-insert-list (list prefix)
  "Insert a list of trees."
  (cl-loop for (elt . rest) on list
           do (cond (rest
                     (insert prefix " |")
                     (idris2-tree-insert elt (concat prefix " |"))
                     (insert "\n"))
                    (t
                     (insert prefix " `")
                     (idris2-tree-insert elt (concat prefix "  "))))))

(defun idris2-tree-insert-decoration (tree)
  (with-struct (idris2-tree. print-fn kids collapsed-p start-mark end-mark active-p) tree
    (let ((deco (idris2-tree-decoration tree)))
      (if (and active-p kids)
          (insert-button deco 'action #'(lambda (_)
                                          (setq buffer-read-only nil)
                                          (idris2-tree-toggle tree)
                                          (setq buffer-read-only t))
                              'help-echo (if collapsed-p "expand" "collapse"))
        (insert deco))
      (insert " "))))

(defun idris2-tree-indent-item (start end prefix &optional preserve-props)
  "Insert PREFIX at the beginning of each but the first line between START and END, copying the text properties in PRESERVE-PROPS.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (let* ((props-here (text-properties-at (point)))
             (props (cl-loop for p in preserve-props
                             for val = (plist-get props-here p)
                             when val
                             append(list p val))))
        (insert-before-markers (apply #'propertize prefix props))
        (forward-line -1)))))

(defun idris2-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (unless (idris2-tree-p tree) (error "%s is not an idris2-tree" tree))
  (with-struct (idris2-tree. print-fn kids collapsed-p start-mark end-mark active-p preserve-properties)
      tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (idris2-tree-insert-decoration tree)
      (funcall print-fn tree)
      (idris2-tree-indent-item start-mark (point) (concat prefix "   ") preserve-properties)
      (add-text-properties line-start (point) (list 'idris2-tree tree))
      (set-marker-insertion-type start-mark t)
      (when  (not collapsed-p)
        (when (functionp kids)
          (setf kids (funcall kids))
          (cl-assert (listp kids)))
        (when kids
          (terpri (current-buffer))
          (idris2-tree-insert-list kids prefix)))
      (setf (idris2-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun idris2-tree-at-point ()
  (cond ((get-text-property (point) 'idris2-tree))
        (t (error "No tree at point"))))

(defun idris2-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (idris2-tree.start-mark tree)
                 (idris2-tree.end-mark tree)))

(defun idris2-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (idris2-tree. collapsed-p start-mark end-mark prefix) tree
    (save-excursion
      (setf collapsed-p (not collapsed-p))
      (goto-char start-mark)
      (idris2-tree-delete tree)
      (insert-before-markers " ") ; move parent's end-mark
      (backward-char 1)
      (idris2-tree-insert tree prefix)
      (delete-char 1))
    (goto-char start-mark)))

(provide 'idris2-warnings-tree)
