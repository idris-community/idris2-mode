;;; idris2-warnings.el --- Mark warnings reported by idris2 in buffers -*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>

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
(require 'idris2-common-utils)
(require 'cl-lib)

(defface idris2-warning-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t
     :inherit warning))
  "Face for warnings from the compiler."
  :group 'idris2-faces)

(defvar idris2-warnings-buffers '() "All buffers which have warnings")
(defvar-local idris2-warnings '() "All warnings in the current buffer")
(defvar idris2-raw-warnings '() "All warnings from Idris2")

(defun idris2-warning-event-hook-function (event)
  (pcase event
    (`(:warning ,output ,_target)
     (idris2-warning-overlay output)
     t)
    (_ nil)))

(defun idris2-warning-reset-all ()
  (mapc #'idris2-warning-reset-buffer idris2-warnings-buffers)
  (setq idris2-raw-warnings '())
  (setq idris2-warnings-buffers '()))

(defun idris2-warning-reset-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (idris2-warning-reset))))

(defun idris2-warning-reset ()
  (mapc #'delete-overlay idris2-warnings)
  (setq idris2-warnings '())
  (delq (current-buffer) idris2-warnings-buffers))

(defun idris2-get-line-region (line)
  (goto-char (point-min))
  (cl-values
   (line-beginning-position line)
   (line-end-position line)))

(defun idris2-warning-overlay-p (overlay)
  (overlay-get overlay 'idris2-warning))

(defun idris2-warning-overlay-at-point ()
  "Return the overlay for a note starting at point, otherwise nil."
  (cl-find (point) (cl-remove-if-not 'idris2-warning-overlay-p (overlays-at (point)))
        :key 'overlay-start))

(defun idris2-warning-overlay (warning)
  "Add a compiler warning to the buffer as an overlay.
May merge overlays, if there's already one in the same location.
WARNING is of form (filename (startline startcolumn) (endline endcolumn) message &optional highlighting-spans)
As of 20140807 (Idris2 0.9.14.1-git:abee538) (endline endcolumn) is mostly the same as (startline startcolumn)
"
  (cl-destructuring-bind (filename sl1 sl2 message spans) warning
    (let ((startline (nth 0 sl1))
          (startcol (1- (nth 1 sl1)))
          (endline (nth 0 sl2))
          (endcol (1- (nth 1 sl2))))
      (push (list filename startline startcol message spans) idris2-raw-warnings)
      (let* ((fullpath (concat (file-name-as-directory idris2-process-current-working-directory)
                               filename))
             (buffer (get-file-buffer fullpath)))
        (when (not (null buffer))
          (with-current-buffer buffer
            (save-restriction
              (widen) ;; Show errors at the proper location in narrowed buffers
              (goto-char (point-min))
              (cl-multiple-value-bind (startp endp) (idris2-get-line-region startline)
                (goto-char startp)
                (let ((start (+ startp startcol))
                      (end (if (and (= startline endline) (= startcol endcol))
                               ;; this is a hack to have warnings reported which point to empty lines
                               (if (= startp endp)
                                   (progn (insert " ")
                                          (1+ endp))
                                 endp)
                             (+ (save-excursion
                                  (goto-char (point-min))
                                  (line-beginning-position endline))
                                endcol)))
                      (overlay (idris2-warning-overlay-at-point)))
                  (if overlay
                      (idris2-warning-merge-overlays overlay message)
                    (idris2-warning-create-overlay start end message)))))))))))

(defun idris2-warning-merge-overlays (overlay message)
  (overlay-put overlay 'help-echo
               (concat (overlay-get overlay 'help-echo) "\n" message)))

(defun idris2-warning-create-overlay (start end message)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'idris2-warning message)
    (overlay-put overlay 'help-echo message)
    (overlay-put overlay 'face 'idris2-warning-face)
    (overlay-put overlay 'mouse-face 'highlight)
    (push overlay idris2-warnings)
    (unless (memq (current-buffer) idris2-warnings-buffers)
      (push (current-buffer) idris2-warnings-buffers))
    overlay))

(provide 'idris2-warnings)


