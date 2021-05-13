;;; idris2-info.el --- Facilities for showing Idris2 help information -*- lexical-binding: t -*-

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains facilities for showing information provided by the
;; Idris2 compiler in a separate buffer, as well as keeping the irritation of
;; that buffer to a minimum.

;;; Code:
(require 'prop-menu)
(require 'idris2-core)
(require 'idris2-common-utils)


(defvar idris2-info-history (list () nil ())
  "A zipper into the history for idris2-info-mode.
It is a three-element list whose first element is the history,
whose second element is the current item if applicable or NIL
otherwise, and whose third element is the future.")

(defun idris2-info-history-clear ()
  "Reset the history for Idris2 info buffers."
  (setq idris2-info-history (list () nil ())))

(defun idris2-info-history-insert (contents)
  "Insert CONTENTS into the Idris2 info history as the current node.
Following the behavior of Emacs help buffers, the future is deleted."
  (pcase-let ((`(,past ,present ,_future) idris2-info-history))
    (setq idris2-info-history
          (if present
              (list (cons present past) contents ())
            (list past contents ())))))

(defun idris2-info-history-back ()
  "Move back in the Idris2 info history."
  (setq idris2-info-history
        (pcase idris2-info-history
          (`((,prev . ,past) ,present ,future)
           (list past prev (cons present future)))
          (`(() ,present ,future) (list () present future)))))

(defun idris2-info-history-forward ()
  "Move forward in the Idris2 info history."
  (setq idris2-info-history
        (pcase idris2-info-history
          (`(,past ,present (,next . ,future))
           (list (cons present past) next future))
          (`(,past ,present ()) (list past present ())))))

(defvar idris2-info-buffer-name (idris2-buffer-name :info)
  "The name of the buffer containing Idris2 help information")

(defvar idris2-buffer-to-return-to-from-info-buffer
  "The buffer that should be returned to when the info buffer is closed.")


(defvar idris2-info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    (define-key map (kbd "q") 'idris2-info-quit)
    ;;; Allow buttons to be clicked with the left mouse button in info buffers
    (define-key map [follow-link] 'mouse-face)
    (cl-loop for keyer
             in '(idris2-define-docs-keys
                  idris2-define-general-keys
                  idris2-define-active-term-keys)
             do (funcall keyer map))
    map))

(easy-menu-define idris2-info-mode-menu idris2-info-mode-map
  "Menu for the Idris2 info buffer"
  `("Idris2 Info"
    ["Show term interaction widgets" idris2-add-term-widgets t]
    ["Close Idris2 info buffer" idris2-info-quit t]))

(define-derived-mode idris2-info-mode fundamental-mode "Idris2 Info"
  "Major mode used for transient Idris2 information buffers
    \\{idris2-info-mode-map}
Invokes `idris2-info-mode-hook'."
  (set (make-local-variable 'prop-menu-item-functions) '(idris2-context-menu-items)))
; if we use view-mode here, our key binding q would be shadowed.

(defun idris2-info-buffer ()
  "Return the Idris2 info buffer, creating one if there is not one.
Ensure that the buffer is in `idris2-info-mode'."
  (let ((buffer (get-buffer-create idris2-info-buffer-name)))
    (with-current-buffer buffer
      (when (not (eq major-mode 'idris2-info-mode))
        (idris2-info-mode)))
    buffer))

(defun idris2-info-quit ()
  "Exits the info window. Tries to go back to the previous window and buffer before it was opened."
  (interactive)
  (idris2-kill-buffer idris2-info-buffer-name)
  (if (and idris2-buffer-to-return-to-from-info-buffer (buffer-live-p idris2-buffer-to-return-to-from-info-buffer))
      (pop-to-buffer idris2-buffer-to-return-to-from-info-buffer `(display-buffer-reuse-window))
    ()
    )
  (setq idris2-buffer-to-return-to-from-info-buffer nil)
  )

(defun idris2-info-buffer-visible-p ()
  (if (get-buffer-window idris2-info-buffer-name 'visible) t nil))

(defun idris2-info-show ()
  "Show the Idris2 info buffer. Updates idris2-buffer-to-return-to-from-info-buffer to current buffer"
  (interactive)
  (setq idris2-buffer-to-return-to-from-info-buffer (current-buffer))
  (with-current-buffer (idris2-info-buffer)
    (setq buffer-read-only t)
    (pcase-let ((inhibit-read-only t)
                (`(,past ,present ,future) idris2-info-history))
      (erase-buffer)
      (when present
        (insert present)
        (insert "\n\n"))
      (when past
        (insert-button "[back]" 'action #'(lambda (_) (interactive) (idris2-info-history-back) (idris2-info-show))))
      (when (and past future) (insert "\t"))
      (when future
        (insert-button "[forward]" 'action #'(lambda (_) (interactive) (idris2-info-history-forward) (idris2-info-show))))
      (when (or past future) (newline))
      (goto-char (point-min))))
  (unless (idris2-info-buffer-visible-p)
    (pop-to-buffer (idris2-info-buffer))
    (message "Press q to close the Idris2 info buffer."))
  (pop-to-buffer idris2-buffer-to-return-to-from-info-buffer))

(defmacro with-idris2-info-buffer (&rest cmds)
  "Execute `CMDS' in a fresh Idris2 info buffer, then display it to the user."
  (declare (indent defun))
  (let ((str-info (cl-gensym "STR-INFO")))
    `(let ((,str-info (with-temp-buffer
                        ,@cmds
                        (buffer-string))))
       (idris2-info-history-insert ,str-info)
       (idris2-info-show))))


(defun idris2-show-info (info-string &optional spans)
  "Show INFO-STRING in the Idris2 info buffer, obliterating its previous contents."
  (with-idris2-info-buffer
    (idris2-propertize-spans (idris2-repl-semantic-text-props spans)
      (insert info-string)))
  info-string)



(provide 'idris2-info)
;;; idris2-info.el ends here
