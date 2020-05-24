;;; idris2-simple-indent.el --- Simple indentation module for Idris2 Mode

;; Copyright (C) 1998 Heribert Schuetz, Graeme E Moss

;; Author: Heribert Schuetz <Heribert.Schuetz@informatik.uni-muenchen.de>
;; Graeme E Moss <gem@cs.york.ac.uk>
;; Hacked for Idris2 by David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: indentation files Idris2

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;; To support simple indentation of Idris2 scripts.
;;
;;
;; Installation:
;;
;; To bind TAB to the indentation command for all Idris2 buffers, add
;; this to .emacs:
;;
;; (add-hook 'idris2-mode-hook 'turn-on-idris2-simple-indent)
;;
;; Otherwise, call `turn-on-idris2-simple-indent'.
;;
;;
;; Customisation:
;;
;; None supported.
;;
;;
;; History:
;;
;; Version 0.1 - Initial version:
;; Imported from haskell-simple-indent 1.2
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; (None so far.)

;;; Code:

;; All functions/variables start with
;; `(turn-(on/off)-)idris2-simple-indent'.

(require 'idris2-common-utils)

(defgroup idris2-simple-indent nil
  "Simple Idris2 indentation."
  :link '(custom-manual "(idris2-mode)Indentation")
  :group 'idris2
  :prefix "idris2-simple-indent-")

;; Version.
(defconst idris2-simple-indent-version "0.1"
  "`idris2-simple-indent' version number.")
(defun idris2-simple-indent-version ()
  "Echo the current version of `idris2-simple-indent' in the minibuffer."
  (interactive)
  (message "Using idris2-simple-indent version %s"
           idris2-simple-indent-version))

(defun idris2-simple-indent-current-indentation ()
  "Return the indentation of the current line, taking into account literate Idris2 syntax"
  (save-excursion
    (move-to-column 0)
    (looking-at (if (idris2-lidr-p) ">\\s-*" "\\s-*"))
    (length (match-string 0))))

(defun idris2-simple-indent-indent-line-to (column)
  "Just like `indent-line-to`, but ignoring the leading > for literate Idris2"
  (if (idris2-lidr-p)
      (if (save-excursion (move-to-column 0) (looking-at ">")) ;; lidr code line - look out for >
          (progn
            (save-excursion (move-to-column 0)
                            (re-search-forward ">\\s-*" nil t)
                            (replace-match (concat ">"
                                                   (if (<= column 1)
                                                       " "
                                                     (make-string (1- column) ? ))) nil nil))
            (move-to-column column))
        (indent-line-to column)) ;; in text part, do normal indent
    (indent-line-to column))) ;; not lidr, do normal indent

(defun idris2-simple-indent-tab-to-tab-stop ()
  "A version of `tab-to-tab-stop' that takes literate Idris2 into account"
  (let ((indent (idris2-simple-indent-current-indentation))
        (stops tab-stop-list)
        indent-to)
    (while (and stops (>= indent (car stops)))
      (setq stops (cdr stops)))
    (setq indent-to (if stops (car stops) 0))
    (idris2-simple-indent-indent-line-to indent-to)))

;; Partly stolen from `indent-relative' in indent.el:
(defun idris2-simple-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point. A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line. If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  ;; just insert two spaces for lidr if not on a code line
  (if (and (idris2-lidr-p)
           (save-excursion (beginning-of-line) (not (looking-at-p ">"))))
      (progn (beginning-of-line)
             (insert "  ")
             (skip-chars-forward " "))
    ;; otherwise we're in code - do code indenting
    (let* ((start-column (current-column))
           (invisible-from nil) ; `nil' means infinity here
           (indent
            (catch 'idris2-simple-indent-break
              (save-excursion
                (while (progn (beginning-of-line)
                              (not (bobp)))
                  (forward-line -1)
                  (if (not (if (idris2-lidr-p)           ;; if this line isn't whitespace-only
                               (or (looking-at ">[ \t]*\n") ;; whitespace only
                                   (looking-at "[^>]"))     ;; not code
                             (looking-at "[ \t]*\n")))
                      (let ((this-indentation (idris2-simple-indent-current-indentation)))
                        (if (or (not invisible-from)
                                (< this-indentation invisible-from))
                            (if (> this-indentation start-column)
                                (setq invisible-from this-indentation)
                              (let ((end (line-beginning-position 2)))
                                (skip-chars-forward " \t" end)
                                ;; Current indent + 2 is a valid stop
                                (if (= (current-column) start-column)
                                    (forward-char 2)
                                  (progn
                                    (move-to-column start-column)
                                    ;; Is start-column inside a tab on this line?
                                    (if (> (current-column) start-column)
                                        (backward-char 1))
                                    (or (looking-at "[ \t]")
                                        (skip-chars-forward "^ \t" end))
                                    (skip-chars-forward " \t" end)))
                                (let ((col (current-column)))
                                  (throw 'idris2-simple-indent-break
                                         (if (or (= (point) end)
                                                 (and invisible-from
                                                      (> col invisible-from)))
                                             invisible-from
                                           col)))))))))))))
      (if indent
          (let ((opoint (point-marker)))
            (idris2-simple-indent-indent-line-to indent)
            (if (> opoint (point))
                (goto-char opoint))
            (set-marker opoint nil))
        (idris2-simple-indent-tab-to-tab-stop)))))

(defun idris2-simple-indent-backtab ()
  "Indent backwards. Dual to `idris2-simple-indent'."
  (interactive)
  (let ((current-indent (idris2-simple-indent-current-indentation))
        (indent-to (cons 0 0)))
    (idris2-simple-indent-indent-line-to 0)
    (while (and (< (idris2-simple-indent-current-indentation) (1- current-indent))
                (<= (cdr indent-to) (car indent-to)))
      (idris2-simple-indent)
      (setf (cdr indent-to) (car indent-to))
      (setf (car indent-to) (idris2-simple-indent-current-indentation)))
    (idris2-simple-indent-indent-line-to (cdr indent-to))))


(defun idris2-simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive)
  (let ((point (point)))
    (let ((start-end
           (save-excursion
             (let* ((start (line-beginning-position))
                    (end (progn (goto-char start)
                                (search-forward-regexp
                                 "[^ ]" (line-end-position) t 1))))
               (when end (cons start (1- end)))))))
      (if start-end
          (progn (newline)
                 (insert (buffer-substring-no-properties
                          (car start-end) (cdr start-end))))
        (newline)))))


;;;###autoload
(define-minor-mode idris2-simple-indent-mode
  "Simple Idris2 indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `idris2-simple-indent-backtab'
\ (bound to <backtab> by default) will move the cursor the
previous indent point. An indent point is a non-whitespace
character following whitespace.

Runs `idris2-simple-indent-hook' on activation."
  :lighter " Ind"
  :group 'idris2-simple-indent
  :keymap '(([backtab] . idris2-simple-indent-backtab))
  (kill-local-variable 'indent-line-function)
  (when idris2-simple-indent-mode
    (set (make-local-variable 'indent-line-function) 'idris2-simple-indent)
    (run-hooks 'idris2-simple-indent-hook)))

;; The main functions.
;;;###autoload
(defun turn-on-idris2-simple-indent ()
  "Turn on function `idris2-simple-indent-mode'."
  (interactive)
  (idris2-simple-indent-mode))

(defun turn-off-idris2-simple-indent ()
  "Turn off function `idris2-simple-indent-mode'."
  (interactive)
  (idris2-simple-indent-mode 0))

;; Provide ourselves:

(provide 'idris2-simple-indent)

;;; idris2-simple-indent.el ends here
