;;; idris2-prover.el --- Prover mode for Idris2 -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014, Hannes Mehnert and David Raymond Christiansen
;; Author: Hannes Mehnert <hannes@mehnert.org>, David Raymond Christiansen <david@davidchristiansen.dk>

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
(require 'idris2-warnings)
(require 'idris2-settings)
(require 'inferior-idris2)

(eval-when-compile (require 'cl-lib))

; consisting of three buffers:
; ------------------------------
; | proof obligations          |
; |----------------------------|
; | proof shell | proof script |
; ------------------------------

(defgroup idris2-prover nil "Idris2 Prover" :prefix 'idris2 :group 'idris2)

(defface idris2-prover-processed-face
  '((t (:background "PaleGreen1")))
  "Face for Idris2 proof script which is already processed."
  :group 'idris2-faces)

(defface idris2-prover-processing-face
  '((t (:background "gold")))
  "Face for Idris2 proof script which is currently processing."
  :group 'idris2-faces)

(defcustom idris2-prover-restore-window-configuration t
  "When non-nil, restore the window configuration after exiting
the prover."
  :type 'boolean
  :group 'idris2-prover)

(defvar idris2-prover-obligations-buffer-name (idris2-buffer-name :proof-obligations)
  "The name of the Idris2 proof obligation buffer.")

(defvar idris2-prover-shell-buffer-name (idris2-buffer-name :proof-shell)
  "The name of the Idris2 proof shell buffer.")

(defvar idris2-prover-script-buffer-name (idris2-buffer-name :proof-script)
  "The name of the Idris2 proof script buffer.")

(defvar idris2-prover-currently-proving nil
  "The hole that Idris2 has open in the interactive prover, or nil
if Idris2 is not proving anything.")

(defconst idris2-prover-error-message-prefix "Prover error: "
  "A prefix to show on minibuffer error messages that originate
  in the prover.")

(defun idris2-prover-obligations-buffer ()
  (or (get-buffer idris2-prover-obligations-buffer-name)
      (let ((buffer (get-buffer-create idris2-prover-obligations-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t))
        buffer)))

(defun idris2-prover-show-obligations ()
  (display-buffer (idris2-prover-obligations-buffer)
     '(display-buffer-pop-up-window . nil)))

(defun idris2-prover-write-goals (goals)
  "Write GOALS to the goal buffer.
If GOALS is a string, it is treated as undecorated text.
Otherwise, it must be a two-element list whose car is a goal
string and whose cadr is highlighting information."
  (with-current-buffer (idris2-prover-obligations-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (when idris2-show-help-text
        (setq header-line-format
              "This is a read-only view of your proof state. Prove the lemma in the script buffer."))
      (let ((goals-string (car goals))
            (goals-spans (cadr goals)))
        (idris2-propertize-spans (idris2-repl-semantic-text-props goals-spans)
          (insert goals-string)))))
  (idris2-prover-show-obligations))

(defvar idris2-prover-saved-window-configuration nil
  "The saved window configuration from before running the prover.")

(defvar-local idris2-prover-script-processed nil
  "Marker for the processed part of proof script")

(defvar-local idris2-prover-script-processed-overlay nil
  "Overlay for processed proof script")

(defvar-local idris2-prover-script-processing nil
  "Marker for the processing part of proof script")

(defvar-local idris2-prover-script-processing-overlay nil
  "Overlay for processing proof script")

(defvar-local idris2-prover-script-warning-overlay nil
  "Overlay for warning in proof script")

; invariant: point-min <= idris2-prover-script-processed <= idris2-prover-script-processing <= point-max

(defvar-local idris2-prover-prove-step 0
  "Step counter of the proof")

(defvar idris2-prover-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'idris2-prover-script-ret)
    (define-key map (kbd "M-n") 'idris2-prover-script-forward)
    (define-key map (kbd "M-p") 'idris2-prover-script-backward)
    (define-key map (kbd "C-c C-q") 'idris2-prover-script-qed)
    (define-key map (kbd "C-c C-k") 'idris2-prover-abandon)
    ;; Using (kbd "<TAB>") in place of "\t" makes emacs angry, and suggests
    ;; using the latter form.
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap used in Idris2 proof script mode.")


(defun idris2-prover-complete ()
  "Completion of the current input."
  (let* ((start (save-excursion (beginning-of-line) (point)))
         (input (buffer-substring-no-properties
                 start
                 (point)))
         (result (car (idris2-eval `(:repl-completions ,input) t))))
    (and result
	 (cl-destructuring-bind (completions _partial) result
	   (unless (null completions)
	     (list start (point) completions))))))

(defun idris2-prover-find-tactic (start-pos)
  "Use some layout heuristics to find the tactic beginning at
START-POS, returning a pair consisting of the start and end
positions of the tactic. Tactics are required to begin at the
left margin."
  (let (tactic-start tactic-end)
    (save-excursion
      (goto-char start-pos)

      ;; Ensure that we're at the next line beginning
      (beginning-of-line)
      (unless (= (point) start-pos)
        (forward-line))

      ;; Go forward until the current line begins a tactic
      (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
        (forward-line))

      (unless (eobp) ;; if at end of buffer, no tactic to be found!
        (setq tactic-start (point))

        ;; Go forward until end of buffer or non-blank line at left margin
        (forward-line)
        (while (and (not (eobp)) (not (looking-at-p "[a-zA-Z]")))
          (forward-line))

        ;; Go backward until a non-whitespace char is found - it is the end of
        ;; the tactic
        (backward-char)
        (while (looking-at-p "\\s-\\|$") (backward-char))
        (forward-char)
        (setq tactic-end (point))

        (cons tactic-start tactic-end)))))

(defun idris2-prover-script-backward ()
  "Backward one piece of proof script"
  (interactive)
  (idris2-eval-async (list :interpret (if idris2-enable-elab-prover ":undo" "undo"))
                     #'(lambda (_result) t)
                     #'(lambda (condition)
                         (message (concat idris2-prover-error-message-prefix condition)))))

(defun idris2-prover-script-forward ()
  "Forward one piece of proof script."
  (interactive)
  (when (eobp) (newline)) ;; There must be a newline at the end
  (when idris2-prover-script-warning-overlay
    (delete-overlay idris2-prover-script-warning-overlay)
    (setq idris2-prover-script-warning-overlay nil))
  (goto-char (+ 1 idris2-prover-script-processed))
  (let ((prior-processed-position (marker-position idris2-prover-script-processed)) ; to restore on error
        (next-tactic (idris2-prover-find-tactic
                      idris2-prover-script-processed)))
    (if (null next-tactic)
        (error "At the end of the proof script")
      (let* ((tactic-start (car next-tactic))
             (tactic-end (cdr next-tactic))
             (tactic-text (buffer-substring-no-properties tactic-start
                                                          tactic-end)))
        (set-marker idris2-prover-script-processed tactic-start)
        (set-marker idris2-prover-script-processing tactic-end)
        (let ((overlay (make-overlay idris2-prover-script-processed
                                     idris2-prover-script-processing)))
          (overlay-put overlay 'face 'idris2-prover-processing-face)
          (setq idris2-prover-script-processing-overlay overlay))
        (with-no-warnings
          (let ((tactic-cmd (replace-regexp-in-string
                             "\\`[ \t\n]*" ""
                             ;; replace Windows newlines with a space
                             (replace-regexp-in-string "" " " tactic-text))))
            (idris2-rex () (list :interpret tactic-cmd) nil
              ((:ok _result)
               (with-current-buffer (idris2-prover-script-buffer)
                 (when idris2-prover-script-processing-overlay
                   (delete-overlay idris2-prover-script-processing-overlay)
                   (setq idris2-prover-script-processing-overlay nil))
                 ;; Delete the region because it will be or has been
                 ;; written with the proof state.
                 (delete-region idris2-prover-script-processed
                                idris2-prover-script-processing)
                 ;; Put point at a useful spot for the next tactic
                 (when (eql (marker-position idris2-prover-script-processed) (point-max))
                   (goto-char idris2-prover-script-processed)
                   (let ((inhibit-read-only t)) (insert "\n")))
                 (goto-char (1+ (marker-position idris2-prover-script-processed)))
                 (recenter)))
              ((:error condition &optional _spans)
               (with-current-buffer (idris2-prover-script-buffer)
                 (when idris2-prover-script-processing-overlay
                   (delete-overlay idris2-prover-script-processing-overlay)
                   (setq idris2-prover-script-processing-overlay nil))
                 (setq idris2-prover-script-warning-overlay
                       (idris2-warning-create-overlay idris2-prover-script-processed
                                                      idris2-prover-script-processing
                                                      condition))
                 ;; Restore the original position of the marker for
                 ;; the processed region to prevent Emacs and Idris2
                 ;; from getting out of sync RE proof script contents
                 (set-marker idris2-prover-script-processed prior-processed-position))
               (message (concat idris2-prover-error-message-prefix condition))
               t))))))))

(defun idris2-prover-script-ret ()
  "Insert a newline at the end of buffer, even if it's read-only."
  (interactive)
  (if (equal (point) (marker-position idris2-prover-script-processed))
      (let ((inhibit-read-only t)) (insert "\n"))
    (newline)))

(defun idris2-prover-script-qed ()
  "Send a QED command to Idris2."
  (interactive)
  (if idris2-prover-currently-proving
      (idris2-eval-async (list :interpret (if idris2-enable-elab-prover ":qed" "qed"))
                         #'(lambda (_result) t)
                         #'(lambda (condition)
                             (message (concat idris2-prover-error-message-prefix condition))))
    (error "No proof in progress")))

(easy-menu-define idris2-prover-script-mode-menu idris2-prover-script-mode-map
  "Menu for Idris2 prover scripts"
  `("Idris2 Proof"
    ["Advance" idris2-prover-script-forward t]
    ["Retract" idris2-prover-script-backward t]
    ["QED" idris2-prover-script-qed t]
    ["Abandon" idris2-prover-abandon t]))

(define-derived-mode idris2-prover-script-mode prog-mode "Idris2-Proof-Script"
  "Major mode for interacting with Idris2 proof script.
    \\{idris2-prover-script-mode-map}
Invokes `idris2-prover-script-mode-hook'."
  :group 'idris2-prover
  (set (make-local-variable 'completion-at-point-functions)
       '(idris2-prover-complete))
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun idris2-prover-script-buffer ()
  (or (get-buffer idris2-prover-script-buffer-name)
      (let ((buffer (get-buffer-create idris2-prover-script-buffer-name)))
        (with-current-buffer buffer
          (idris2-prover-script-mode)
          (setq idris2-prover-script-processing (make-marker))
          (setq idris2-prover-script-processed (make-marker))
          (set-marker idris2-prover-script-processing (point))
          (set-marker idris2-prover-script-processed (point)))
        buffer)))

(defun idris2-prover-reset-prover-script-buffer ()
  "Erase or initialize a proof script buffer, resetting all the
special prover state."
  (with-current-buffer (idris2-prover-script-buffer)
    (when idris2-prover-script-processed-overlay
      (delete-overlay idris2-prover-script-processed-overlay)
      (setq idris2-prover-script-processed-overlay nil))
    (when idris2-prover-script-processing-overlay
      (delete-overlay idris2-prover-script-processing-overlay)
      (setq idris2-prover-script-processing-overlay nil))
    (setq idris2-prover-prove-step 0)
    (erase-buffer)
    (when idris2-show-help-text
      (setq header-line-format
            (let ((fwd (where-is-internal 'idris2-prover-script-forward))
                  (bak (where-is-internal 'idris2-prover-script-backward))
                  (qed (where-is-internal 'idris2-prover-script-qed)))
              (concat " Write your proof script here."
                      (if (and fwd bak qed)
                          (format "Use %s to advance and %s to retract.  %s saves a completed proof."
                                  (key-description (car fwd))
                                  (key-description (car bak))
                                  (key-description (car qed)))
                        "")))))
    (unless idris2-prover-script-processing
      (setq idris2-prover-script-processing (make-marker)))
    (unless idris2-prover-script-processed
      (setq idris2-prover-script-processed (make-marker)))
    (set-marker idris2-prover-script-processing (point))
    (set-marker idris2-prover-script-processed (point))))

(defun idris2-prover-write-script (script count)
  "Put the proof state recieved from Idris2 into the proof script buffer.
SCRIPT is the list of tactics in the proof state, and COUNT is
the length reported by Idris2."
  (interactive)
  (with-current-buffer (idris2-prover-script-buffer)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point-max) 'read-only nil))
    (cond ((< count idris2-prover-prove-step)
           ;; this is actually the (count - 1) == Idris2-prover-prove-step case!
           ;; in other words, we are undoing the final step.
           ;; can the other case(s) happen??
           (goto-char idris2-prover-script-processed)
           (if (= (forward-line -1) 0)
               ;; we went back
               (end-of-line)
             ;; we did not go back - are thus at top of buffer and must
             ;; retract whole script
             (goto-char (point-min)))
           (set-marker idris2-prover-script-processed (point)))
          ((> count idris2-prover-prove-step)
           ;; Here we are inserting a newly-checked proof step.
           (goto-char idris2-prover-script-processed)
           (while (< idris2-prover-prove-step count)
             (let ((lelem (nth idris2-prover-prove-step script)))
               (insert-before-markers lelem))
             (newline)
             (setq idris2-prover-prove-step (1+ idris2-prover-prove-step))))
          (t nil))
    (setq idris2-prover-prove-step count)
    (unless (null idris2-prover-script-processed-overlay)
      (delete-overlay idris2-prover-script-processed-overlay))
    (let ((overlay (make-overlay 0 idris2-prover-script-processed)))
      (overlay-put overlay 'face 'idris2-prover-processed-face)
      (setq idris2-prover-script-processed-overlay overlay))
    (let ((inhibit-read-only t))
      (put-text-property (point-min) idris2-prover-script-processed 'read-only t))))

(defun idris2-prover-abandon ()
  "Abandon an in-progress proof."
  (interactive)
  ;; Ask for confirmation when called interactively
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Abandon proof and discard script? "))
    (if idris2-prover-currently-proving
        (idris2-eval (list :interpret (if idris2-enable-elab-prover ":abandon" "abandon")) t)
      (error "No proof in progress"))))

(defun idris2-prover-end ()
  "Get rid of left over buffers from proof mode and unset global state related to the prover."
  (interactive)
  (setq idris2-prover-currently-proving nil)
  (let ((obligations (idris2-prover-obligations-buffer))
        (script (idris2-prover-script-buffer)))
    (when obligations
      (delete-windows-on obligations)
      (kill-buffer obligations))
    (when script (kill-buffer script)))
  (when (and idris2-prover-restore-window-configuration
             (window-configuration-p
              idris2-prover-saved-window-configuration))
    (set-window-configuration idris2-prover-saved-window-configuration))
  (setq idris2-prover-saved-window-configuration nil))

(autoload 'idris2-repl-write-string "idris2-repl.el")
(defun idris2-prover-event-hook-function (event)
  "Process an EVENT returned from Idris2 when the prover is running."
  (pcase event
    (`(:start-proof-mode ,name ,_target)
     (setq idris2-prover-currently-proving name)
     (setq idris2-prover-saved-window-configuration
           (current-window-configuration))
     (idris2-prover-reset-prover-script-buffer)
     (idris2-repl-write-string (format "Start proof of %s" name))
     (let* ((obligations-window (idris2-prover-show-obligations))
            (script-window (split-window obligations-window)))
       (set-window-buffer script-window (idris2-prover-script-buffer)))
     t)
    (`(:end-proof-mode ,msg ,_target)
     (let ((name (car msg))
           (proof (cadr msg)))
       (idris2-perhaps-insert-proof-script proof)
       (idris2-prover-end)
       (idris2-repl-write-string (concat "End proof of " name))
       (run-hooks 'idris2-prover-success-hook))
     t)
    (`(:write-proof-state ,msg ,_target)
     (cl-destructuring-bind (script count) msg
       (idris2-prover-write-script script count))
     t)
    (`(:write-goal ,goal ,_target)
     (idris2-prover-write-goals goal)
     t)
    (`(:abandon-proof ,_msg ,_target)
     (when (get-buffer idris2-prover-script-buffer-name)
       (with-current-buffer idris2-prover-script-buffer-name
         (copy-region-as-kill (point-min) (point-max))
         (message "Proof saved to kill ring")))
     (idris2-prover-end)
     (idris2-repl-write-string "Abandoned proof")
     t)
    (_ nil)))

(defcustom idris2-prover-success-hook '(idris2-list-holes-on-load)
  "Functions to call when completing a proof"
  :type 'hook
  :options '(idris2-list-holes-on-load)
  :group 'idris2-prover)

(defun idris2-perhaps-insert-proof-script (proof)
  "Prompt the user as to whether PROOF should be inserted into the buffer."
  (save-window-excursion
    (pop-to-buffer idris2-currently-loaded-buffer)
    (delete-other-windows)
    (let ((proof-buffer (get-buffer-create "*idris2-finished-proof*")))
      (unwind-protect
          (progn
            (pop-to-buffer proof-buffer)
            (insert proof)
            (if (y-or-n-p "Keep this proof script?")
                (idris2-insert-proof-script idris2-currently-loaded-buffer proof)
              (kill-new proof)
              (message "Proof saved to kill ring")))
        (kill-buffer proof-buffer)))))

(defconst idris2-proof-script-insertion-marker "---------- Proofs ----------"
  "Look for this marker to insert proofs. Should agree with the
  one in the Idris2 compiler source.")

(defun idris2-insert-proof-script (buffer proof)
  "Insert PROOF into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward idris2-proof-script-insertion-marker nil t)
        (when (re-search-forward "\\(\\s-*\n\\)*\\'")
          (replace-match (concat "\n\n" idris2-proof-script-insertion-marker "\n") nil nil)))
      (newline)
      (insert proof)
      (newline))))

(provide 'idris2-prover)
