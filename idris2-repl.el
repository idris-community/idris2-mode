;;; idris2-repl.el --- Run an Idris2 interpreter using S-Expression communication protocol.-*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert and David Raymond Christiansen

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

;;; Code:

(require 'cl-lib)
(require 'prop-menu)

(require 'idris2-core)
(require 'idris2-settings)
(require 'inferior-idris2)
(require 'idris2-common-utils)
(require 'idris2-prover)
(require 'idris2-highlight-input)

;; Quick and dirty calculation for high-contrast between the logo's
;; font and the background, based on W3C's advice
;;   https://www.w3.org/TR/2013/NOTE-WCAG20-TECHS-20130905/G18

;; We use emacs's hue-saturation-luminance representation to calculate
;; luminance rather than the w3's formula
(require 'color)

(defun idris2-luminance (rgb)
  "Return the luminance of an RGB color"
  (caddr (apply 'color-rgb-to-hsl rgb)))

(defun idris2-contrast-ratio-against-black (color-name)
  "Calculate the contrast ratio of two colors given by name
  relative to black (the foreground color of the idris logo)."
  (/ (+ 0.05 (idris2-luminance (color-name-to-rgb color-name)))
     ; Black has luminance 0.0
     0.05))

(defun idris2-get-bg-color () (face-attribute 'default :background))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))


(defvar idris2-prompt-string "Idris2"
  "The prompt shown in the REPL.")

(defvar idris2-repl-buffer-name (idris2-buffer-name :repl)
  "The name of the Idris2 REPL buffer.")

(defvar-local idris2-prompt-start nil
  "Marker for the start of the Idris2 prompt.")

(defvar-local idris2-input-start nil
  "Marker for the start of user input for Idris2.")

(defun idris2-repl-welcome-message ()
  "The message to display as part of the Idris2 banner, if applicable."
  "Welcome to the Idris2 REPL!")

(defun idris2-repl-get-logo ()
  "Return the path to the Idris2 logo if it exists, or `nil' if not."
  (let* ((logo-path-light (concat idris2-mode-path "logo-small.png"))
          (logo-path-dark  (concat idris2-mode-path "logo-small-dark-contrast.png"))
          (logo-path       (if (< (idris2-contrast-ratio-against-black
                                   (idris2-get-bg-color))
                                  6.0)
                               logo-path-dark
                             logo-path-light)))
    (if (file-readable-p logo-path)
        logo-path
      nil)))

(defun idris2-repl-insert-logo ()
  "Attempt to insert a graphical logo.
Returns non-`nil' on success, `nil' on failure."
  (let ((logo (idris2-repl-get-logo)))
    (if (and (display-graphic-p)
             (image-type-available-p 'png)
             logo)
        (progn (insert-image (create-image logo)
                             (idris2-repl-welcome-message))
               t)
      nil)))

(defun idris2-repl-animate-banner ()
  "Insert a text banner using animation.
Returns non-`nil' on success, `nil' on failure."
  (animate-string (idris2-repl-welcome-message) 0 0)
  t)

(defun idris2-repl-text-banner ()
  "Insert a text banner with no animation.
Returns non-`nil' on success, `nil' on failure."
  (insert (idris2-repl-welcome-message))
  t)

(defun idris2-repl-insert-banner ()
  "Insert Idris2 banner into buffer."
  (when (zerop (buffer-size))
    ;; If a banner is inserted, add a newline too
    (when (run-hook-with-args-until-success 'idris2-repl-banner-functions)
      (insert "\n"))
    (let ((version-string (idris2-get-idris2-version-string)))
      (when (and idris2-repl-show-idris2-version
                 version-string)
        (insert (propertize (concat "Idris2 " version-string)
                            'face 'italic)
                "\n\n")))))

(defun idris2-repl-insert-prompt (&optional always-insert)
  "Insert or update Idris2 prompt in buffer.
If ALWAYS-INSERT is non-nil, always insert a prompt at the end of the buffer."
  ;; Put the prompt at the end, if no active prompt is present.
  (when always-insert
    (set-marker idris2-prompt-start (point-max))
    (set-marker idris2-input-start (point-max)))
  (goto-char idris2-prompt-start)
  (let ((inhibit-read-only 'idris2-repl-prompt))
    (delete-region idris2-prompt-start idris2-input-start))
  (unless (bolp) (insert "\n"))
  (let ((prompt (if (and (equal idris2-repl-prompt-style 'short)
                         (not idris2-prover-currently-proving))
                    "λΠ> "
                  (format "%s> " idris2-prompt-string))))
    (set-marker idris2-prompt-start (point))
    (idris2-propertize-region
        `(face idris2-repl-prompt-face
               read-only idris2-repl-prompt
               intangible t
               idris2-repl-prompt t
               help-echo ,idris2-prompt-string
               rear-nonsticky (idris2-repl-prompt read-only face intangible))
      (let ((inhibit-read-only t))
        (insert prompt)))
    (set-marker idris2-input-start (point-max))
    (goto-char idris2-input-start)))


(defun idris2-repl-update-prompt (new-prompt)
  "Update prompt string to NEW-PROMPT."
  (unless (equal idris2-prompt-string new-prompt)
    (setq idris2-prompt-string new-prompt)
    (with-current-buffer (idris2-repl-buffer)
      (idris2-repl-insert-prompt))))


(defun idris2-repl-buffer ()
  "Return or create the Idris2 REPL buffer."
  (or (get-buffer idris2-repl-buffer-name)
      (let ((buffer (get-buffer-create idris2-repl-buffer-name)))
        (save-selected-window
          (when idris2-repl-show-repl-on-startup
            (pop-to-buffer buffer t))
          (with-current-buffer buffer
            (idris2-repl-mode)
            (idris2-repl-buffer-init))
          buffer))))

(defun idris2-repl-clear-buffer ()
  "Clear prior output from the Idris2 REPL buffer."
  (interactive)
  (with-current-buffer (idris2-repl-buffer)
    (let ((inhibit-read-only t)
          (current-input (idris2-repl-current-input)))
      (erase-buffer)
      (idris2-repl-insert-prompt)
      (insert current-input))))

(defun idris2-switch-to-output-buffer ()
  "Select the output buffer and scroll to bottom."
  (interactive)
  (pop-to-buffer (idris2-repl-buffer))
  (goto-char (point-max)))

;;;###autoload
(defun idris2-repl ()
  (interactive)
  (idris2-run)
  (idris2-switch-to-output-buffer))

(defvar idris2-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") 'idris2-repl-return)
    ;; (define-key map (kbd "<TAB>") ...) makes the debugger complain, and
    ;; suggests this method of binding instead.
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "<home>") 'idris2-repl-begin-of-prompt)
    (define-key map (kbd "C-a") 'idris2-repl-begin-of-prompt)
    (define-key map (kbd "M-p") 'idris2-repl-backward-history)
    (define-key map (kbd "<C-up>") 'idris2-repl-backward-history)
    (define-key map (kbd "M-n") 'idris2-repl-forward-history)
    (define-key map (kbd "<C-down>") 'idris2-repl-forward-history)
    (define-key map (kbd "C-c M-o") 'idris2-repl-clear-buffer)
    (cl-loop for keyer
             in '(idris2-define-docs-keys
                  idris2-define-general-keys
                  idris2-define-active-term-keys)
             do (funcall keyer map))
    map)
  "Keymap used in Idris2 REPL mode.")

(easy-menu-define idris2-repl-mode-menu idris2-repl-mode-map
  "Menu for the Idris2 REPL mode"
  `("Idris2 REPL"
    ("Interpreter options" :active idris2-process
     ["Show implicits" (idris2-set-option :show-implicits t)
      :visible (not (idris2-get-option :show-implicits))]
     ["Hide implicits" (idris2-set-option :show-implicits nil)
      :visible (idris2-get-option :show-implicits)]
     ["Show error context" (idris2-set-option :error-context t)
      :visible (not (idris2-get-option :error-context))]
     ["Hide error context" (idris2-set-option :error-context nil)
      :visible (idris2-get-option :error-context)])
    ["Show term interaction widgets" idris2-add-term-widgets t]
    ["Customize idris2-mode" (customize-group 'idris2) t]
    ["Quit inferior idris2 process" idris2-quit t]
    ))

(define-derived-mode idris2-repl-mode fundamental-mode "Idris2-REPL"
  "Major mode for interacting with Idris2.
    \\{idris2-repl-mode-map}
Invokes `idris2-repl-mode-hook'."
                                        ;syntax-table?
  :group 'idris2-repl
  (set (make-local-variable 'indent-tabs-mode) nil)
  (add-hook 'idris2-event-hooks 'idris2-repl-event-hook-function)
  (add-hook 'kill-buffer-hook 'idris2-repl-remove-event-hook-function nil t)
  (when idris2-repl-history-file
    (idris2-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'idris2-repl-safe-save-history nil t))
  (add-hook 'kill-emacs-hook 'idris2-repl-save-all-histories)
  (set (make-local-variable 'completion-at-point-functions) '(idris2-repl-complete))
  (setq mode-name `("Idris2-REPL" (:eval (if idris2-rex-continuations "!" ""))))
  (set (make-local-variable 'prop-menu-item-functions)
       '(idris2-context-menu-items)))

(defun idris2-repl-remove-event-hook-function ()
  (setq idris2-prompt-string "Idris2")
  (remove-hook 'idris2-event-hooks 'idris2-repl-event-hook-function))

(defun idris2-repl-event-hook-function (event)
  (pcase event
    (`(:write-string ,output ,_target)
     (idris2-repl-write-string output)
     t)
    (`(:set-prompt ,prompt ,_target)
     (idris2-repl-update-prompt prompt)
     t)
    (`(:warning ,output ,_target)
     (when (member 'warnings-repl idris2-warnings-printing)
       (idris2-repl-write-string (format "Error: %s line %d (col %d):\n%s" (nth 0 output) (nth 1 output) (if (eq (safe-length output) 3) 0 (nth 2 output)) (car (last output))))))
    (`(:run-program ,file ,_target)
     (idris2-execute-compiled-program file))
    (_ nil)))

(defun idris2-execute-compiled-program (filename)
  (let* ((name (concat "idris2-" filename))
         (buffer (make-comint-in-buffer name nil filename)))
    (pop-to-buffer buffer)))

(defun idris2-repl-update-banner ()
  (idris2-repl-insert-banner)
  (goto-char (point-max))
  (idris2-repl-insert-prompt t))

(defun idris2-repl-buffer-init ()
  (dolist (markname '(idris2-prompt-start
                      idris2-input-start))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point)))
  (idris2-repl-update-banner))

(defun idris2-repl-return ()
  "Send command over to Idris2."
  (interactive)
  (goto-char (point-max))
  (let ((end (point)))
    (idris2-repl-add-to-input-history (buffer-substring idris2-input-start end))
    (let ((overlay (make-overlay idris2-input-start end)))
      (overlay-put overlay 'face 'idris2-repl-input-face)))
  (let ((input (idris2-repl-current-input))
        (input-start (marker-position idris2-input-start)))
    (goto-char (point-max))
    (if (string-match-p "^\\s-*$" input)
        (delete-region (point) idris2-input-start)
      (insert "\n")
      (set-marker idris2-prompt-start (point))
      (set-marker idris2-input-start (point))
      (idris2-repl-eval-string input input-start))))

(defun idris2-repl-complete ()
  "Completion of the current input"
  (let* ((input (idris2-repl-current-input))
         (result (idris2-eval `(:repl-completions ,input))))
    (cl-destructuring-bind (completions partial) (car result)
      (if (null completions)
          nil
        (list (+ idris2-input-start (length partial)) (point-max) completions)))))

(defun find-common-prefix (input slist)
  "Finds longest common prefix of all strings in list."
  (let ((first (car slist))
        (ilen (length input)))
    (if (> (length first) ilen)
        (progn
          (let ((next (substring first 0 (1+ ilen))))
            (if (cl-every (lambda (p) (string-prefix-p next p)) slist)
                (find-common-prefix next slist)
              input)))
      input)))

(defun idris2-repl-begin-of-prompt ()
  "Got to the beginning of linke or the prompt."
  (interactive)
  (cond ((and (>= (point) idris2-input-start)
              (idris2-same-line-p (point) idris2-input-start))
         (goto-char idris2-input-start))
        (t (beginning-of-line 1))))

(defun idris2-repl-current-input ()
  "Return the current input as string."
  (buffer-substring-no-properties idris2-input-start (point-max)))

(defun idris2-repl-highlight-input (start-pos start-line start-col end-line end-col props)
  "Apply semantic highlighting to the REPL input beginning at START-POS using the Idris2 location information START-LINE, START-COL, END-LINE, and END-COL and semantic annotations PROPS."
  (let ((buffer (get-buffer (idris2-buffer-name :repl))))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((input-line (save-excursion
                             (goto-char start-pos)
                             (beginning-of-line)
                             (count-lines (point-min) start-pos)))
               (input-col (save-excursion
                            (goto-char start-pos)
                            (current-column)))
               (start-line-repl (+ input-line start-line -1))
               (start-col-repl (+ input-col start-col))
               (end-line-repl (+ input-line end-line -1))
               (end-col-repl (+ input-col end-col)))
          (idris2-highlight-input-region buffer
                                        start-line-repl start-col-repl
                                        end-line-repl end-col-repl
                                        props))))))

(defun idris2-repl-eval-string (string start)
  "Evaluate STRING on the inferior Idris2, where input was at position START."
  (idris2-rex (start) (list :interpret string) t
    ((:ok result &optional spans)
     (pcase result
       (`(:highlight-source ,hs) ;; Semantic highlighting
        (when start
          (dolist (h hs)
            ;; Compute positions relative to the input start for
            ;; semantic highlighting
            (pcase h
              (`(((:filename ,_fn)
                  (:start ,start-line ,start-col)
                  (:end ,end-line ,end-col))
                 ,props)
               (idris2-repl-highlight-input
                start start-line start-col end-line end-col props))))))
       (_ (idris2-repl-insert-result result spans)))) ;; The actual result
    ((:error condition &optional spans)
     (idris2-repl-show-abort condition spans))))

(defun idris2-repl-show-abort (condition &optional highlighting)
  (with-current-buffer (idris2-repl-buffer)
    (save-excursion
      (goto-char idris2-prompt-start)
      (idris2-propertize-spans (idris2-repl-semantic-text-props highlighting)
        (insert-before-markers condition)))
    (idris2-repl-insert-prompt)
    (idris2-repl-show-maximum-output)))


(defun idris2-repl-write-string (string)
  "Append STRING to output."
  (with-current-buffer (idris2-repl-buffer)
    (save-excursion
      (goto-char idris2-prompt-start)
      (idris2-propertize-region
          `(face idris2-repl-output-face
            read-only idris2-repl-output
            rear-nonsticky (face read-only))
        (insert-before-markers string))
      (when (and (= (point) idris2-prompt-start)
                 (not (bolp)))
        (insert-before-markers "\n")))
    (idris2-repl-insert-prompt)
    (idris2-repl-show-maximum-output)))


(defun idris2-repl-insert-result (string &optional highlighting)
  "Insert STRING and mark it asg evaluation result.
Optional argument HIGHLIGHTING is a collection of semantic
highlighting information from Idris2."
  (with-current-buffer (idris2-repl-buffer)
    (save-excursion
      (goto-char (point-max))
      (when (and (not (bolp))
                 (not (string-equal string "")))
        (insert-before-markers "\n"))
      (idris2-propertize-region '(read-only idris2-repl-output
                                 rear-nonsticky (face read-only))
        (idris2-propertize-spans (idris2-repl-semantic-text-props highlighting)
          (idris2-propertize-region
              '(face idris2-repl-result-face
                rear-nonsticky (face))
            (insert-before-markers string)))))
    (idris2-repl-insert-prompt)
    (idris2-repl-show-maximum-output)))


(defun idris2-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                 (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1)
          (goto-char idris2-input-start))))))


;;; history

(defvar-local idris2-repl-input-history '()
  "History list of strings entered into the REPL buffer.")

(defun idris2-repl-add-to-input-history (string)
  "Adds input to history."
  (unless (equal string "")
    (setq idris2-repl-input-history
          (remove string idris2-repl-input-history)))
  (unless (equal string (car idris2-repl-input-history))
      (push string idris2-repl-input-history)))

(defvar-local idris2-repl-input-history-position -1
  "Newer items have smaller indices.")

(defun idris2-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region idris2-input-start (point-max)))

(defun idris2-repl-replace-input (string)
  (idris2-repl-delete-current-input)
  (insert-and-inherit string))

(defun idris2-repl-history-replace (direction)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((min-pos -1)
         (max-pos (length idris2-repl-input-history))
         (prefix (idris2-repl-history-prefix))
         (pos0 (if (idris2-repl-history-search-in-progress-p)
                   idris2-repl-input-history-position
                 min-pos))
         (pos (idris2-repl-position-in-history pos0 direction prefix))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (idris2-repl-replace-input (nth pos idris2-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          (t
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (message "%s (prefix is: %s)" msg prefix)
    (setq idris2-repl-input-history-position pos)
    (setq this-command 'idris2-repl-history-replace)))

(defvar-local idris2-repl-history-prefix-data ""
  "Current history prefix.")

(defun idris2-repl-history-prefix ()
  "Return the prefix we want to look for in the history."
  (if (idris2-repl-history-search-in-progress-p)
      idris2-repl-history-prefix-data
    (setq idris2-repl-history-prefix-data (idris2-repl-current-input))
    idris2-repl-history-prefix-data))

(defun idris2-repl-history-search-in-progress-p ()
  (eq last-command 'idris2-repl-history-replace))

(defun idris2-repl-position-in-history (start-pos direction prefix)
  "Return the position of the history item matching the PREFIX.
Return -1 resp. the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (cl-ecase direction
                 (forward -1)
                 (backward 1)))
         (history idris2-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             for history-item = (nth pos history)
             if (string-prefix-p prefix history-item)
             return pos)))

(defun idris2-repl-backward-history ()
  "Cycle backward through history."
  (interactive)
  (idris2-repl-history-replace 'backward))

(defun idris2-repl-forward-history ()
  "Cycle forward through history."
  (interactive)
  (idris2-repl-history-replace 'forward))


;; persistent history
(defun idris2-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'idris2-repl-mode)
        (idris2-repl-safe-save-history)))))

(defun idris2-repl-safe-save-history ()
  (idris2-repl-call-with-handler
   #'idris2-repl-save-history
   "%S while saving the history. Continue? "))

(defun idris2-repl-safe-load-history ()
  (idris2-repl-call-with-handler
   #'idris2-repl-load-history
   "%S while loading the history. Continue? "))

(defun idris2-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))

(defun idris2-repl-read-history-filename ()
  (read-file-name "Use Idris2 REPL history from file: "
                  idris2-repl-history-file))

(defun idris2-repl-load-history (&optional filename)
  "Set the current Idris2 REPL history.
It can be read either from FILENAME or `idris2-repl-history-file' or
from a user defined filename."
  (interactive (list (idris2-repl-read-history-filename)))
  (let ((file (or filename idris2-repl-history-file)))
    (setq idris2-repl-input-history (idris2-repl-read-history file))))

(defun idris2-repl-read-history (&optional filename)
  "Read and return the history from FILENAME.
The default value for FILENAME is `idris2-repl-history-file'."
  (let ((file (or filename idris2-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun idris2-repl-save-history (&optional filename history)
  "Simply save the current Idris2 REPL history to a file.
When Idris2 is setup to always load the old history and one uses only
one instance of idris2 all the time, there is no need to merge the
files and this function is sufficient."
  (interactive (list (idris2-repl-read-history-filename)))
  (let ((file (or filename idris2-repl-history-file))
        (hist (or history idris2-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (cl-subseq hist 0 (min (length hist) idris2-repl-history-size))))
      (with-temp-file file
        (let ((cs idris2-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for Idris2 REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(provide 'idris2-repl)
;;; idris2-repl.el ends here
