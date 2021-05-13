;;; idris2-commands.el --- Commands for Emacs passed to idris2 -*- lexical-binding: t -*-

;; Copyright (C) 2013 Hannes Mehnert

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

;;; Code:

(require 'idris2-core)
(require 'idris2-settings)
(require 'inferior-idris2)
(require 'idris2-repl)
(require 'idris2-warnings)
(require 'idris2-compat)
(require 'idris2-info)
(require 'idris2-tree-info)
(require 'idris2-log)
(require 'idris2-ipkg-mode)
(require 'idris2-warnings-tree)
(require 'idris2-hole-list)
(require 'idris2-prover)
(require 'idris2-common-utils)
(require 'idris2-syntax)
(require 'idris2-highlight-input)

(require 'cl-lib)
(require 'thingatpt)
(require 'xref)

(defvar-local idris2-load-to-here nil
  "The maximum position to load")

(defun idris2-make-dirty ()
  "Mark an Idris2 buffer as dirty and remove the loaded region."
  (setq idris2-buffer-dirty-p t)
  (when idris2-loaded-region-overlay
    (delete-overlay idris2-loaded-region-overlay))
  (setq idris2-loaded-region-overlay nil))

(defun idris2-make-clean ()
  (setq idris2-buffer-dirty-p nil))

(defun idris2-current-buffer-dirty-p ()
  "Check whether the current buffer's most recent version is loaded."
  (or idris2-buffer-dirty-p
      (not (equal (current-buffer)
                  idris2-currently-loaded-buffer))
      ;; for when we load the whole buffer
      (and (not idris2-load-to-here) (not idris2-loaded-region-overlay))
      ;; true when the place to load is outside the loaded region - extend region!
      (and idris2-loaded-region-overlay
           idris2-load-to-here
           (> (marker-position idris2-load-to-here)
              (overlay-end idris2-loaded-region-overlay)))))

(defun idris2-position-loaded-p (pos)
  (and idris2-loaded-region-overlay
       (member idris2-loaded-region-overlay (overlays-at pos))
       t))

(defun idris2-ensure-process-and-repl-buffer ()
  "Ensure that an Idris2 process is running and the Idris2 REPL buffer exists."
  (idris2-run)
  (idris2-repl-buffer))

(defun idris2-switch-working-directory (new-working-directory)
  (unless (string= idris2-process-current-working-directory new-working-directory)
    (idris2-ensure-process-and-repl-buffer)
    (idris2-eval `(:interpret ,(concat ":cd \"" new-working-directory "\"")))
    (setq idris2-process-current-working-directory new-working-directory)))

(defun idris2-list-holes-on-load ()
  "Use the user's settings from customize to determine whether to list the holes."
  (interactive)
  (when idris2-hole-show-on-load (idris2-list-holes)))

(defcustom idris2-load-file-success-hook '(idris2-list-holes-on-load
                                          idris2-set-current-pretty-print-width)
  "Functions to call when loading a file is successful"
  :type 'hook
  :options '(idris2-list-holes-on-load
             idris2-set-current-pretty-print-width)
  :group 'idris2)

(defun idris2-possibly-make-dirty (beginning end _length)
  ;; If there is a load-to-here marker and a currently loaded region, only
  ;; make the buffer dirty when the change overlaps the loaded region.
  (if (and idris2-load-to-here idris2-loaded-region-overlay)
      (when (member idris2-loaded-region-overlay
                    (overlays-in beginning end))
        (idris2-make-dirty))
    ;; Otherwise just make it dirty.
    (idris2-make-dirty)))


(defun idris2-update-loaded-region (fc)
  (if fc
      (let* ((end (assoc :end fc))
             (line (cadr end))
             (col (cl-caddr end)))
        (when (overlayp idris2-loaded-region-overlay)
          (delete-overlay idris2-loaded-region-overlay))
        (with-current-buffer idris2-currently-loaded-buffer
          (setq idris2-loaded-region-overlay
                (make-overlay (point-min)
                              (save-excursion (goto-char (point-min))
                                              (forward-line (1- line))
                                              (move-to-column (1- col))
                                              (point))
                              (current-buffer)))
          (overlay-put idris2-loaded-region-overlay 'face 'idris2-loaded-region-face)))

    ;; HACK: Some versions of Idris2 don't properly return a span for
    ;; some modules, returning () instead. Remove this (and the
    ;; surrounding (if fc)) after Idris2 0.9.17, which contains a fix.
    (idris2-update-loaded-region
     `((:filename ,(cdr (idris2-filename-to-load)))
       (:start 1 1)
       ,(save-excursion
          (goto-char (point-max))
          `(:end ,(idris2-get-line-num) 1))))))

(defun idris2-load-to (&optional pos)
  (when (not pos) (setq pos (point)))
  (setq idris2-load-to-here (copy-marker pos t))
  (setq overlay-arrow-position (copy-marker (save-excursion
                                              (goto-char pos)
                                              (line-beginning-position)) nil)))

(defun idris2-no-load-to ()
  (setq idris2-load-to-here nil)
  (setq overlay-arrow-position nil))

(defun idris2-load-forward-line (&optional nlines)
  (interactive)
  (when idris2-load-to-here
    (save-excursion
      (goto-char idris2-load-to-here)
      (forward-line nlines)
      (idris2-make-dirty)
      (idris2-load-to (point)))))

(defun idris2-load-backward-line ()
  (interactive)
  (idris2-load-forward-line -1))

(defun idris2-filename-to-load ()
  "Compute the working directory and filename to load in Idris2, returning these as a cons."
  (let* ((fn (buffer-file-name))
         (ipkg-srcdir (idris2-ipkg-find-src-dir))
         (srcdir (if ipkg-srcdir
                     ipkg-srcdir
                   (file-name-directory fn))))
    (when (and  ;; check that srcdir is prefix of filename - then load relative
           (> (length fn) (length srcdir))
           (string= (substring fn 0 (length srcdir)) srcdir))
      (setq fn (file-relative-name fn srcdir)))
    (cons srcdir fn)))

(defun idris2-load-file (&optional set-line)
  "Pass the current buffer's file to the inferior Idris2 process.
A prefix argument forces loading but only up to the current line."
  (interactive "p")
  (save-buffer)
  (idris2-ensure-process-and-repl-buffer)
  (when (and set-line (= set-line 4))
    (idris2-load-to (point))
    (idris2-make-dirty))
  (when (and set-line (= set-line 16)) (idris2-no-load-to))
  (if (buffer-file-name)
      (when (idris2-current-buffer-dirty-p)
        (when idris2-prover-currently-proving
          (if (y-or-n-p (format "%s is open in the prover. Abandon and load? "
                                idris2-prover-currently-proving))
              (idris2-prover-abandon)
            (signal 'quit nil)))
        ;; Remove warning overlays
        (idris2-warning-reset-all)
        ;; Clear the contents of the compiler notes buffer, if it exists
        (when (get-buffer idris2-notes-buffer-name)
          (with-current-buffer idris2-notes-buffer-name
            (let ((inhibit-read-only t)) (erase-buffer))))
        ;; Remove stale semantic highlighting
        (idris2-highlight-remove-overlays (current-buffer))
        ;; Actually do the loading
        (let* ((dir-and-fn (idris2-filename-to-load))
               (fn (cdr dir-and-fn))
               (srcdir (car dir-and-fn)))
          (setq idris2-currently-loaded-buffer nil)
          (idris2-switch-working-directory srcdir)
          (idris2-delete-ibc t) ;; delete the ibc to avoid interfering with partial loads
          (idris2-eval-async
           (if idris2-load-to-here
               `(:load-file ,fn ,(save-excursion
                                   (goto-char idris2-load-to-here)
                                   (idris2-get-line-num)))
             `(:load-file ,fn))
           (lambda (result)
             (pcase result
               (`(:highlight-source ,hs)
                (cl-loop
                 for h in hs
                 do (pcase h
                      (`(((:filename ,fn)
                          (:start ,start-line ,start-col)
                          (:end ,end-line ,end-col))
                         ,props)
                       (when (string= (file-name-nondirectory fn)
                                      (file-name-nondirectory (buffer-file-name)))
                         (idris2-highlight-input-region (current-buffer)
                                                       start-line start-col
                                                       end-line end-col
                                                       props))))))
               (_ (idris2-make-clean)
                  (idris2-update-options-cache)

                  (setq idris2-currently-loaded-buffer (current-buffer))
                  (when (member 'warnings-tree idris2-warnings-printing)
                    (idris2-list-compiler-notes))
                  (run-hooks 'idris2-load-file-success-hook)
                  (idris2-update-loaded-region result))))
           (lambda (_condition)
             (when (member 'warnings-tree idris2-warnings-printing)
               (idris2-list-compiler-notes)
               (if idris2-stay-in-current-window-on-compiler-error
                 (display-buffer (idris2-buffer-name :notes))
                 (pop-to-buffer (idris2-buffer-name :notes))))))))
    (error "Cannot find file for current buffer")))

(defun idris2-view-compiler-log ()
  "Jump to the log buffer, if it is open"
  (interactive)
  (let ((buffer (get-buffer idris2-log-buffer-name)))
    (if buffer
        (pop-to-buffer buffer)
      (message "No Idris2 compiler log is currently open"))))

(defun idris2-next-error ()
  "Jump to the next error overlay in the buffer."
  (interactive)
  (let ((warnings-forward (sort (cl-remove-if-not #'(lambda (w) (> (overlay-start w) (point))) idris2-warnings)
                                #'(lambda (w1 w2) (<= (overlay-start w1) (overlay-start w2))))))
    (if warnings-forward
        (goto-char (overlay-start (car warnings-forward)))
      (error "No warnings or errors until end of buffer"))))

(defun idris2-previous-error ()
  "Jump to the previous error overlay in the buffer."
  (interactive)
  (let ((warnings-backward (sort (cl-remove-if-not #'(lambda (w) (< (overlay-end w) (point))) idris2-warnings)
                                #'(lambda (w1 w2) (>= (overlay-end w1) (overlay-end w2))))))
    (if warnings-backward
        (goto-char (overlay-end (car warnings-backward)))
      (error "No warnings or errors until beginning of buffer"))))

(defun idris2-load-file-sync ()
  "Pass the current buffer's file synchronously to the inferior
Idris2 process. This sets the load position to point, if there is one."
  (save-buffer)
  (idris2-ensure-process-and-repl-buffer)
  (if (buffer-file-name)
      (unless (idris2-position-loaded-p (point))
        (idris2-warning-reset-all)
        (when (and idris2-load-to-here
                   (< (marker-position idris2-load-to-here) (point)))
          (idris2-load-to (point)))
        (let* ((dir-and-fn (idris2-filename-to-load))
               (fn (cdr dir-and-fn))
               (srcdir (car dir-and-fn)))
          (setq idris2-currently-loaded-buffer nil)
          (idris2-switch-working-directory srcdir)
          (let ((result
                 (if idris2-load-to-here
                     (idris2-eval `(:load-file ,fn
                                              ,(save-excursion
                                                 (goto-char idris2-load-to-here)
                                                 (idris2-get-line-num))))
                   (idris2-eval `(:load-file ,fn)))))
            (idris2-update-options-cache)
            (setq idris2-currently-loaded-buffer (current-buffer))
            (idris2-make-clean)
            (idris2-update-loaded-region (car result)))))
    (error "Cannot find file for current buffer")))


(defun idris2-get-line-num ()
  "Get the current line number"
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


(defun idris2-thing-at-point-raw ()
  "Return the name at point, or nil otherwise. Use this in Idris2 source buffers."
   (if (equal (syntax-after (point))
	      (string-to-syntax ".")) ;; TODO 3 probably this should be replaced with idris2 operator characters
       ;; We're on an operator.
       (save-excursion
	 (skip-syntax-backward ".")
	 (let ((beg (point)))
	   (skip-syntax-forward ".")
	   (buffer-substring-no-properties beg (point))))
     ;; Try if we're on a symbol or fail otherwise.
     (current-word t)
     )
  )

(defun idris2-thing-at-point (&optional prompt)
  "Return the name, line, and column number at point as a list. If prompt is t then
will prompt user if no symbol is there, and return what was entered by the user along with
the line and col number. Otherwise, will return nil for whole list.
Use this in Idris2 source buffers."

  (let ((name (or (idris2-thing-at-point-raw) (and prompt (read-string "Enter symbol: " nil)))))
    (if name
	(list name (idris2-get-line-num) (current-column)) nil)
    )
  )


(defun idris2-name-at-point ()
  "Return the name at point, taking into account semantic
annotations. Use this in Idris2 source buffers or in
compiler-annotated output. Does not return a line number."
  ;; co: this isn't working right... pointer on function name and it goes to first argument.
  ;;     TODO 2.9 not sure these overlays are appropriate in general, anyway. may rewrite with text properties
  ;; (let ((ref (cl-remove-if
  ;;             #'null
  ;;             (cons (get-text-property (point) 'idris2-ref)
  ;;                   (cl-loop for overlay in (overlays-at (point))
  ;;                            collecting (overlay-get overlay 'idris2-ref))))))
    ;; (if (null ref)
        (car (idris2-thing-at-point))
	;; (car ref))))
	)

(defun idris2-find-full-path (file)
  "Searches through idris2-process-current-working-directory and idris2-source-locations for given file and returns first match."
  (let* ((file-dirs (cons idris2-process-current-working-directory idris2-source-locations))
	 (poss-full-filenames (mapcar #'(lambda (d) (concat (file-name-as-directory d) file)) file-dirs))
	 (act-full-filenames (seq-filter #'file-exists-p poss-full-filenames))
	 )
    (if (null act-full-filenames) nil
      (progn
       (if (not (null (cdr act-full-filenames)))
	   (message "Multiple locations found for file '%s': %s" file act-full-filenames) ())
       (car act-full-filenames)))
    )
  )


(defun idris2-info-for-name (what name)
  "Display the type for a name"
  (let* ((ty (idris2-eval (list what name)))
             (result (car ty))
             (formatting (cdr ty)))
      (idris2-show-info (format "%s" result) formatting)))

(defun idris2-jump-to-location (loc is-same-window)
  "jumps to specified location"
  (pcase-let* ((`(,name ,file ,line ,col) loc)
	       (full-path (idris2-find-full-path file)))
    (xref-push-marker-stack) ;; this pushes a "tag" mark. haskell mode
    ;; also does this and it seems appropriate, allows the user to pop
    ;; the tag and go back to the previous point. (pop-tag-mark
    ;; default Ctl-t)
    (if full-path
	(idris2-goto-source-location-full full-path (+ 1 line) col is-same-window)
      (user-error "Source not found for %s" file)
      )
    )
  )

(defun idris2-show-jump-choices (locs is-same-window)
  (unless (idris2-info-buffer-visible-p)
    (idris2-info-show)
    (message "Press q to close the Idris2 info buffer."))
  (with-current-buffer (idris2-info-buffer)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (loc (reverse locs))
	(pcase-let* ((`(,name ,file ,line ,col) loc)
		     (fullpath (idris2-find-full-path file))
		     )
	  (if (file-exists-p fullpath)
	      (insert-button name 'follow-link t 'button loc
			     'action #'(lambda (_) (idris2-info-quit) (idris2-jump-to-location loc is-same-window)))
	    (insert (format "%s (not found)" name)))
	  (insert-char ?\n)
	  (goto-char (point-min))
	  )
	)
      )
    )
  )

(defun idris2-jump-to-def-name (name &optional is-same-window)
  (let ((res (car (idris2-eval (cons :name-at (cons name ()))))))
    (if (null res)
	(user-error "symbol '%s' not found" name)
      (if (null (cdr res)) ;; only one choice
	  (idris2-jump-to-location (car res) is-same-window)
	(idris2-show-jump-choices res is-same-window)
	)
     )
    )
  )

(defun idris2-jump-to-def (&optional is-same-window)
  "moves cursor to the definition of type at point"
  (interactive)
  (let ((name (car (idris2-thing-at-point t))))
    (idris2-jump-to-def-name name is-same-window))
  )

(defun idris2-jump-to-def-same-window ()
  "same as idris2-jump-to-def but uses same window (note: previous position can be restored using \\[pop-tag-mark]"
  (interactive)
  (idris2-jump-to-def t))


(defun idris2-type-at-point (thing)
  "Display the type of the name at point, considered as a global variable"
  (interactive "P")
  (let ((name (if thing (read-string "Check: ")
                (idris2-name-at-point))))
    (when name
      (idris2-info-for-name :type-of name))))

(defun idris2-print-definition-of-name (thing)
  "Display the definition of the function or type at point"
  (interactive "P")
  (let ((name (if thing (read-string "Print definition: ")
                (idris2-name-at-point))))
    (when name
      (idris2-info-for-name :print-definition name))))

(defun idris2-extract-location-from-name (str)
  "given a string in the form '(name) (possibly 'implementation' at (filename):(startline):(startcol)--(endline):(endcol)', extracts filename, startline and startcol as a list or nil otherwise"
  (let* ((index (string-match "\\([^ ]+\\) \\(?:implementation \\)?at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)--[0-9]+:[0-9]+$" str)))
    (if (null index) nil
      (progn
	(let
	    (
	     (n (match-string 1 str))
	     (fn (match-string 2 str))
	     (ln (string-to-number (match-string 3 str)))
	     (col (- (string-to-number (match-string 4 str)) 1))) ;; For some reason, ex. for col 0, Idris returns 0 as the column when using ':name-at' and 1 when printing to the user, so we subtract 1 to be consistent internally
	(list n fn ln col)))
      )
    ))



(defun idris2-who-calls-name-helper (collapse name-str children)
  (make-idris2-tree
   :item ""
   :highlighting nil
   :collapsed-p collapse
   :button (list name-str 'action #'(lambda (button)
				      (let ((loc (idris2-extract-location-from-name name-str)))
					(if loc
					    (idris2-jump-to-location loc nil)
					  (idris2-jump-to-def-name name-str nil)))))
   :kids
      #'(lambda () (mapcar #'(lambda (child)
			       (pcase-let* ((`(,cname ,ctimes-called) child)
					    (children-of-child (car (cdr (caar (idris2-eval `(:who-calls ,cname))))))
					    )
				 (idris2-who-calls-name-helper t cname children-of-child)
		    ;; (idris2-who-calls-name-helper t (format "%s (Called %d times)" cname ctimes-called) children-of-child))
		   ))
			   children))
   :preserve-properties '(idris2-tt-tree))
  )

(defun idris2-who-calls-name (name)
  "Show the callers of NAME in a tree."
  (let* ((response (car (idris2-eval `(:who-calls ,name))))
         (roots (mapcar #'(lambda (name-children) (apply 'idris2-who-calls-name-helper nil name-children)) response)))
    (if (not (null roots))
        (idris2-tree-info-show-multiple roots "Callers")
      (message "The name %s was not found." name))
    nil))

(defun idris2-who-calls-name-at-point (thing)
  (interactive "P")
  (xref-push-marker-stack)
  (let ((name-loc (idris2-thing-at-point t)))
    (idris2-who-calls-name (car name-loc)))
  )

(defun idris2-name-calls-who (name)
  "Show the callees of NAME in a tree."
  (xref-push-marker-stack)
  (let* ((callees (idris2-eval `(:calls-who ,name)))
         (roots (mapcar #'(lambda (c) (idris2-caller-tree c :calls-who)) (car callees))))
    (if (not (null roots))
        (idris2-tree-info-show-multiple roots "Callees")
      (message "The name %s was not found." name))
    nil))

(defun idris2-name-calls-who-at-point (thing)
  (interactive "P")
  (let ((name (if thing (read-string "Calls who: ")
                (idris2-name-at-point))))
    (when name
      (idris2-name-calls-who name))))

(defun idris2-browse-namespace (namespace)
  "Show the contents of NAMESPACE in a tree info buffer."
  (interactive
   ;; Compute a default namespace for the prompt based on the text
   ;; annotations at point when called interactively. Overlays are
   ;; preferred over text properties.
   (let ((default
           (or (cl-some #'(lambda (o) (overlay-get o 'idris2-namespace))
                        (overlays-at (point)))
               (get-text-property (point) 'idris2-namespace))))
     (list (read-string "Browse namespace: " default))))
  (idris2-tree-info-show (idris2-namespace-tree namespace)
                        "Browse Namespace"))

(defun idris2-caller-tree (caller cmd)
  "Display a tree from an IDE caller list, lazily retrieving a few levels at a time"
  (pcase caller
    (`((,name ,highlight) ,children)
     (make-idris2-tree
      :item name
      :highlighting highlight
      :collapsed-p t
      :kids (lambda ()
              (cl-mapcan #'(lambda (child)
                             (let* (
				    (cmd-to-run (list cmd (car child))
				    (child-name (car (idris2-eval `(,cmd ,(car child))))))
                               (if child-name
                                   (list (idris2-caller-tree child-name cmd))
                                 nil)))
                         children)))
      :preserve-properties '(idris2-tt-tree)))
    (_ (error "failed to make tree from %s" caller))))

(defun idris2-namespace-tree (namespace &optional recursive)
  "Create a tree of the contents of NAMESPACE, lazily retrieving children when RECURSIVE is non-nil."
  (cl-flet*
      ;; Show names as childless trees with decorated roots
      ((name-tree (n) (make-idris2-tree :item (car n)
                                       :highlighting (cadr n)
                                       :kids nil
                                       :preserve-properties '(idris2-tt-tree)))
       ;; The children of a tree are the namespaces followed by the names.
       (get-children (sub-namespaces names)
                     (append (mapcar #'(lambda (ns)
                                         (idris2-namespace-tree ns t))
                                     sub-namespaces)
                             (mapcar #'name-tree names))))
    (let ((highlight `((0 ,(length namespace)
                          ((:decor :namespace)
                           (:namespace ,namespace))))))
      (if recursive
          ;; In the recursive case, generate a collapsed tree and lazily
          ;; get the contents as expansion is requested
          (make-idris2-tree
           :item namespace
           :highlighting highlight
           :collapsed-p t
           :kids (lambda ()
                   (pcase (idris2-eval `(:browse-namespace ,namespace))
                     (`((,sub-namespaces ,names . ,_))
                      (get-children sub-namespaces names))
                     (_ nil)))
           :preserve-properties '(idris2-tt-term))
        ;; In the non-recursive case, generate an expanded tree with the
        ;; first level available, but only if the namespace actually makes
        ;; sense
        (pcase (idris2-eval `(:browse-namespace ,namespace))
          (`((,sub-namespaces ,names . ,_))
           (make-idris2-tree
            :item namespace
            :highlighting highlight
            :collapsed-p nil
            :kids (get-children sub-namespaces names)
            :preserve-properties '(idris2-tt-term)))
          (_ (error "Invalid namespace %s" namespace)))))))

(defun idris2-newline-and-indent ()
  "Indent a new line like the current one by default"
  (interactive)
  (let ((indent ""))
    (save-excursion
      (move-beginning-of-line nil)
      (when (looking-at (if (idris2-lidr-p) "^\\(>\\s-*\\)" "\\(\\s-*\\)"))
        (setq indent (match-string 1))))
    (insert "\n" indent)))

(defun idris2-delete-forward-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
If the current buffer is in `idris2-mode' and the file being
edited is a literate Idris2 file, deleting the end of a line will
take into account bird tracks.  If Transient Mark mode is
enabled, the mark is active, and N is 1, delete the text in the
region and deactivate the mark instead.  To disable this, set
`delete-active-region' to nil.

Optional second arg KILLFLAG non-nil means to kill (save in kill
ring) instead of delete.  Interactively, N is the prefix arg, and
KILLFLAG is set if N was explicitly specified."
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
   (cond
    ;; Under the circumstances that `delete-forward-char' does something
    ;; special, delegate to it. This was discovered by reading the source to
    ;; it.
    ((and (use-region-p)
          delete-active-region
          (= n 1))
     (call-interactively 'delete-forward-char n killflag))
    ;; If in idris2-mode and editing an LIDR file and at the end of a line,
    ;; then delete the newline and a leading >, if it exists
    ((and (eq major-mode 'idris2-mode)
          (idris2-lidr-p)
          (= n 1)
          (eolp))
     (delete-char 1 killflag)
     (when (and (not (eolp)) (equal (following-char) ?\>))
       (delete-char 1 killflag)
       (when (and (not (eolp)) (equal (following-char) ?\ ))
         (delete-char 1 killflag))))
    ;; Nothing special to do - delegate to `delete-char', just as
    ;; `delete-forward-char' does
    (t (delete-char 1 killflag))))

(defun idris2-apropos (what)
  "Look up something in names, type signatures, and docstrings"
  (interactive "sSearch Idris2 docs for: ")
  (idris2-info-for-name :apropos what))

(defun idris2-type-search (what)
  "Search the Idris2 libraries by fuzzy type matching"
  (interactive "sSearch for type: ")
  (idris2-info-for-name :interpret (concat ":search " what)))

(defun idris2-docs-at-point (thing)
  "Display the internal documentation for the name at point, considered as a global variable"
  (interactive)
  (message "Not implemented yet"))

(defun idris2-eldoc-lookup ()
  "Support for showing type signatures in the modeline when there's a running Idris2"
  (get-char-property (point) 'idris2-eldoc))

(defun idris2-pretty-print ()
  "Get a term or definition pretty-printed by Idris2. Useful for writing papers or slides."
  (interactive)
  (let ((what (read-string "What should be pretty-printed? "))
        (fmt (completing-read "What format? " '("html", "latex") nil t nil nil "latex"))
        (width (read-string "How wide? " nil nil "80")))
    (if (<= (string-to-number width) 0)
        (error "Width must be positive")
      (if (< (length what) 1)
          (error "Nothing to pretty-print")
        (let ((text (idris2-eval `(:interpret ,(concat ":pprint " fmt " " width " " what)))))
          (with-idris2-info-buffer
            (insert (car text))
            (goto-char (point-min))
            (re-search-forward (if (string= fmt "latex")
                                   "% START CODE\n"
                                 "<!-- START CODE -->"))
            (push-mark nil t)
            (re-search-forward (if (string= fmt "latex")
                                   "% END CODE\n"
                                 "<!-- END CODE -->"))
            (goto-char (match-beginning 0))
            (copy-region-as-kill (mark) (point))
            (message "Code copied to kill ring")))))))


(defun idris2-case-split ()
  "Case split the pattern variable at point"
  (interactive)
  (let ((what (idris2-thing-at-point)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let ((result (car (idris2-eval `(:case-split ,(cadr what) ,(car what))))))
        (if (<= (length result) 2)
            (message "Can't case split %s" (car what))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (substring result 0 (1- (length result)))))))))

(defun idris2-make-cases-from-hole ()
  "Make a case expression from the metavariable at point."
  (interactive)
  (let ((what (idris2-thing-at-point)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let ((result (car (idris2-eval `(:make-case ,(cadr what) ,(car what))))))
        (if (<= (length result) 2)
            (message "Can't make cases from %s" (car what))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (substring result 0 (1- (length result)))))))))

(defun idris2-case-dwim ()
  "If point is on a hole name, make it into a case expression. Otherwise, case split as a pattern variable."
  (interactive)
  (if (or (looking-at-p "\\?[a-zA-Z_]+")
          (looking-back "\\?[a-zA-Z0-9_]+" nil))
      (idris2-make-cases-from-hole)
    (idris2-case-split)))

(defun idris2-add-clause (proof)
  "Add clauses to the declaration at point"
  (interactive "P")
  (let ((what (idris2-thing-at-point))
        (command (if proof :add-proof-clause :add-clause)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let ((result (car (idris2-eval `(,command ,(cadr what) ,(car what)))))
            final-point
            (prefix (save-excursion        ; prefix is the indentation to insert for the clause
                      (goto-char (point-min))
                      (forward-line (1- (cadr what)))
                      (goto-char (line-beginning-position))
                      (re-search-forward "\\(^>?\\s-*\\)" nil t)
                      (let ((prefix (match-string 1)))
                        (if prefix
                            prefix
                          "")))))
        ;; Go forward until we get to a line with equal or less indentation to
        ;; the type declaration, or the end of the buffer, and insert the
        ;; result
        (goto-char (line-beginning-position))
        (forward-line)
        (while (and (not (eobp))
                    (progn (goto-char (line-beginning-position))
                           ;; this will be true if we're looking at the prefix
                           ;; with extra whitespace
                           (looking-at-p (concat prefix "\\s-+"))))
          (forward-line))
        (insert prefix)
        (setq final-point (point)) ;; Save the location of the start of the clause
        (insert result)
        (newline)
        (goto-char final-point))))) ;; Put the cursor on the start of the inserted clause

(defun idris2-add-missing ()
  "Add missing cases"
  (interactive)
  (message "No add-missing in Idris2 yet")
  )

(defun idris2-make-with-block ()
  "Add with block"
  (interactive)
  (let ((what (idris2-thing-at-point)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let ((result (car (idris2-eval `(:make-with ,(cadr what) ,(car what))))))
        (beginning-of-line)
        (kill-line)
        (insert result)))))

(defun idris2-make-lemma ()
  "Extract lemma from hole"
  (interactive)
  (let ((what (idris2-thing-at-point)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let* ((type-decl (car (idris2-eval `(:make-lemma ,(cadr what) ,(car what))))))
	(message "type-decl is %s" type-decl)

	;; (let ((lem-app (cadr (assoc :replace-metavariable (cdr result))))
	;;       (type-decl (cadr (assoc :definition-type (cdr result)))))
	;; replace the hole
	;; assume point is on the hole right now!
	(while (not (looking-at "\\?[a-zA-Z0-9?_]+"))
	  (backward-char 1))

	;; now we're on the ? - delete that character, that is the name of our lemma
	(delete-char 1)

	;; now we add the type signature - search upwards for the first blank line
	;; and get the indentation of the line after it. Then insert before it, respecting indentation
        (re-search-backward (if (idris2-lidr-p)
				"^\\(>\\s-*\\)\\(([^)]+)\\|\\w+\\)\\s-*:"
                              "^\\(\\s-*\\)\\(([^)]+)\\|\\w+\\)\\s-*:"))

	(let ((indentation (match-string 1)) end-point)
	  (when (not (idris2-lidr-p))
	    (re-search-backward "^\\s-*\n")) ;; to skip any comment before the definition, we find the preceding blank line
	  (message "ind is '%s'" indentation)
	  (newline 1)
	  (beginning-of-line)
	  (insert indentation)
	  (insert type-decl)
	  (setq end-point (point)) ;; we want the point ready to type the definition of the lemma
	  (newline 1)
	  (goto-char end-point)
	  )
	)
      )
    )
  )


(defun idris2-compile-and-execute ()
  "Execute the program in the current buffer"
  (interactive)
  (idris2-load-file-sync)
  (idris2-eval '(:interpret ":exec")))

(defun idris2-proof-search (&optional arg)
  "Invoke the proof search. A plain prefix argument causes the
command to prompt for hints and recursion depth, while a numeric
prefix argument sets the recursion depth directly."
  (interactive "P")
  (let
      ((what (idris2-thing-at-point)))
    (when (car what)
      (save-excursion (idris2-load-file-sync))
      (let ((result (car (idris2-eval `(:proof-search ,(cadr what) ,(car what))))))
	(if (string= result "")
	    (error "Nothing found")
	  (save-excursion
	    (let ((start (progn (search-backward "?") (point)))
		  (end (progn (forward-char) (search-forward-regexp "[^a-zA-Z0-9_']") (backward-char) (point))))
	      (delete-region start end))
	    (insert result)))))
    )
  )

(defun idris2-refine (name)
  "Refine by some NAME, without recursive proof search."
  (interactive)
  (message "No refine in Idris2 yet!")
)

(defun idris2-identifier-backwards-from-point ()
  (let (identifier-start
        (identifier-end (point))
        (failure (list nil nil nil)))
    (save-excursion
      (while (and (> (point) (point-min)) (idris2-is-ident-char-p (char-before)))
        (backward-char)
        (setq identifier-start (point)))
      (if identifier-start
          (list (buffer-substring-no-properties identifier-start identifier-end)
                identifier-start
                identifier-end)
        failure))))

(defun idris2-complete-symbol-at-point ()
  "Attempt to complete the symbol at point as a global variable.

This function does not attempt to load the buffer if it's not
already loaded, as a buffer awaiting completion is probably not
type-correct, so loading will fail."
  (if (not idris2-process)
      nil
    (cl-destructuring-bind (identifier start end) (idris2-identifier-backwards-from-point)
      (when identifier
        (let ((result (car (idris2-eval `(:repl-completions ,identifier)))))
          (cl-destructuring-bind (completions _partial) result
            (if (null completions)
                nil
              (list start end completions
                    :exclusive 'no))))))))

(defun idris2-complete-keyword-at-point ()
  "Attempt to complete the symbol at point as an Idris2 keyword."
  (pcase-let* ((all-idris2-keywords
                (append idris2-keywords idris2-definition-keywords))
               (`(,identifier ,start ,end)
                (idris2-identifier-backwards-from-point)))
    (when identifier
      (let ((candidates (cl-remove-if-not
                         (apply-partially #'string-prefix-p identifier)
                         all-idris2-keywords)))
        (if (null candidates)
            nil
          (list start end candidates
                :exclusive 'no))))))

(defun idris2-list-holes ()
  "Get a list of currently-open holes"
  (interactive)
  (idris2-hole-list-show (car (idris2-eval '(:metavariables 80)))))

(defun idris2-kill-buffers ()
  (idris2-warning-reset-all)
  (setq idris2-currently-loaded-buffer nil)
  ;; not killing :events since it it tremendously useful for debuging
  (let ((bufs (list :connection :repl :proof-obligations :proof-shell :proof-script :log :info :notes :holes :tree-viewer)))
    (dolist (b bufs) (idris2-kill-buffer b))))

(defun idris2-pop-to-repl ()
  "Go to the REPL, if one is open."
  (interactive)
  (let ((buf (get-buffer (idris2-buffer-name :repl))))
    (if buf
        (pop-to-buffer buf)
      (error "No Idris2 REPL buffer is open."))))

(defun idris2-quit ()
  "Quit the Idris2 process, cleaning up the state that it has synchronized with Emacs."
  (interactive)
  (setq idris2-prover-currently-proving nil)
  (let* ((pbufname (idris2-buffer-name :process))
         (pbuf (get-buffer pbufname))
         (cbuf (get-buffer (idris2-buffer-name :connection))))
    (when cbuf
      (when (get-buffer-process cbuf)
        (with-current-buffer cbuf (delete-process nil))) ; delete connection without asking
      (kill-buffer cbuf))
    (when pbuf
      (when (get-buffer-process pbuf)
        (with-current-buffer pbuf (delete-process nil))) ; delete process without asking
      (kill-buffer pbuf)
      (unless (get-buffer pbufname) (idris2-kill-buffers))
      (setq idris2-rex-continuations '())
      (when idris2-loaded-region-overlay
        (delete-overlay idris2-loaded-region-overlay)
        (setq idris2-loaded-region-overlay nil)))
    (idris2-prover-end)
    (idris2-kill-buffers)))

(defun idris2-delete-ibc (no-confirmation)
  "Delete the IBC file for the current buffer. A prefix argument
means to not ask for confirmation."
  (interactive "P")
  (let* ((fname (buffer-file-name))
         (ibc (concat (file-name-sans-extension fname) ".ibc")))
    (if (not (or (string= (file-name-extension fname) "idr")
                 (string= (file-name-extension fname) "lidr")))
        (error "The current file is not an Idris2 file")
      (when (or no-confirmation (y-or-n-p (concat "Really delete " ibc "?")))
        (when (file-exists-p ibc)
          (delete-file ibc)
          (message "%s deleted" ibc))))))

(defun idris2--active-term-beginning (term pos)
  "Find the beginning of active term TERM that occurs at POS.

It is an error if POS is not in the specified term. TERM should
be Idris2's own serialization of the term in question."
  (unless (equal (get-char-property pos 'idris2-tt-term) term)
    (error "Term not present at %s" pos))
  (save-excursion
    ;; Find the beginning of the active term
    (goto-char pos)
    (while (equal (get-char-property (point) 'idris2-tt-term)
                  term)
      (backward-char 1))
    (forward-char 1)
    (point)))

(defun idris2-make-term-menu (_term)
  "Make a menu for the widget for some term."
  (let ((menu (make-sparse-keymap)))
    (define-key menu [idris2-term-menu-normalize]
      `(menu-item "Normalize"
                  (lambda () (interactive))))
    (define-key-after menu [idris2-term-menu-show-implicits]
      `(menu-item "Show implicits"
                  (lambda () (interactive))))
    (define-key-after menu [idris2-term-menu-hide-implicits]
      `(menu-item "Hide implicits"
                  (lambda () (interactive))))
    (define-key-after menu [idris2-term-menu-core]
      `(menu-item "Show core"
                  (lambda () (interactive))))
    menu))

(defun idris2-insert-term-widget (term)
  "Make a widget for interacting with the term represented by TERM beginning at START-POS in the current buffer."
  (let ((inhibit-read-only t)
        (start-pos (copy-marker (point)))
        (end-pos (copy-marker (idris2-find-term-end (point) 1)))
        (buffer (current-buffer)))
    (insert-before-markers
     (propertize
      "▶"
      'face 'idris2-active-term-face
      'mouse-face 'highlight
      'idris2-term-widget term
      'help-echo "<mouse-3>: term menu"
      'keymap (let ((map (make-sparse-keymap)))
                (define-key map [mouse-3]
                  (lambda () (interactive)
                    (let ((selection
                           (x-popup-menu t (idris2-make-term-menu term))))
                      (cond ((equal selection
                                    '(idris2-term-menu-normalize))
                             (idris2-normalize-term start-pos buffer)
                             (idris2-remove-term-widgets))
                            ((equal selection
                                    '(idris2-term-menu-show-implicits))
                             (idris2-show-term-implicits start-pos buffer)
                             (idris2-remove-term-widgets))
                            ((equal selection
                                    '(idris2-term-menu-hide-implicits))
                             (idris2-hide-term-implicits start-pos buffer)
                             (idris2-remove-term-widgets))
                            ((equal selection
                                    '(idris2-term-menu-core))
                             (idris2-show-core-term start-pos buffer)
                             (idris2-remove-term-widgets))))))
                map)))
    (let ((term-overlay (make-overlay start-pos end-pos)))
      ;; TODO: delete the markers now that they're not useful
      (overlay-put term-overlay 'idris2-term-widget term)
      (overlay-put term-overlay 'face 'idris2-active-term-face))))

(defun idris2-add-term-widgets ()
  "Add interaction widgets to annotated terms."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (term)
      (while (setq term (idris2-search-property 'idris2-tt-term))
        (idris2-insert-term-widget term)))))

(defun idris2-remove-term-widgets (&optional buffer)
  "Remove interaction widgets from annotated terms."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (let ((inhibit-read-only t))
        (mapc (lambda (overlay)
                (when (overlay-get overlay 'idris2-term-widget)
                  (delete-overlay overlay)))
              (overlays-in (point-min) (point-max)))
        (goto-char (point-min))
        (while (idris2-search-property 'idris2-term-widget)
          (delete-char 1))))))

(defun idris2-show-term-implicits (position &optional buffer)
  "Replace the term at POSITION with a fully-explicit version."
  (interactive "d")
  (idris2-active-term-command position :show-term-implicits buffer))

(defun idris2-hide-term-implicits (position &optional buffer)
  "Replace the term at POSITION with a fully-implicit version."
  (interactive "d")
  (idris2-active-term-command position :hide-term-implicits buffer))

(defun idris2-normalize-term (position &optional buffer)
  "Replace the term at POSITION with a normalized version."
  (interactive "d")
  (idris2-active-term-command position :normalise-term buffer))

(defun idris2-show-core-term (position &optional buffer)
  "Replace the term at POSITION with the corresponding core term."
  (interactive "d")
  (idris2-active-term-command position :elaborate-term buffer))

(defun idris2-active-term-command (position cmd &optional buffer)
  "For the term at POSITION, Run the live term command CMD."
  (unless (member cmd '(:show-term-implicits
                        :hide-term-implicits
                        :normalise-term
                        :elaborate-term))
    (error "Invalid term command %s" cmd))
  (with-current-buffer (or buffer (current-buffer))
    (let ((term (plist-get (text-properties-at position) 'idris2-tt-term)))
      (if (null term)
          (error "No term here")
        (let* ((res (car (idris2-eval (list cmd term))))
               (new-term (car res))
               (spans (cadr res))
               (col (save-excursion (goto-char (idris2-find-term-end position -1))
                                    (current-column)))
               (rendered
                (with-temp-buffer
                  (idris2-propertize-spans (idris2-repl-semantic-text-props spans)
                    (insert new-term))
                  ;; Indent the new term properly, if it's annotated
                  (let ((new-tt-term (plist-get (text-properties-at (point-min)) 'idris2-tt-term)))
                    (when new-tt-term
                      (goto-char (point-min))
                      (when (= (forward-line 1) 0)
                        (indent-rigidly (point) (point-max) col))
                      (put-text-property (point-min) (point-max) 'idris2-tt-term new-tt-term)))
                  (buffer-string))))
          (idris2-replace-term-at position rendered))))))

(defun idris2-find-term-end (pos step)
  "Find an end of the term at POS, moving STEP positions in each iteration.
Return the position found."
  (unless (or (= step 1) (= step -1))
    (error "Valid values for STEP are 1 or -1"))
  ;; Can't use previous-single-property-change-position because it breaks if
  ;; point is at the beginning of the term (likewise for next/end).
  (let ((term (plist-get (text-properties-at pos) 'idris2-tt-term)))
    (when (null term)
      (error "No term at %s" pos))
    (save-excursion
      (goto-char pos)
      (while (and (string= term
                           (plist-get (text-properties-at (point))
                                      'idris2-tt-term))
                  (not (eobp))
                  (not (bobp)))
        (forward-char step))
      (if (= step -1)
          (1+ (point))
        (point)))))

(defun idris2-replace-term-at (position new-term)
  "Replace the term at POSITION with the new rendered term NEW-TERM.
The idris2-tt-term text property is used to determined the extent
of the term to replace."
  (when (null (plist-get (text-properties-at position) 'idris2-tt-term))
    (error "No term here"))
  (let ((start (idris2-find-term-end position -1))
        (end (idris2-find-term-end position 1))
        (inhibit-read-only t))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert new-term))))

(defun idris2-prove-hole (name &optional elab)
  "Launch the prover on the hole NAME, using Elab mode if ELAB is non-nil."
  (idris2-eval-async `(:interpret ,(concat (if elab ":elab " ":p ") name))
                    (lambda (_) t))
  ;; The timer is necessary because of the async nature of starting the prover
  (run-with-timer 0.25 nil
                  #'(lambda ()
                      (let ((buffer (get-buffer idris2-prover-script-buffer-name)))
                        (when buffer
                          (let ((window (get-buffer-window buffer)))
                            (when window
                              (select-window window))))))))

(defun idris2-fill-paragraph (justify)
  ;; In literate Idris2 files, allow filling non-code paragraphs
  (if (and (idris2-lidr-p) (not (save-excursion (move-beginning-of-line nil)
                                               (looking-at-p ">\\s-"))))
      (fill-paragraph justify)
    (save-excursion
      (if (nth 4 (syntax-ppss))
          (fill-comment-paragraph justify) ;; if inside comment, use normal Emacs comment filling
        (if (save-excursion (move-beginning-of-line nil)
                            (looking-at "\\s-*|||\s-*")) ;; if inside documentation, fill with special prefix
            (let ((fill-prefix (substring-no-properties (match-string 0)))
                  (paragraph-start "\\s-*|||\\s-*$\\|\\s-*$\\|\\s-*@" )
                  (paragraph-separate "\\s-*|||\\s-*$\\|\\s-*$"))
              (fill-paragraph))
          ;; Otherwise do nothing
          "")))))


(defun idris2-set-idris2-load-packages ()
  "Interactively set the `idris2-load-packages' variable"
  (interactive)
  (let* ((idris2-libdir (replace-regexp-in-string
                        "[\r\n]*\\'" ""   ; remove trailing newline junk
                        (shell-command-to-string (concat idris2-interpreter-path " --libdir"))))
         (idris2-libs (cl-remove-if #'(lambda (x) (string= (substring x 0 1) "."))
                                   (directory-files idris2-libdir)))
         (packages '())
         (prompt "Package to use (blank when done): ")
         (this-package (completing-read prompt (cons "" idris2-libs))))
    (while (not (string= this-package ""))
      (push this-package packages)
      (setq this-package (completing-read prompt (cl-remove-if #'(lambda (x) (member x packages))
                                                               idris2-libs))))
    (when (y-or-n-p (format "Use the packages %s for this session?"
                            (cl-reduce #'(lambda (x y) (concat x ", " y)) packages)))
      (setq idris2-load-packages packages)
      (when (y-or-n-p "Save package list for future sessions? ")
        (add-file-local-variable 'idris2-load-packages packages)))))

(defun idris2-open-package-file ()
  "Provide easy access to package files."
  (interactive)
  (let ((files (idris2-find-file-upwards "ipkg")))
    (cond ((= (length files) 0)
           (error "No .ipkg file found"))
          ((= (length files) 1)
           (find-file (car files)))
          (t (find-file (completing-read "Package file: " files nil t))))))

(defun idris2-start-project ()
  "Interactively create a new Idris2 project, complete with ipkg file."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (default-filename (downcase (replace-regexp-in-string "[^a-zA-Z]" "" project-name)))
         (create-in (read-directory-name "Create in: " nil default-filename))
         (default-ipkg-name (concat default-filename ".ipkg"))
         (ipkg-file (read-string
                     (format "Package file name (%s): " default-ipkg-name)
                     nil nil default-ipkg-name))
         (src-dir (read-string "Source directory (src): " nil nil "src"))
         (module-name-suggestion (replace-regexp-in-string "[^a-zA-Z]+" "." (capitalize project-name)))
         (first-mod (read-string
                     (format "First module name (%s): " module-name-suggestion)
                     nil nil module-name-suggestion)))
    (when (file-exists-p create-in) (error "%s already exists" create-in))
    (when (string= src-dir "") (setq src-dir nil))
    (make-directory create-in t)
    (when src-dir (make-directory (concat (file-name-as-directory create-in) src-dir) t))
    (find-file (concat (file-name-as-directory create-in) ipkg-file))
    (insert "package " (replace-regexp-in-string ".ipkg$" "" ipkg-file))
    (newline 2)
    (insert "-- " project-name)
    (newline)
    (let ((name (user-full-name)))
      (unless (string= name "unknown")
        (insert "-- by " name)
        (newline)))
    (newline)
    (insert "opts = \"\"")
    (newline)
    (when src-dir (insert "sourcedir = " src-dir) (newline))
    (insert "modules = ")
    (insert first-mod)
    (newline)
    (save-buffer)
    (let* ((mod-path (reverse (split-string first-mod "\\.+")))
           (mod-dir (mapconcat #'file-name-as-directory
                               (cons create-in (cons src-dir (reverse (cdr mod-path))))
                               ""))
           (filename (concat mod-dir (car mod-path) ".idr")))
      (make-directory mod-dir t)
      (pop-to-buffer (find-file-noselect filename))
      (insert "module " first-mod)
      (newline)
      (save-buffer))))

;;; Pretty-printer stuff

(defun idris2-set-current-pretty-print-width ()
  "Send the current pretty-printer width to Idris2, if there is a process."
  (let ((command (format ":consolewidth %s"
                         (or idris2-pretty-printer-width
                             "infinite"))))
    (when (and idris2-process
               (not idris2-prover-currently-proving))
      ;; (idris2-eval `(:interpret ,command) t) TIMHACK
     )))

;;; Computing a menu with these commands
(defun idris2-context-menu-items (plist)
  "Compute a contextual menu based on the Idris2 semantic decorations in PLIST."
  (let ((ref (or (plist-get plist 'idris2-name-key) (plist-get plist 'idris2-ref)))
        (ref-style (plist-get plist 'idris2-ref-style))
        (namespace (plist-get plist 'idris2-namespace))
        (source-file (plist-get plist 'idris2-source-file))
        (tt-term (plist-get plist 'idris2-tt-term)))
    (append
     (when ref
       (append (list (list "Get type"
                           (lambda ()
                             (interactive)
                             (idris2-info-for-name :type-of ref))))
               (cond ((member ref-style
                              '(:type :data :function))
                      (list
                       (list "Get docs"
                             (lambda ()
                               (interactive)
                               (idris2-info-for-name :docs-for ref)))
                       (list "Get definition"
                             (lambda ()
                               (interactive)
                               (idris2-info-for-name :print-definition ref)))
                       (list "Who calls?"
                             (lambda ()
                               (interactive)
                               (idris2-who-calls-name ref)))
                       (list "Calls who?"
                             (lambda ()
                               (interactive)
                               (idris2-name-calls-who ref)))))
                     ((equal ref-style :metavar)
                      (cons (list "Launch prover"
                                  (lambda ()
                                    (interactive)
                                    (idris2-prove-hole ref)))
                            (when idris2-enable-elab-prover
                              (list (list "Launch interactive elaborator"
                                          (lambda ()
                                            (interactive)
                                            (idris2-prove-hole ref t))))))))))
     (when namespace
       (list (list (concat "Browse " namespace)
                   (lambda ()
                     (interactive)
                     (idris2-browse-namespace namespace)))))
     (when (and namespace source-file)
       (list (list (concat "Edit " source-file)
                   (lambda ()
                     (interactive)
                     (find-file source-file)))))
     (when tt-term
       (list (list "Normalize term"
                   (let ((pos (point)))
                     (lambda ()
                       (interactive)
                       (save-excursion
                         (idris2-normalize-term
                          (idris2--active-term-beginning tt-term pos))))))
             (list "Show term implicits"
                   (let ((pos (point)))
                     (lambda ()
                       (interactive)
                       (save-excursion
                         (idris2-show-term-implicits
                          (idris2--active-term-beginning tt-term pos))))))
             (list "Hide term implicits"
                   (let ((pos (point)))
                     (lambda ()
                       (interactive)
                       (save-excursion
                         (idris2-hide-term-implicits
                          (idris2--active-term-beginning tt-term pos))))))
             (list "Show core"
                   (let ((pos (point)))
                     (lambda ()
                       (interactive)
                       (save-excursion
                         (idris2-show-core-term
                          (idris2--active-term-beginning tt-term pos)))))))))))

(provide 'idris2-commands)
;; idris2-commands.el ends here
