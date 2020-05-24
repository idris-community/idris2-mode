;;; inferior-idris2.el --- Run an Idris2 interpreter using S-Expression communication protocol -*- lexical-binding: t -*-

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
(require 'idris2-settings)
(require 'idris2-common-utils)
(require 'pp)
(require 'cl-lib)
(require 'idris2-events)
(require 'idris2-log)
(require 'idris2-warnings)

;;; Words of encouragement - strongly inspired by Slime
(defun idris2-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))


(defvar idris2-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    ,(format "%s, this could be the start of a beautiful program."
             (idris2-user-first-name))
    ,(format "%s, this could be the start of a beautiful proof."
             (idris2-user-first-name))
    "The terms have seized control of the means of computation - a glorious future awaits!"
    "It typechecks! Ship it!"
    "Do you know 'Land of My Fathers'?"
    "Constructors are red / Types are blue / Your code always works / Because Idris2 loves you"))

(defun idris2-random-words-of-encouragement ()
  "Return a random string of encouragement"
  (nth (random (length idris2-words-of-encouragement))
       idris2-words-of-encouragement))

;;; Process stuff
(defvar idris2-process nil
  "The Idris2 process.")

(defvar idris2-connection nil
  "The Idris2 connection.")

(defvar idris2-protocol-version 0 "The protocol version")

(defun idris2-version-hook-function (event)
  (pcase event
    (`(:protocol-version ,version ,_target)
     (setf idris2-protocol-version version)
     (remove-hook 'idris2-event-hooks 'idris2-version-hook-function)
     t)))

(defvar-local idris2-load-packages nil
  "The list of packages to be loaded by Idris2. Set using file or directory variables.")

(defun idris2-compute-flags ()
  "Calculate the command line options to use when running Idris2."
  (append (cl-loop for p in idris2-load-packages
                   collecting "-p"
                   collecting p)
          idris2-interpreter-flags
          (cl-mapcan #'funcall
                     idris2-command-line-option-functions)))

(defvar idris2-current-flags nil
  "The list of command-line-args actually passed to Idris2. This
  is maintained to restart Idris2 when the arguments change.")

(autoload 'idris2-prover-event-hook-function "idris2-prover.el")
(autoload 'idris2-quit "idris2-commands.el")
(defun idris2-run ()
  "Run an inferior Idris2 process."
  (interactive)
  (let ((command-line-flags (idris2-compute-flags)))
    ;; Kill the running Idris2 if the command-line flags need updating
    (when (and (get-buffer-process (get-buffer (idris2-buffer-name :connection)))
               (not (equal command-line-flags idris2-current-flags)))
      (message "Idris2 command line arguments changed, restarting Idris2")
      (idris2-quit)
      (sit-for 0.01)) ; allows the sentinel to run and reset idris2-process
    ;; Start Idris2 if necessary
    (when (not idris2-process)
      (setq idris2-process
            (get-buffer-process
             (apply #'make-comint-in-buffer
                    "idris2"
                    (idris2-buffer-name :process)
                    idris2-interpreter-path
                    nil
                    "--ide-mode-socket"
                    command-line-flags)))
      (with-current-buffer (idris2-buffer-name :process)
        (add-hook 'comint-preoutput-filter-functions
                  'idris2-process-filter
                  nil
                  t)
        (add-hook 'comint-output-filter-functions
                  'idris2-show-process-buffer
                  nil
                  t))
      (set-process-sentinel idris2-process 'idris2-sentinel)
      (setq idris2-current-flags command-line-flags)
      (accept-process-output idris2-process 3))))

(defun idris2-connect (port)
  "Establish a connection with a Idris2 REPL."
  (when (not idris2-connection)
    (setq idris2-connection
          (open-network-stream "Idris2 IDE support" (idris2-buffer-name :connection) "127.0.0.1" port))
    (add-hook 'idris2-event-hooks 'idris2-version-hook-function)
    (add-hook 'idris2-event-hooks 'idris2-log-hook-function)
    (add-hook 'idris2-event-hooks 'idris2-warning-event-hook-function)
    (add-hook 'idris2-event-hooks 'idris2-prover-event-hook-function)
    (set-process-filter idris2-connection 'idris2-output-filter)
    (set-process-sentinel idris2-connection 'idris2-sentinel)
    (set-process-query-on-exit-flag idris2-connection t)
    (setq idris2-process-current-working-directory "")
    (run-hooks 'idris2-run-hook)
    (message "Connected. %s" (idris2-random-words-of-encouragement))))

(defun idris2-sentinel (_process msg)
  (message "Idris2 disconnected: %s" (substring msg 0 -1))
  (when idris2-connection
    (delete-process idris2-connection)
    (setq idris2-connection nil))
  (when idris2-process
    (delete-process idris2-process)
    (setq idris2-process nil)))

(defvar idris2-process-port-output-regexp (rx (? (group (+ any (not num)))) (group (+ (any num))))
  "Regexp used to match the port of an Idris2 process.")

(defun idris2-process-filter (string)
  "Accept output from the process"
  (if idris2-connection
      string
    ;; Idris2 sometimes prints a warning prior to the port number, which causes
    ;; `string-match' to return 0
    (cl-flet ((idris2-warn (msg)
                          (unless (or (null msg) (string-blank-p msg))
                            (message "Warning from Idris2: %s" msg))))
      (if (not (string-match idris2-process-port-output-regexp string))
          (idris2-warn string)
        (idris2-warn (match-string 1 string))
        (idris2-connect (string-to-number (match-string 2 string))))
      "")))

(defun idris2-show-process-buffer (string)
  "Show the Idris2 process buffer if STRING is non-empty."
  (when (> (length string) 0)
    (pop-to-buffer (get-buffer (idris2-buffer-name :process)))))

(defun idris2-output-filter (process string)
  "Accept output from the socket and process all complete messages"
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (idris2-connection-available-input process))

(defun idris2-connection-available-input (process)
  "Process all complete messages which arrived from Idris2."
  (with-current-buffer (process-buffer process)
    (while (idris2-have-input-p)
      (let ((event (idris2-receive)))
        (idris2-event-log event nil)
        (unwind-protect
            (save-current-buffer
              (idris2-dispatch-event event process)))))))

(defun idris2-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (idris2-decode-length))))

(defun idris2-receive ()
  "Read a message from the idris2 process"
  (goto-char (point-min))
  (let* ((length (idris2-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun idris2-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun idris2-send (sexp proc)
  "Send a SEXP to Idris2 over the PROC. This is the lowest level of communication."
  (let* ((msg (concat (idris2-prin1-to-string sexp) "\n"))
         (string (concat (idris2-encode-length (length msg)) msg)))
    (idris2-event-log sexp t)
    (process-send-string proc string)))

(defun idris2-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun idris2-prin1-to-string (sexp)
  "Like `prin1-to-string', but don't octal-escape non-ascii characters."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defvar idris2-rex-continuations '()
  "List of (ID FUNCTION [FUNCTION]) continuations waiting for RPC
  results. The first function will be called with a final result,
  and the second (if present) will be called with intermediate
  output results.")

(defvar idris2-continuation-counter 1
  "Continuation serial number counter.")

(defvar idris2-event-hooks)

(defun idris2-dispatch-event (event process)
  (or (run-hook-with-args-until-success 'idris2-event-hooks event)
      (destructure-case event
        ((:emacs-rex form continuation &optional output-continuation)
         (let ((id (cl-incf idris2-continuation-counter)))
           (idris2-send `(,form ,id) process)
           (push (if output-continuation
                     (list id continuation output-continuation)
                   (list id continuation))
                 idris2-rex-continuations)))
        ((:output value id)
         (let ((rec (assq id idris2-rex-continuations)))
           ;; Commands that don't ask for :output don't get it
           (when (and rec (nth 2 rec))
             (funcall (nth 2 rec) value))))
        ((:return value id)
         (let ((rec (assq id idris2-rex-continuations)))
           (cond (rec (setf idris2-rex-continuations
                            (remove rec idris2-rex-continuations))
                      (funcall (cadr rec) value))
                 (t (error "Unexpected reply: %S %S" id value))))))))

(cl-defmacro idris2-rex ((&rest saved-vars) sexp intermediate &rest continuations)
  "(idris2-rex (VAR ...) (SEXP) INTERMEDIATE CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Idris2.

If INTERMEDIATE is non-nil, also register for intermediate results.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:error CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 3))
  (let ((result (cl-gensym)))
    `(let ,(cl-loop for var in saved-vars
                    collect (cl-etypecase var
                              (symbol (list var var))
                              (cons var)))
       (idris2-dispatch-event
        (list :emacs-rex ,sexp
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations))
              ,@(when intermediate
                  `((lambda (,result)
                      (destructure-case ,result
                        ,@continuations)))))
        idris2-connection))))

(defun idris2-eval-async (sexp cont &optional failure-cont)
  "Evaluate EXPR on the superior Idris2 and call CONT with the result, or FAILURE-CONT in failure case."
  (idris2-rex (cont (buffer (current-buffer)) failure-cont)
      sexp t
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:error condition &optional _spans)
     (when failure-cont
       (set-buffer buffer)
       (funcall failure-cont condition))
     (message "Evaluation returned an error: %s." condition))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar idris2-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(autoload 'idris2-list-compiler-notes "idris2-warnings-tree.el")
(defun idris2-eval (sexp &optional no-errors)
  "Evaluate EXPR on the inferior Idris2 and return the result,
ignoring intermediate output. If `NO-ERRORS' is non-nil, don't
trigger warning buffers and don't call `ERROR' if there was an
Idris2 error."
  (let* ((tag (cl-gensym (format "idris2-result-%d-"
                                 (1+ idris2-continuation-counter))))
	 (idris2-stack-eval-tags (cons tag idris2-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (idris2-rex (tag sexp)
           sexp nil
         ((:ok value &optional spans)
          (if (member tag idris2-stack-eval-tags)
              (throw tag (list #'identity (cons value spans)))
            (if no-errors
                nil
              (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                     tag sexp))))
         ((:error condition &optional _spans)
          (if no-errors
              (throw tag (list #'identity nil))
            (when (member 'warnings-tree idris2-warnings-printing)
              (when (idris2-list-compiler-notes)
                (pop-to-buffer (idris2-buffer-name :notes))))
            (throw tag (list #'error "%s (synchronous Idris2 evaluation failed)" condition)))))
       (let ((debug-on-quit t)
             (inhibit-quit nil))
         (while t
           (when (eq (process-status idris2-process) 'exit)
             (error "Idris2 process exited unexpectedly"))
           (accept-process-output idris2-connection 0.1)))))))

(defvar idris2-options-cache '()
  "An alist caching the Idris2 interpreter options, to
  allow consulting them when the Idris2 interpreter is busy.")

(defun idris2-update-options-cache ()
  (idris2-eval-async '(:get-options)
                    #'(lambda (opts) (setq idris2-options-cache opts))))

(defun idris2-get-options ()
  (idris2-eval '(:get-options)))

(defun idris2-get-option (opt)
  ;; First check the local cache
  (let ((local-val (assoc opt idris2-options-cache)))
    (if local-val
        (equal (cadr local-val) :True)
      (let ((remote-val (assoc opt (car (idris2-get-options)))))
        (if remote-val
            (equal (cadr remote-val) :True)
          (error "Unknown Idris2 option %s" opt))))))

(defun idris2-set-option (opt b)
  (let ((bi (if b :True :False)))
    (idris2-rex ((buffer (current-buffer)) opt b bi)
        `(:set-option ,opt ,bi) nil
      ((:ok _res)
       (set-buffer buffer)
       (let ((cache-elt (assoc opt idris2-options-cache)))
         (if cache-elt
             (setf (cadr cache-elt) bi)
           (add-to-list 'idris2-options-cache (list opt bi)))))
      ((:error condition &optional _spans)
       (message "Setting option %s to %s returned an error: %s." opt b condition)))))

(defun idris2-get-idris2-version ()
  "Ask the Idris2 compiler for its version information.
Returns a cons cell whose car is a list of version number
components and whose cdr is a list of prerelease identifiers, if
applicable. Returns nil if the version of Idris2 used doesn't
support asking for versions."
  (pcase (idris2-eval :version t)
    (`((,version ,prerelease)) (cons version prerelease))
    (_ nil)))

(defun idris2-get-idris2-version-string ()
  "Ask the Idris2 compiler for its version information, and return the result as a user-friendly string.
Returns nil if the version of Idris2 used doesn't support asking
for versions."
  (let ((version (idris2-get-idris2-version)))
    (if (consp version) ; returns nil on older versions of Idris2
        (let* ((version-number (car version))
               (version-prerelease (cdr version)))
          (concat (mapconcat #'number-to-string version-number ".")
                  (if version-prerelease
                      (concat "-" (mapconcat #'identity version-prerelease "-"))
                    "")))
      nil)))


(provide 'inferior-idris2)
