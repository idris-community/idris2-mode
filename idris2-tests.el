;;; idris2-tests.el --- Tests for idris2-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <drc@itu.dk>
;; Keywords: languages

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

;; This is a collection of simple tests for idris2-mode.

;;; Code:

(require 'idris2-mode)
(require 'inferior-idris2)
(require 'idris2-ipkg-mode)
(require 'cl-lib)


(ert-deftest trivial-test ()
  (should t))

(ert-deftest idris2-test-idris2-editor-port ()
  (let ((output "Can't find import Prelude\n37072\n"))
    (should (string-match idris2-process-port-output-regexp output))
    (should (string= "Can't find import Prelude\n" (match-string 1 output)))
    (should (string= "37072" (match-string 2 output))))
  (let ((output "37072\n"))
    (should (string-match idris2-process-port-output-regexp output))
    (should (null (match-string 1 output)))
    (should (string= "37072" (match-string 2 output)))))

(ert-deftest idris2-test-idris2-quit ()
  "Ensure that running Idris2 and quitting doesn't leave behind
unwanted buffers."
  (let ((before (buffer-list)))
    (idris2-repl)
    (dotimes (_ 5) (accept-process-output nil 1))
    (idris2-quit)
    (let* ((after (buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 0)))))

(ert-deftest idris2-test-idris2-quit-logging-enabled ()
  "Ensure that running Idris2 and quitting doesn't leave behind
unwanted buffers. In particular, only *idris2-events* should
remain."
  (let ((before (buffer-list))
        (idris2-log-events 't))
    (idris2-repl)
    (dotimes (_ 5) (accept-process-output nil 1))
    (idris2-quit)
    (let* ((after (buffer-list))
           (extra (cl-set-difference after before)))
      (should (= (length extra) 1))
      (should (string= (buffer-name (car extra)) idris2-event-buffer-name)))

    ;; Cleanup
    (kill-buffer idris2-event-buffer-name)))

(ert-deftest idris2-test-hole-load ()
  "Test the hole-list-on-load setting."
  (idris2-quit)
  ;;; The default setting should be to show holes
  (should idris2-hole-show-on-load)

  (let ((buffer (find-file "test-data/MetavarTest.idr")))
    ;;; Check that the file was loaded
    (should (bufferp buffer))

    ;;; Check that it shows the hole list with the option turned on
    (with-current-buffer buffer
      (idris2-load-file))
    ;;; Allow async stuff to happen
    (dotimes (_ 5) (accept-process-output nil 1))
    (let ((mv-buffer (get-buffer idris2-hole-list-buffer-name)))
      ;; The buffer exists and contains characters
      (should (bufferp mv-buffer))
      (should (> (buffer-size mv-buffer) 10)))
    (idris2-quit)

    ;; Now check that it works with the setting the other way
    (let ((idris2-hole-show-on-load nil))
      (with-current-buffer buffer
        (idris2-load-file))
      (dotimes (_ 5) (accept-process-output nil 1))
      (let ((mv-buffer (get-buffer idris2-hole-list-buffer-name)))
        (should-not (bufferp mv-buffer))
        (should (null mv-buffer))))
    ;; Clean up
    (kill-buffer))

  ;; More cleanup
  (idris2-quit))

(ert-deftest idris2-test-proof-search ()
  "Test that proof search works"
  (idris2-quit)

  (let ((buffer (find-file "test-data/ProofSearch.idr")))
    (with-current-buffer buffer
      (idris2-load-file)
      (dotimes (_ 5) (accept-process-output nil 1))
      (goto-char (point-min))
      (re-search-forward "search_here")
      (goto-char (match-beginning 0))
      (idris2-proof-search)
      (dotimes (_ 5) (accept-process-output nil 1))
      (should (looking-at-p "lteSucc (lteSucc (lteSucc (lteSucc (lteSucc lteZero))))"))
      (move-beginning-of-line nil)
      (delete-region (point) (line-end-position))
      (insert "prf = ?search_here")
      (save-buffer)
      (kill-buffer)))

  ;; More cleanup
  (idris2-quit))

(ert-deftest idris2-test-find-cmdline-args ()
  "Test that idris2-mode calculates command line arguments from .ipkg files."
  ;; Outside of a project, none are found
  (let ((buffer (find-file "test-data/ProofSearch.idr")))
    (with-current-buffer buffer
      (should (null (idris2-ipkg-flags-for-current-buffer)))
      (kill-buffer)))
  ;; Inside of a project, the correct ones are found
  (let ((buffer (find-file "test-data/cmdline/src/Command/Line/Test.idr")))
    (with-current-buffer buffer
      (should (equal (idris2-ipkg-flags-for-current-buffer)
                     (list "-p" "effects")))
      (kill-buffer))))

(ert-deftest idris2-test-error-buffer ()
  "Test that loading a type-incorrect Idris2 buffer results in an error message buffer."
  (let ((buffer (find-file "test-data/TypeError.idr")))
    (with-current-buffer buffer
      (idris2-load-file)
      (dotimes (_ 5) (accept-process-output nil 1))
      (should (get-buffer idris2-notes-buffer-name)))
    (with-current-buffer (get-buffer idris2-notes-buffer-name)
      (goto-char (point-min))
      (should (re-search-forward "Nat" nil t))) ;; check that the buffer has something error-like
    (with-current-buffer buffer
      (kill-buffer))
    (idris2-quit)))

(ert-deftest idris2-test-ipkg-packages-with-underscores-and-dashes ()
  "Test that loading an ipkg file can have dependencies on packages with _ or - in the name."
  (let ((buffer (find-file "test-data/package-test/Packaging.idr")))
    (with-current-buffer buffer
      (should (equal '("-p" "idris2-free" "-p" "recursion_schemes")
                     (idris2-ipkg-pkgs-flags-for-current-buffer)))
      (kill-buffer buffer))
    (idris2-quit)))

(provide 'idris2-tests)
;;; idris2-tests.el ends here
