;;; idris2-ipkg-mode.el --- Major mode for editing Idris2 package files -*- lexical-binding: t -*-

;; Copyright (C) 2014

;; Author: David Raymond Christiansen
;; URL: https://github.com/idris2-hackers/idris2-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24"))


;;; Commentary:

;; This is an Emacs mode for editing Idris2 packages. It requires the latest
;; version of Idris2, and some features may rely on the latest Git version of
;; Idris2.

;;; Code:
(require 'ansi-color)
(require 'compile)

(require 'idris2-core)
(require 'idris2-settings)
(require 'idris2-common-utils)
(require 'idris2-keys)

;;; Faces

(defface idris2-ipkg-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris2 package keywords"
  :group 'idris2-faces)

(defface idris2-ipkg-package-name-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to highlight the name of the package"
  :group 'idris2-faces)


;;; Syntax

(defconst idris2-ipkg-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    ;; Matching {}, but with nested comments
    (modify-syntax-entry ?\{ "(} 1bn" st)
    (modify-syntax-entry ?\} "){ 4bn" st)
    (modify-syntax-entry ?\- "_ 123" st)
    (modify-syntax-entry ?\n ">" st)

    st))

(defconst idris2-ipkg-keywords
  '("package"
    "authors"
    "maintainers"
    "license"
    "brief"
    "readme"
    "homepage"
    "sourceloc"
    "bugtracker"
    "options"
    "opts"
    "sourcedir"
    "builddir"
    "outputdir"
    "prebuild"
    "postbuild"
    "preinstall"
    "postinstall"
    "preclean"
    "postclean"
    "langversion"
    "version"
    "depends"
    "modules"
    "main"
    "executable"))

(defconst idris2-ipkg-font-lock-defaults
  `(,idris2-ipkg-keywords))


;;; Completion

(defun idris2-ipkg-find-keyword ()
  (let ((start nil)
        (end (point))
        (failure (list nil nil nil)))
    (if (idris2-is-ident-char-p (char-before))
        (progn
          (save-excursion
            (while (idris2-is-ident-char-p (char-before))
              (backward-char))
            (setq start (point)))
          (if start
              (list (buffer-substring-no-properties start end)
                    start
                    end)
            failure))
      failure)))

(defun idris2-ipkg-complete-keyword ()
  "Complete the current .ipkg keyword, if possible"
  (interactive)
  (cl-destructuring-bind (identifier start end) (idris2-ipkg-find-keyword)
    (when identifier
      (list start end idris2-ipkg-keywords))))

;;; Inserting fields
(defun idris2-ipkg-insert-field ()
  "Insert one of the ipkg fields"
  (interactive)
  (let ((field (completing-read "Field: " (remove "package" idris2-ipkg-keywords) nil t)))
    (beginning-of-line)
    (while (and (not (looking-at-p "^\\s-*$")) (= (forward-line) 0)))
    (beginning-of-line)
    (when (not (looking-at-p "^\\s-*$")) ;; end of buffer had stuff
      (goto-char (point-max))
      (newline))
    (newline)
    (insert field " = ")
    (let ((p (point)))
      (newline)
      (goto-char p))))

;;; Clickable modules

(defun idris2-ipkg-make-files-clickable ()
  "Make all modules with existing files clickable, where clicking opens them"
  (interactive)
  (idris2-clear-file-link-overlays 'idris2-ipkg-mode)
  (let ((src-dir (idris2-ipkg-buffer-src-dir (file-name-directory (buffer-file-name)))))
    ;; Make the sourcedir clickable
    (save-excursion
      (goto-char (point-min))
      (when (and (file-exists-p src-dir)
                 (file-directory-p src-dir)
                 (re-search-forward "^sourcedir\\s-*=\\s-*\\([a-zA-Z/0-9]+\\)" nil t))
        (let ((start (match-beginning 1))
              (end (match-end 1))
              (map (make-sparse-keymap)))
          (define-key map [mouse-2] #'(lambda ()
                                        (interactive)
                                        (dired src-dir)))
          (idris2-make-file-link-overlay start end map
                                        (concat "mouse-2: dired " src-dir)))))
    ;; Make the modules clickable
    (save-excursion
      (goto-char (point-min))
      (cl-flet ((mod-link ()
                  (re-search-forward "[a-zA-Z0-9\\.]+" nil t)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (idris2-make-module-link beg end src-dir))))
        (when (re-search-forward "^modules\\s-*=\\s-*" nil t)
          (cl-loop initially (mod-link)
                   while (looking-at-p "\\s-*,\\s-*")
                   do (progn (skip-chars-forward " ,\n")
                             (mod-link))))))
    ;; Make the Makefile clickable
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^makefile\\s-*=\\s-*\\([a-zA-Z/0-9]+\\)" nil t)
        (let ((start (match-beginning 1))
              (end (match-end 1))
              (makefile (concat (file-name-as-directory src-dir) (match-string 1))))
        (when (file-exists-p makefile)
          (let ((map (make-sparse-keymap)))
            (define-key map [mouse-2] #'(lambda ()
                                          (interactive)
                                          (find-file makefile)))
            (idris2-make-file-link-overlay start end map  "mouse-2: edit makefile"))))))))


(defun idris2-ipkg-enable-clickable-files ()
  "Enable setting up clickable modules and makefiles on idle Emacs"
  (interactive)
  (add-hook 'after-save-hook 'idris2-ipkg-make-files-clickable)
  (idris2-ipkg-make-files-clickable))

;;; finding ipkg files

;; Based on http://www.emacswiki.org/emacs/EmacsTags section "Finding tags files"
;; That page is GPL, so this is OK to include
(defun idris2-find-file-upwards (suffix &optional allow-hidden)
  "Recursively searches each parent directory starting from the
directory of the current buffer filename or from
`default-directory' if that's not found, looking for a file with
name ending in SUFFIX.  Returns the paths to the matching files,
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (matching (if parent
                                         (idris2-try-directory-files parent t (concat "\\\." suffix "$"))
                                       nil)))
                      (cond
                       (matching matching)
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (let* ((file (buffer-file-name (current-buffer)))
           (dir (if file (file-name-directory file) default-directory)))
      (when dir
        (cl-remove-if #'(lambda (f)
                          (and (not allow-hidden)
                               (string-prefix-p "." (file-name-nondirectory f))))
                      (find-file-r dir))))))

(defun idris2-try-directory-files (directory &optional full match nosort)
  "Call `directory-files' with arguments DIRECTORY, FULL, MATCH,
and NOSORT, but return the empty list on failure instead of
throwing an error.

See the docstring for `directory-files' for the meaning of the
arguments."
  ;; This wrapper is useful because some users can't read all the
  ;; directories above the current working directory. In particular,
  ;; /home is sometimes not readable.
  (condition-case nil
      (directory-files directory full match nosort)
    (error nil)))

(defvar idris2-ipkg-build-buffer-name "*idris2-build*")

(defun idris2-ipkg--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the idris2-mode compilation buffer."
  idris2-ipkg-build-buffer-name)

(defun idris2-ipkg--ansi-compile-filter (start)
  "Apply ANSI formatting to the region of the buffer from START to point."
  (save-excursion
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region start (point)))))

(defun idris2-ipkg-command (ipkg-file command)
  "Run a command on ipkg-file. The command can be build, install, or clean."
  ;; Idris2 must have its working directory in the same place as the ipkg file
  (let ((dir (file-name-directory ipkg-file))
        (file (file-name-nondirectory ipkg-file))
        (opt (cond ((equal command 'build) "--build")
                   ((equal command 'install) "--install")
                   ((equal command 'clean) "--clean")
                   (t (error "Invalid command name %s" command)))))
    (unless dir
      (error "Unable to determine directory for filename '%s'" ipkg-file))
    (let* ((default-directory dir) ; default-directory is a special variable - this starts idris2 in dir
           (compilation-buffer-name-function
            'idris2-ipkg--compilation-buffer-name-function)
           (command (concat idris2-interpreter-path " " opt " " file))
           (compilation-filter-hook
            (cons 'idris2-ipkg--ansi-compile-filter compilation-filter-hook)))
      (compile command))))

(defun idris2-ipkg-build (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris2-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to build: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to build: " nil nil nil t)))))
  (idris2-ipkg-command ipkg-file 'build))

(defun idris2-ipkg-install (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris2-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to install: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to install: " nil nil nil t)))))
  (idris2-ipkg-command ipkg-file 'install))

(defun idris2-ipkg-clean (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris2-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to clean: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to clean: " nil nil nil t)))))
  (idris2-ipkg-command ipkg-file 'clean))

(defun idris2-ipkg-build-quit ()
  (interactive)
  (idris2-kill-buffer idris2-ipkg-build-buffer-name))

(defun idris2-ipkg-buffer-src-dir (basename)
  (save-excursion
    (goto-char (point-min))
    (let ((found
           (re-search-forward "^\\s-*sourcedir\\s-*=\\s-*\\(\\sw+\\)"
                              nil
                              t)))
      (if found
          (let ((subdir (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
            (concat (file-name-directory basename) subdir))
        (file-name-directory basename)))))

(defun idris2-ipkg-find-src-dir (&optional ipkg-file)
  (let ((found (or (and ipkg-file (list ipkg-file))
                   (idris2-find-file-upwards "ipkg"))))
    (if (not found)
        nil
      (setq ipkg-file (car found))
      ;; Now ipkg-file contains the path to the package
      (with-temp-buffer
        (insert-file-contents ipkg-file)
        (idris2-ipkg-buffer-src-dir ipkg-file)))))

(defun idris2-ipkg-buffer-cmdline-opts ()
  (save-excursion
    (goto-char (point-min))
    (let ((found
           (re-search-forward "^\\s-*opts\\s-*=\\s-*\"\\([^\"]*\\)\""
                              nil
                              t)))
      (if found
          (buffer-substring-no-properties (match-beginning 1) (match-end 1))
        ""))))

(defun idris2-ipkg-find-cmdline-opts (&optional ipkg-file)
  (let ((found (or (and ipkg-file (list ipkg-file))
                   (idris2-find-file-upwards "ipkg"))))
    (if (not found)
        nil
      (setq ipkg-file (car found))
      ;; Now ipkg-file contains the path to the package
      (with-temp-buffer
        (insert-file-contents ipkg-file)
        (idris2-ipkg-buffer-cmdline-opts)))))

(defun idris2-ipkg-flags-for-current-buffer ()
  "Extract the command line options field from the current .ipkg buffer."
  (let ((opts (idris2-ipkg-find-cmdline-opts)))
    (if (stringp opts)
        (split-string opts nil t)
      nil)))

(defun idris2-ipkg-pkgs-for-current-buffer ()
  "Find the explicit list of packages for the current .ipkg buffer."
  (let ((file (idris2-find-file-upwards "ipkg")))
    (when file
      (with-temp-buffer
        (let ((pkgs nil))
          (cl-flet
              ((get-pkg ()
                        (re-search-forward "[a-zA-Z0-9\\._-]+" nil t)
                        (let ((beg (match-beginning 0))
                              (end (match-end 0)))
                          (push (buffer-substring-no-properties beg end) pkgs))))
            (insert-file-contents (car file))
            (goto-char (point-min))
            (when (re-search-forward "^\\s-*pkgs\\s-*=\\s-*" nil t)
              (cl-loop initially (get-pkg)
                       while (looking-at-p "\\s-*,\\s-*")
                       do (progn (skip-chars-forward " ,\n")
                                 (get-pkg)))))
          pkgs)))))

(defun idris2-ipkg-pkgs-flags-for-current-buffer ()
  "Compute a list of Idris2 command line options based on the pkgs field of the .ipkg file."
  (let ((pkgs (idris2-ipkg-pkgs-for-current-buffer)))
    (cl-loop for pkg in pkgs appending (list "-p" pkg))))

(add-to-list 'idris2-command-line-option-functions 'idris2-ipkg-flags-for-current-buffer)
(add-to-list 'idris2-command-line-option-functions 'idris2-ipkg-pkgs-flags-for-current-buffer)

;;; Settings

(defgroup idris2-ipkg nil "Idris2 package mode" :prefix 'idris2-ipkg :group 'idris2)

(defcustom idris2-ipkg-mode-hook '(idris2-ipkg-enable-clickable-files)
  "Hook to run when setting up the mode for editing Idris2 packages."
  :type 'hook
  :options '(idris2-ipkg-enable-clickable-files)
  :group 'idris2-ipkg)

;;; Mode definition

(defvar idris2-ipkg-mode-map (let ((map (make-sparse-keymap)))
                              (cl-loop for keyer
                                       in '(idris2-define-ipkg-keys
                                            idris2-define-ipkg-editing-keys)
                                       do (funcall keyer map))
                              map)
  "Keymap used for Idris2 package mode")

(easy-menu-define idris2-ipkg-mode-menu idris2-ipkg-mode-map
  "Menu for Idris2 package mode"
  `("IPkg"
    ["Build package" idris2-ipkg-build t]
    ["Install package" idris2-ipkg-install t]
    ["Clean package" idris2-ipkg-clean t]
    "----------------"
    ["Insert field" idris2-ipkg-insert-field t]))

;;;###autoload
(define-derived-mode idris2-ipkg-mode prog-mode "Idris2 Pkg"
  "Major mode for Idris2 package files
     \\{idris2-ipkg-mode-map}
Invokes `idris2-ipkg-mode-hook'."
  :group 'idris2
  :syntax-table idris2-ipkg-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       idris2-ipkg-font-lock-defaults)
  (set (make-local-variable 'completion-at-point-functions)
       '(idris2-ipkg-complete-keyword)))

;; Make filenames clickable
(add-to-list 'compilation-error-regexp-alist-alist
             `(idris2-type-checking
               "Type checking \\(.+\\)$" 1 nil nil 0 1))

(cl-pushnew 'idris2-type-checking compilation-error-regexp-alist)

(push '("\\.ipkg$" . idris2-ipkg-mode) auto-mode-alist)

(provide 'idris2-ipkg-mode)

;;; idris2-ipkg-mode.el ends here
