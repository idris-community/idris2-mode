# idris2-mode for emacs

This is an emacs mode for editing Idris2 code.

It's a copy of (https://github.com/idris-hackers/idris-mode) mostly, for working with Idris2 in its present state. See that readme for more details.

Install it by running within your `~/.emacs.d` directory:
```
$ git clone https://github.com/redfish64/idris2-mode
```

Then in your `~/.emacs.d/init.el` file, add:
```
(add-to-list 'load-path "~/.emacs.d/idris2-mode/")
(require 'idris2-mode)
```

Here is what works so far:
```
M-n		idris2-next-error
M-p		idris2-previous-error

C-c C-a		idris2-proof-search
C-c C-c		idris2-case-dwim
C-c C-e		idris2-make-lemma
C-c C-l		idris2-load-file
C-c C-s		idris2-add-clause
C-c C-t		idris2-type-at-point
C-c C-w		idris2-make-with-block
C-c C-z		idris2-pop-to-repl
C-c C-b C-b	idris2-ipkg-build
C-c C-b C-c	idris2-ipkg-clean
C-c C-b TAB	idris2-ipkg-install
C-c C-b C-p	idris2-open-package-file
C-c C-d C-d	idris2-docs-at-point
C-c C-d C-t	idris2-type-search
<unmapped>  idris2-jump-to-def
<unmapped>  idris2-jump-to-def-same-window
```

Note, if you use evil-mode, you'll probably want to use this to
prevent a huge lag when editing files, see https://github.com/ProofGeneral/PG/issues/427#issuecomment-500616289 for more details:

```
;;Fixes lag when editing idris code with evil
(defun ~/evil-motion-range--wrapper (fn &rest args)
  "Like `evil-motion-range', but override field-beginning for performance.
See URL `https://github.com/ProofGeneral/PG/issues/427'."
  (cl-letf (((symbol-function 'field-beginning)
             (lambda (&rest args) 1)))
    (apply fn args)))
(advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)
```
