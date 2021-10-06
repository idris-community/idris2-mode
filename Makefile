# Makefile for idris2-mode, to run tests and ensure dependencies are in order
# Portions based on the Makefile for Proof General

EMACS = emacs

NEED_PKGS = prop-menu

BATCHEMACS = $(EMACS) --batch --no-site-file -q \
        -eval '(add-to-list (quote load-path) "${PWD}/")' \
        -eval '(require (quote package))' \
        -eval '(add-to-list (quote package-archives) (quote ("melpa" . "http://melpa.org/packages/")) t)' \
        -eval '(package-initialize)'

BYTECOMP = $(BATCHEMACS) \
        -eval '(require (quote bytecomp))' \
        -eval '(setq byte-compile-warnings t)' \
        -eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile

OBJS =  idris2-commands.elc        \
        idris2-common-utils.elc    \
        idris2-compat.elc          \
        idris2-core.elc            \
        idris2-events.elc          \
        idris2-highlight-input.elc \
        idris2-info.elc            \
        idris2-ipkg-mode.elc       \
        idris2-keys.elc            \
        idris2-log.elc             \
        idris2-hole-list.elc       \
        idris2-mode.elc            \
        idris2-prover.elc          \
        idris2-repl.elc            \
        idris2-settings.elc        \
        idris2-simple-indent.elc   \
        idris2-tree-info.elc       \
        idris2-syntax.elc          \
        idris2-warnings.elc        \
        idris2-warnings-tree.elc   \
        inferior-idris2.elc


.PHONY: getdeps build

.el.elc:
	$(BYTECOMP) $<

build: getdeps $(OBJS)

getdeps:
	$(BATCHEMACS) -eval \
	"(let* ((need-pkgs '($(NEED_PKGS))) \
	        (want-pkgs (seq-remove #'package-installed-p need-pkgs))) \
	   (unless (null want-pkgs) \
	     (package-refresh-contents) \
	     (mapcar #'package-install want-pkgs)))"


.PHONY: test

test: build
	$(BATCHEMACS) -L . -l ert -l idris2-tests.el -f ert-run-tests-batch-and-exit


.PHONY: docs

docs: docs/documentation.tex
	-@( cd docs/ && xelatex documentation.tex )


.PHONY: clean

clean:
	-$(RM) $(OBJS)
	-$(RM) -r docs/auto docs/*.aux docs/*.log docs/*.pdf
