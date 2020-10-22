# idris2-mode for emacs

This is an emacs mode for editing Idris2 code.

It's a copy of (https://github.com/idris-hackers/idris-mode) mostly, for working with Idris2 in its present state (Not all functionality implemented there).

Install it by running within your ~/.emacs.d directory:

git clone https://github.com/redfish64/idris2-mode

Then in your ~/.emacs.d/init.el, add:
(add-to-list 'load-path "~/.emacs.d/idris2-mode/")
(require 'idris2-mode)

I fixed proof-search and make-lemma, so far.
