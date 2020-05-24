[![MELPA Stable](http://stable.melpa.org/packages/idris2-mode-badge.svg)](http://stable.melpa.org/#/idris2-mode) [![MELPA](http://melpa.org/packages/idris2-mode-badge.svg)](http://melpa.org/#/idris2-mode)

![Interactive editing](http://itu.dk/people/drc/idris2-mode.gif)

# idris2-mode for emacs

This is an emacs mode for editing [Idris2][] code.

Syntax highlighting functional languages is a little quirky; I've
deferred to haskell-mode for some things
(e.g. `font-lock-variable-name-face` for operators) and done some
other things as appropriate for Idris2. `font-lock-type-face` isn't
used at all, for example, and data types declared with `data` use the
same face as functions and values defined with `=`.

![Screenshot](http://itu.dk/~hame/idris2-emacs.png)

## Changes

See the file CHANGES.markdown in the repository root for user-visible
changes between versions, starting at version 0.9.18.

## Documentation

There are some docstrings spread around the mode, thus C-h m is helpful and returns the available key bindings.
The REPL also supports tab completion, thus pressing tab opens a buffer with the available completions.

The [extended abstract] for DTP14 gives overview of the features of idris2-mode. The underlying [protocol] is described in the Idris2 documentation.

[extended abstract]: http://davidchristiansen.dk/pubs/dtp2014-idris2-mode.pdf
[protocol]: http://docs.idris2-lang.org/en/latest/reference/ide-protocol.html

Certain areas of `idris2-mode` show explanatory help text. When you've learned how to use `idris2-mode`, you can turn these off by setting `idris2-show-help-text` to `nil`.

## Inferior Idris2

There is now support for running an Idris2 interpreter in a buffer. Use
'C-c C-l' to load the current Idris2 buffer into the interpreter. This will
spawn an inferior idris2 process and load the buffer. It will report warnings
if idris2 reports any. Pressing C-c C-l again will reload that buffer - if you
switch to a different buffer and press C-c C-l, that buffer will be loaded
instead.

Customize `idris2-interpreter-path` if idris2 is not on your default path.

[Idris2]: http://www.idris2-lang.org

## Highlighting
`idris2-mode` provides two forms of source code highlighting, that work together: convential Emacs `font-lock` to highlight things like keywords and code that has not yet been type checked, and compiler-supported semantic highlighting of identifiers (as known from Agda). Semantic highlighting is controlled by the variable `idris2-semantic-source-highlighting`.

## Keybindings
`idris2-mode` follows conventions from SLIME whenever possible. In particular:

* For ease of typing, any sequence of three keys allows the final key to be pressed with or without the control key, unless the last key is `h`. For example, `C-c C-d d` can also be typed `C-c C-d C-d`.
* Documentation-related commands are prefixed with `C-c C-d`.
* Commands related to **b**uilding packages are prefixed with `C-c C-b`.

## Contextual menus
`idris2-mode` makes use of semantic information from the Idris2 compiler to display contextual menus. By default, the graphical contextual menu is bound to the right mouse button and the textual contextual menu is bound to `C-c C-SPC`. Using these commands will display commands that are available based on what is under the mouse pointer or what is at point, respectively.

## Error messages

When loading a buffer, `idris2-mode` will decorate errors from the Idris2 compiler with underlines. Tooltips show the error message.
The following error message commands are available:

* `M-n`: Move the point to the next compiler note
* `M-p`: Move the point to the previous comiler note

## Colors and fonts
Idris2 mode displays output from the Idris2 compiler with full semantic highlighting. It is quite possible that this is ugly in your color scheme. If that is the case, you can use `M-x customize-group RET idris2-faces RET` to modify them. In particular, some users don't like the background color for the currently loaded region of the buffer. This is controlled by `idris2-loaded-region-face`. Remove all it's properties to make it disappear.

## Interactive editing

![Interactive editing](http://itu.dk/people/drc/idris2-mode.gif)

The following commands are available when there is an inferior Idris2 process (which is started on demand by the commands):

* `C-c C-l`: Load the current file into Idris2. A prefix argument causes Idris2 to only load the portion of the file up to point.
* `C-u C-u C-c C-l`: Obliterate the loading marker, switching back to loading the whole buffer.
* `C-c C-n`, `C-c C-p`: Extend and contract the region to be loaded, if such a region exists, one line at a time.
* `C-c C-s`: Create an initial pattern match clause for a type declaration
* `C-c C-m`: Add missing pattern-match cases to an existing definition
* `C-c C-a`: Attempt to solve a hole automatically. A plain prefix argument prompts for hints, while a numeric prefix argument sets the recursion depth.
* `C-c C-e`: Extract a hole or provisional definition name to an explicit top level definition
* `C-c C-c`: Case split the pattern variable under point, or fill the hole at point with a case expression.
* `C-c C-t`: Get the type for the identifier under point. A prefix argument prompts for the name.
* `C-c C-w`: Add a with block for the pattern-match clause under point
* `C-c C-h a`: Search names, types, and docstrings for a given string.
* `C-c C-z`: Pop to a presently open REPL buffer

## Online documentation

The Idris2 compiler supports documentation. The following commands access it:
* `C-c C-d d`: Show the documentation for the name under point (`:doc` at the REPL). A prefix argument prompts for the name.
* `C-c C-d a`: Search the documentation for a string (`:apropos` at the REPL).
* `C-c C-d t`: Search for documentation regarding a particular type (`:search` at the REPL).

Additionally, `idris2-mode` integrates with `eldoc-mode`, which shows documentation overviews and type signatures in the minibuffer.

## Completion

`M-Tab` or whatever you have `completion-at-point` bound to will ask the running Idris2 process for completions for the current identifier. Note that this command requires that the Idris2 interpreter is already running, because attempting to load an incomplete buffer would probably not work.

## Active terms

Some terms output by the Idris2 compiler are *active*, meaning that `idris2-mode` is aware of their original representation. For these terms, commands exist to normalise them and show or hide their implicit arguments.

To see the active terms available, use the command `idris2-add-term-widgets`, which is also found in the menu. To issue term commands, right-click on the triangle that points at the term. The widgets can be removed again using `idris2-remove-term-widgets`.

The following keybindings are available:
* `C-c C-m n`: Normalize the term at point (`M-x idris2-normalize-term`)
* `C-c C-m i`: Show implicits for the term at point (`M-x idris2-show-term-implicits`)
* `C-c C-m h`: Hide implicits for the term at point (`M-x idris2-hide-term-implicits`)
* `C-c C-m c`: Show the core language for the term at point (`M-x idris2-show-core-term`)

## Package files
Idris2's build system, which consists of package files ending in `.ipkg`, has rudimentary support from `idris2-mode`. The following commands are available in Idris2 buffers or package buffers; if they are run from an Idris2 buffer, then `idris2-mode` will attempt to locate the package file automatically. The mnemonic for `C-b` in the prefix is "build".
* `C-c C-b c`: Clean the package, removing `.ibc` files
* `C-c C-b b`: Build the package
* `C-c C-b i`: Install the package to the user's repository, building first if necessary

The following commands are available in `idris2-ipkg-mode`:
* `C-c C-f`: Insert a field, with completion support. Completion for field names is also available by pressing `M-TAB`.

When a package is present, `idris2-mode` gains a few convenience features. In particular, the Idris2 compiler's working directory is set based on the `sourcedir` directive in the package file, and certain filenames or module names become clickable buttons, to conveniently open them.

Additionally, the command `M-x idris2-start-project` will create a directory structure and initial package file for a new project.

## Using packages
`idris2-mode` supports setting the packages to be loaded by the Idris2 process. Specifically, the buffer-local variable `idris2-load-packages` is expected to contain a list of package names to be loaded. When the buffer is loaded, if the current packages loaded by the Idris2 process don't agree with the contents of the variable, Idris2 is restarted with the correct `-p` options.

You can set this variable interactively using the command `M-x idris2-set-idris2-load-packages`. This will add the variable as a file-local variable, so that it will be set automatically when the file is loaded in the future.

## Installation

Idris2 mode uses lexical binding and other features not available in versions of Emacs prior to 24. Thus, only Emacs 24 and up are supported.

You can install pre-built packages from [MELPA](http://melpa.org/) or [MELPA Stable](http://stable.melpa.org): `idris2-mode` will automatically be enabled in `.idr` files without any further configuration. Please install a version corresponding to the version of Idris2 that you use. The current Git (and therefore MELPA) version of `idris2-mode` will work with the current Git version of Idris2, while the latest release on Hackage will work with the corresponding tagged version of `idris2-mode`.

If you are using Emacs 24.4 or newer with released versions of Idris2, but you want other packages from MELPA, then you can pin the version of `idris2-mode` to the one found in MELPA Stable. For details, please consult the documentation for `package-pinned-packages` (`C-h v package-pinned-packages RET`).

Alternatively, download the elisp files, and place them somewhere in your load path. If you're installing manually, then you also need to install the [`prop-menu` package](https://github.com/david-christiansen/prop-menu-el), which `idris2-mode` uses to create contextual menus. Installation from MELPA will automatically install dependencies.

If you want `idris2-mode` to be enabled by default, add the line `(require 'idris2-mode)` to your `~/.emacs` or `~/.emacs.d/init.el` file.

Idris2 mode is heavily dependent on the Idris2 compiler for its more advanced features. Thus, please ensure that Emacs can see your Idris2 binary. Emacs looks for executables in the directories specified in the variable `exec-path`, which is initialized from your PATH at startup. If Idris2 is not on your PATH, then you may need to add it to `exec-path` manually. E.g.: if you installed idris2 with cabal into `~/.cabal/bin`, then add the line `(add-to-list 'exec-path "~/.cabal/bin")` to your emacs initialization file. Alternatively, you can customize the variable `idris2-interpreter-path` and provide an absolute path.

## Customization

Customize various aspects of the mode using `M-x customize-group RET idris2 RET`.

Additionally, you may want to update your Emacs configuration so that it does not open Idris2 bytecode files by default. You can do this by adding `".ibc"` to the variable `completion-ignored-extensions`, either in customize or by adding `(add-to-list 'completion-ignored-extensions ".ibc")` to your `init.el`. If you use `ido`, then you may also need to set `ido-ignore-extensions` to `t`.

## Keybinding conventions

All three-letter keybindings are available in versions with and without `C-` on the final key, following the convention from SLIME.

## Tests

Before sending a patch or pull request, please run the automated tests for `idris2-mode` and correct any errors that are found. There are two kinds of test:

1. The Emacs byte code compiler can catch many issues. Running `make compile` will invode the byte code compiler, failing if there are any warnings. You may wish to run `make clean` after `make compile` to get rid of pesky `.elc` files.

2. There is a test suite that can be invoked with `make test`. It requires a functioning `idris2` executable.


## Integration with other Emacs packages

### Evil mode support (Vim compatability)
There is emulation for idris2-vim commands in idris2-mode. To enable this support please install the `evil` and `evil-leader` packages from MELPA (or your favorite source of packages) and then add `(idris2-define-evil-keys)` to `init.el`.

The following commands are supported (taken from idris2-vim):

* `<LocalLeader>r`: Reload file

* `<LocalLeader>t`: Show type

* `<LocalLeader>d`: Add initial pattern-match clause

* `<LocalLeader>c`: Case split

* `<LocalLeader>w`: Add `with` clause

* `<LocalLeader>m`: Add missing pattern-match cases

* `<LocalLeader>p`: Proof search

* `<LocalLeader>h`: Show documentation

### Helm

[`helm-idris2`](https://www.github.com/david-christiansen/helm-idris2) builds on `idris2-mode` to provide an alternative interface to looking up documentation. It supports incremental searching in documentation and names, similar to the built-in `:apropos` and `idris2-apropos` commands.

### Pop Win

[Pop Win](http://www.emacswiki.org/emacs/PopWin) is an Emacs utility to manage ephemeral buffers, such as completion and compilation buffers. It provides tools for controlling their position and lifetime. Pop Win requires configuration to work with `idris2-mode`. An incomplete example configuration follows:

```elisp
(push 'idris2-compiler-notes-mode
      popwin:special-display-config)
(push '(idris2-repl-mode
        :height 0.2
        :noselect nil
        :position bottom
        :stick t)
      popwin:special-display-config)
```

### Spacemacs
We have received reports that the `idris2-stay-in-current-window-on-compiler-error` setting does not function properly for users of Spacemacs. The following user configuration can fix it:

```elisp
(defun dotspacemacs/user-config ()
  ; ...
  (with-eval-after-load 'idris2-mode
    (setq idris2-stay-in-current-window-on-compiler-error t)
    (dolist (x '("*idris2-notes*" "*idris2-holes*" "*idris2-info*"))
      (plist-put (cdr (assoc x popwin:special-display-config)) :noselect t))))
```


### [`frames-only-mode`](https://github.com/davidshepherd7/frames-only-mode) and many buffer pop-ups

[`frames-only-mode`](https://github.com/davidshepherd7/frames-only-mode) obviates the need for emacs internal windows so that emacs can get along better with tiling managers - such as [xmonad](https://github.com/xmonad/xmonad) - by using emacs' frames instead of windows.

Throughout a session with `idris2-mode`, many frames will accummulate, such as `*idris2-holes*`, and over time these clutter your screen. A quick simple solution is to add the following to your emacs configuration:

```elisp
(defun my-idris2-mode-hook ()

  ;; This makes it so that especially errors reuse their frames
  ;; https://emacs.stackexchange.com/questions/327/how-can-i-block-a-frame-from-being-split/338
  ;; alternatively, add this to certain frames: (set-frame-parameter nil 'unsplittable t)
  ;; (without this, idris2 throws out tons of new frames)
  (add-to-list 'display-buffer-alist
               '(".*". (display-buffer-reuse-window . ((reusable-frames . t)))))
  (setq idris2-stay-in-current-window-on-compiler-error t)
  (setq idris2-prover-restore-window-configuration t)

  ;; If you kill a buffer (eg, hit "q"), frames with these names wil also be killed
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris2-repl*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris2-notes*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris2-info*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris2-holes*"))

(add-hook 'idris2-mode-hook #'my-idris2-mode-hook)
```


