# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Breaking changes
* The way to dynamically generate the candidate list has changed.
  Instead of rebinding `selectrum-preprocess-candidates-function` and
  `selectrum-refine-candidates-function`, you simply pass a function
  as the COLLECTION argument to `selectrum-read`. This function takes
  one argument, the current user input, and returns the list of
  candidates as strings. Alternatively, it can return an alist whose
  `candidates` key is the candidate list and whose `input` key is a
  transformed user input to use for highlighting.

  As part of this change, `selectrum-refine-candidates-function` no
  longer can return an alist; that functionality should instead be
  moved to the CANDIDATES function. (This feature was never properly
  supported in the first place if you tried to use it in a way that
  can't be done equivalently in the CANDIDATES function.)

  See [#27].

### Features
* Candidates inserted by `selectrum-insert-current-candidate` are now
  added to history ([#54]).
* You can resume the last completion session using the
  `selectrum-repeat` command. (Note that you must bind this command to
  a key sequence in order to use it.) This command implements similar
  functionality to `ivy-resume`. See [#39].
* Experimental support for using Selectrum as a backend for Helm
  commands. Use it by loading the `selectrum-helm` library and
  enabling `selectrum-helm-mode`. See [#18].
* You can now give a prefix argument to
  `selectrum-select-current-candidate` to select the candidate at a
  given index directly. New user option `selectrum-show-indices` to
  display these indices for your convenience. This feature implements
  similar functionality to `ivy-avy`. See [#16].
* Recursive minibuffers are now supported.
* In the standard `completing-read` interface, you can use Isearch to
  retrieve history elements. The Isearch entry-point bindings now work
  properly in Selectrum too, except that they allow you to select a
  history element using Selectrum. See [#49].
* You can now cause the minibuffer to always have the same height,
  even if there are fewer candidates, by enabling
  `selectrum-fix-minibuffer-height` ([#35]).
* Multiple candidate selection is now supported, and we provide a
  `selectrum-completing-read-multiple` function which is installed
  automatically by `selectrum-mode`. This means that commands like
  `describe-face` (which delegate to `completing-read-multiple`
  internally) now use Selectrum by default. To select additional
  candidates within a supported command, use `M-RET`. The feature is
  supported by a new keyword argument `:multiple` to `selectrum-read`.
  We have a new face `selectrum-additional-candidate` which determines
  how selected candidates other than the current candidate are
  highlighted. See [#53].
* We provide a `selectrum-completion-in-region` function now and
  install it on `completion-in-region-function` in `selectrum-mode`,
  so `completion-at-point` will use Selectrum when there is more than
  one completion ([#42]).

### Enhancements
* `selectrum-read-file-name` which is used as
  `read-file-name-function` now uses `read-file-name-default`
  internally. This means all default features of file completion
  should be available now. Most notably you can now use `M-n` to
  insert file names into the minibuffer (using
  `file-name-at-point-functions`) and you are able to use shortcuts
  like `//` or `~/` ([#50], [#52]).
* When reading directories using `read-directory-name` the default is
  sorted to the top instead of inserting it ([#50]).
* In `read-file-name`, when a default is provided (for example in the
  `dired-do-rename` command), we actually use it as the initial
  contents of the minibuffer, which allows you to have convenient
  access to the default filename when that default file does not exist
  ([#25]).
* We now bind `minibuffer-completing-file-name` during
  `read-file-name`, in conformance with the standard Emacs interface
  ([#30]).
* A new text property `selectrum-candidate-display-right-margin` is
  added, to display a string at the right margin after a candidate
  ([#44]).
* You can now access standard minibuffer history using `M-p` and `M-n`
  ([#4], [#38]).
* Previously, setting `resize-mini-windows` to nil would cause
  Selectrum to be unable to display any candidates. This has been
  fixed by having Selectrum bind the variable to `grow-only` when
  entering the minibuffer ([#35]).
* Previously, a large value of `selectrum-num-candidates-displayed`
  would produce a confusing result because `max-mini-window-height`
  imposed a lower limit on the height of the minibuffer. Now that
  variable is bound automatically by Selectrum based on the value of
  `selectrum-num-candidates-displayed` ([#22]).
* Multiline candidates are now displayed properly and do not mess up
  scrolling in the candidate list ([#12]).
* When you select the user input area and it doesn't have anything
  typed, we now show an overlay indicating that you are in this state,
  so it is less confusing. The overlay shows what default value will
  be submitted if you press return. See [#55].
* Switching buffers is now less confusing, because we don't modify the
  order of the buffer list at all. Previously the default buffer to
  switch to was moved to the top of the list. Now we leave it where it
  is, and just select it initially. `selectrum-read` grows a new
  argument `:no-move-default-candidate` to support this improvement.

### Bugs fixed
* You can now use the undo system in the minibuffer. Previously,
  trying to do so would break Selectrum ([#31]).
* Passing a list of symbols to `selectrum-completing-read` works now.
* Previously, `selectrum-read-buffer` ignored its PREDICATE argument.
  This has now been fixed ([#32], [#33]).
* Previously, `selectrum-read` would return nil when
  `selectrum-submit-exact-input` was used on an empty input and no
  `:default-candidate` was provided. Now the empty string is returned,
  in accordance with the `completing-read` API ([#34]).
* The keymap used in the minibuffer now inherits from
  `minibuffer-local-map`, so standard minibuffer bindings should still
  work ([raxod502/ctrlf#41]).
* The application of face `selectrum-current-candidate` does not
  trample on the results of `selectrum-highlight-candidates-function`.
  In other words, the matched part of the current candidate is now
  highlighted just like the matched part of the other candidates. See
  [#21].
* Previously, an error was thrown if you used certain non-Selectrum
  minibuffer commands before loading Selectrum. This has been fixed
  ([#28]).

[#4]: https://github.com/raxod502/selectrum/issues/4
[#12]: https://github.com/raxod502/selectrum/issues/12
[#16]: https://github.com/raxod502/selectrum/issues/16
[#18]: https://github.com/raxod502/selectrum/issues/18
[#21]: https://github.com/raxod502/selectrum/issues/21
[#22]: https://github.com/raxod502/selectrum/issues/22
[#25]: https://github.com/raxod502/selectrum/pull/25
[#27]: https://github.com/raxod502/selectrum/pull/27
[#28]: https://github.com/raxod502/selectrum/issues/28
[#30]: https://github.com/raxod502/selectrum/issues/30
[#31]: https://github.com/raxod502/selectrum/issues/31
[#32]: https://github.com/raxod502/selectrum/issues/32
[#33]: https://github.com/raxod502/selectrum/pull/33
[#34]: https://github.com/raxod502/selectrum/pull/34
[#35]: https://github.com/raxod502/selectrum/issues/35
[#38]: https://github.com/raxod502/selectrum/pull/38
[#39]: https://github.com/raxod502/selectrum/issues/39
[#42]: https://github.com/raxod502/selectrum/issues/42
[#44]: https://github.com/raxod502/selectrum/pull/44
[#49]: https://github.com/raxod502/selectrum/issues/49
[#52]: https://github.com/raxod502/selectrum/issues/52
[#53]: https://github.com/raxod502/selectrum/issues/53
[#54]: https://github.com/raxod502/selectrum/pull/54
[#55]: https://github.com/raxod502/selectrum/issues/55
[raxod502/ctrlf#41]: https://github.com/raxod502/ctrlf/issues/41

## 1.0 (released 2020-03-23)
### Added
* Package `selectrum`
* Minor mode `selectrum-mode`
* Faces:
    * `selectrum-current-candidate`
    * `selectrum-primary-highlight`
    * `selectrum-secondary-highlight`
* Interface user options:
    * `selectrum-num-candidates-displayed`
    * `selectrum-minibuffer-bindings`
    * `selectrum-count-style`
* Commands bound in minibuffer:
    * `selectrum-previous-candidate`
    * `selectrum-next-candidate`
    * `selectrum-previous-page`
    * `selectrum-next-page`
    * `selectrum-goto-beginning`
    * `selectrum-goto-end`
    * `selectrum-kill-ring-save`
    * `selectrum-select-current-candidate`
    * `selectrum-submit-exact-input`
    * `selectrum-insert-current-candidate`
* Entry points:
    * `selectrum-read`
    * `selectrum-completing-read`
    * `selectrum-read-buffer`
    * `selectrum-read-file-name`
    * `selectrum-read-directory-name`
    * `selectrum-read-library-name`
* Hooks:
    * `selectrum-candidate-selected-hook`
    * `selectrum-candidate-inserted-hook`
* API user options, variables, and functions:
    * `selectrum-refine-candidates-function`
    * `selectrum-default-candidate-refine-function`
    * `selectrum-preprocess-candidates-function`
    * `selectrum-default-candidate-preprocess-function`
    * `selectrum-highlight-candidates-function`
    * `selectrum-default-candidate-highlight-function`
    * `selectrum-should-sort-p`

[keep a changelog]: https://keepachangelog.com/en/1.0.0/
