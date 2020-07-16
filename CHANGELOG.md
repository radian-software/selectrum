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

* The function `selectrum-read-directory-name` is no longer available.

### Features
* You can now give a prefix argument to
  `selectrum-insert-current-candidate` to insert the candidate at a
  given index directly ([#96]).
* Candidates inserted by `selectrum-insert-current-candidate` are now
  added to history ([#54]).
* You can resume the last completion session using the
  `selectrum-repeat` command. (Note that you must bind this command to
  a key sequence in order to use it.) This command implements similar
  functionality to `ivy-resume`. See [#39].
* Experimental support for using Selectrum as a backend for Helm
  commands. Use it by enabling `selectrum-helm-mode` from the
  `selectrum-helm` library. See [#18].
* You can now give a prefix argument to
  `selectrum-select-current-candidate` to select the candidate at a
  given index directly. New user option `selectrum-show-indices` to
  display these indices for your convenience. This feature implements
  similar functionality to `ivy-avy`. See [#16].
* Recursive minibuffers are now supported.
* In the standard `completing-read` interface, you can use
  `previous-matching-history-element` to retrieve history
  elements. The binding now works properly in Selectrum too, except
  that you can use Selectrum to select a history element. See [#49],
  [#77]. If you prefer to use the original interface you can use
  `selectrum-previous-history-element` which is just not bound by
  default [#57].
* You can now cause the minibuffer to always have the same height,
  even if there are fewer candidates, by enabling
  `selectrum-fix-minibuffer-height` ([#35]).
* Multiple candidate selection is now supported, and we provide a
  `selectrum-completing-read-multiple` function which is installed
  automatically by `selectrum-mode`. This means that commands like
  `describe-face` (which delegate to `completing-read-multiple`
  internally) now use Selectrum by default. To select additional
  candidates within a supported command, use `TAB` and input
  `crm-separator` (`,` by default). See [#53], [#80], [#74].  The user
  option `selectrum-completing-read-multiple-show-help` can be used to
  control display of additional usage information in the prompt
  ([#130], [#132]).
* We provide a `selectrum-completion-in-region` function now and
  install it on `completion-in-region-function` in `selectrum-mode`,
  so `completion-at-point` will use Selectrum when there is more than
  one completion ([#42]). This function can display annotation
  informations if the `completion-at-point-function` backend offers
  them ([#62]), and will respect completion boundaries ([#89]).
  Appearance can be configured using the faces
  `selectrum-completion-annotation`, `selectrum-completion-docsig`,
  and `completions-common-part` ([#86]).
* `selectrum-read` accepts two additional keyword arguments
  `minibuffer-completion-table` and
  `minibuffer-completion-predicate`. These can be used to pass the
  `completing-read` collection and predicate so they are available for
  internal handling of completion API features and for other external
  commands or packages which make use of them ([#94], [#95]).
* If the completion table passed to `completing-read` provides
  `annotation-function` or `display-sort-function` in its metadata,
  Selectrum will use this information to annotate or sort the
  candidates accordingly. Annotations defined by
  `completion-extra-properties` are handled, too ([#82], [#95]).
* One can trigger an update of Selectrum's completions UI manually by
  calling `selectrum-exhibit` ([#95]).
* You can now interact with candidates via the mouse. Left click
  (`mouse-1`) selects the candidate, and right click (`mouse-3`)
  inserts the candidate, just like `RET` and `TAB`, respectively. See
  [#113] and [#118].

### Enhancements
* `icomplete-mode` is now automatically disabled when entering
  Selectrum, to avoid conflicts ([#99]).
* `selectrum-read-file-name` which is used as
  `read-file-name-function` now uses `read-file-name-default`
  internally. This means all default features of file completion
  should be available now. Most notably you can now use `M-n` to
  insert file names into the minibuffer (using
  `file-name-at-point-functions`) and you are able to use shortcuts
  like `//` or `~/` ([#50], [#52]).
* When reading directories and the default is already in the prompt,
  it gets selected so you can immediately submit it ([#126], [#127]).
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
* Previously, `selectrum-read` sometimes modified the list of
  candidates it was given. This has been fixed, and there is a new
  keyword argument `:may-modify-candidates` to re-enable the old
  behavior for cases where it is safe and the performance gains are
  useful. See [#74].

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
  ([#21], [#76]).
* Previously, an error was thrown if you used certain non-Selectrum
  minibuffer commands before loading Selectrum. This has been fixed
  ([#28]).
* If `selectrum-num-candidates-displayed` is set to one, the
  highlighting now works correctly. Before, the prompt would get
  highlighted instead of the current candidate. See [#85].
* `selectrum-read-library-name` previously, in certain versions of
  Emacs, showed some entries with `.el` appended. This has now been
  fixed. Also, `TAB` now inserts the current candidate and not the
  whole path to the library, so that the result can be submitted
  directly ([#73]).
* Empty string completion candidates are now ignored like in the
  default completion UI ([#101]).
* Text properties are now stripped for standard completion functions
  ([#107], [#108]).
* You can now select and submit empty input and for file prompts
  existing paths when require-match is non-nil ([#67], [#125]).

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
[#57]: https://github.com/raxod502/selectrum/pull/57
[#62]: https://github.com/raxod502/selectrum/pull/62
[#67]: https://github.com/raxod502/selectrum/issues/67
[#73]: https://github.com/raxod502/selectrum/pull/73
[#74]: https://github.com/raxod502/selectrum/pull/74
[#76]: https://github.com/raxod502/selectrum/pull/76
[#77]: https://github.com/raxod502/selectrum/pull/77
[#80]: https://github.com/raxod502/selectrum/issues/80
[#82]: https://github.com/raxod502/selectrum/issues/82
[#85]: https://github.com/raxod502/selectrum/pull/85
[#86]: https://github.com/raxod502/selectrum/pull/86
[#89]: https://github.com/raxod502/selectrum/pull/89
[#94]: https://github.com/raxod502/selectrum/issues/94
[#95]: https://github.com/raxod502/selectrum/pull/95
[#96]: https://github.com/raxod502/selectrum/pull/96
[#99]: https://github.com/raxod502/selectrum/issues/99
[#101]: https://github.com/raxod502/selectrum/pull/101
[#107]: https://github.com/raxod502/selectrum/issues/107
[#108]: https://github.com/raxod502/selectrum/pull/108
[#113]: https://github.com/raxod502/selectrum/issues/113
[#118]: https://github.com/raxod502/selectrum/pull/118
[#125]: https://github.com/raxod502/selectrum/pull/125
[#126]: https://github.com/raxod502/selectrum/issues/126
[#127]: https://github.com/raxod502/selectrum/pull/127
[#130]: https://github.com/raxod502/selectrum/issues/130
[#132]: https://github.com/raxod502/selectrum/pull/132
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
