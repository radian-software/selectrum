# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Enhancements
* In `read-file-name`, when a default is provided (for example in the
  `dired-do-rename` command), we actually use it as the initial
  contents of the minibuffer, which allows you to have convenient
  access to the default filename when that default file does not exist
  ([#25]).
* We now bind `minibuffer-completing-file-name` during
  `read-file-name`, in conformance with the standard Emacs interface
  ([#30]).

## Bugs fixed
* You can now use the undo system in the minibuffer. Previously,
  trying to do so would break Selectrum ([#31]).
* Passing a list of symbols to `selectrum-completing-read` works now.
* Previously, `selectrum-read-buffer` ignored its PREDICATE argument.
  This has now been fixed ([#32, #33]).
* Previously, `selectrum-read` would return nil when
  `selectrum-submit-exact-input` was used on an empty input and no
  `:default-candidate` was provided. Now the empty string is returned,
  in accordance with the `completing-read` API ([#34]).

[#25]: https://github.com/raxod502/selectrum/pull/25
[#30]: https://github.com/raxod502/selectrum/issues/30
[#31]: https://github.com/raxod502/selectrum/issues/31
[#32]: https://github.com/raxod502/selectrum/issues/32
[#33]: https://github.com/raxod502/selectrum/issues/33
[#34]: https://github.com/raxod502/selectrum/issues/34

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
