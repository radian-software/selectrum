# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Changed
* `selectrum-read` now also accepts a function as CANDIDATES.  This
   can be used to generate them dynamically and to adjust user input
   used for matching/highlighting ([#27]).
* The API of `selectrum-refine-candidates-function` changed to always
  return a list of strings and is no longer able to adjust user input.

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
