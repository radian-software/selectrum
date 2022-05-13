;; This file has code that is evaluated in CI before the indentation
;; of selectrum.el is checked. This is helpful because it allows us to
;; ensure that various things are indented correctly if they require
;; some setup for Emacs to know how to do the right thing.

;; The indentation of `define-key' has for some reason changed in
;; Emacs 29 when it was deprecated in favor of `keymap-set'. Maybe
;; that is a bug and they will change it, but for now, force the
;; indentation to the backwards-compatible version.
(put #'define-key 'lisp-indent-function 'defun)
