;;; selectrum.el --- Easily select item from list -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 8 Dec 2019
;; Homepage: https://github.com/raxod502/selectrum
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

;;; Commentary:

;; Please see https://github.com/raxod502/selectrum for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; User options

(defgroup selectrum nil
  "Simple incremental narrowing framework with sane API."
  :group 'convenience
  :prefix "selectrum-"
  :link '(url-link "https://github.com/raxod502/selectrum"))

(defcustom selectrum-num-candidates-displayed 10
  "Maximum number of candidates which are displayed at the same time.
The height of the minibuffer will be this number of rows plus one
for the prompt line, assuming no multiline text."
  :type 'number)

(defcustom selectrum-candidate-filter-function #'string-prefix-p
  "Function used to check if candidate should be displayed.
Receives two arguments, the user input and the candidate, both as
strings. Returns non-nil if the candidate matches the input."
  :type 'function)

(defcustom selectrum-candidate-sort-function #'string-lessp
  "Function used to sort candidates.
Receives two arguments, both candidates as strings. Returns
non-nil if the first should sort before the second, like
`string-lessp'."
  :type 'function)

;;;; Minibuffer state

(defvar selectrum--start-of-input-marker nil
  "Marker at the start of the minibuffer user input.
This is used to prevent point from moving into the prompt.")

(defvar selectrum--end-of-input-marker nil
  "Marker at the end of the minibuffer user input.
This is used to prevent point from moving into the candidates.")

(defvar selectrum--sorted-candidates nil
  "List of sorted candidates to be displayed.")

(defvar selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.")

;;;; Hook functions

(defun selectrum--minibuffer-post-command-hook ()
  "Clean things up after a user command in the minibuffer."
  (goto-char (max (point) selectrum--start-of-input-marker))
  (goto-char (min (point) selectrum--end-of-input-marker)))

(defun selectrum--minibuffer-exit-hook ()
  "Clean up Selectrum from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'selectrum--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook 'local))

(defun selectrum--minibuffer-setup-hook (candidates)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of strings that was passed to
`selectrum-read'."
  (add-hook
   'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook nil 'local)
  (add-hook
   'post-command-hook
   #'selectrum--minibuffer-post-command-hook
   nil 'local)
  (save-excursion
    (let ((start (point)))
      (dolist (candidate candidates)
        (insert "\n" candidate))
      (add-text-properties
       start (point) '(read-only t))))
  (setq selectrum--start-of-input-marker (point-marker))
  (setq selectrum--end-of-input-marker (point-marker))
  (set-marker-insertion-type selectrum--end-of-input-marker t))

;;;; Main entry point

(defun selectrum-read (prompt candidates)
  "Prompt user to select one of CANDIDATES, list of strings.
Return the selected string."
  (interactive
   (list "Prompt: " '("apple" "banana" "carrot" "date" "egg" "fig"
                      "guava" "honeyberry" "juniper" "kiwi" "lemon" "mango")))
  (minibuffer-with-setup-hook
      (apply-partially #'selectrum--minibuffer-setup-hook candidates)
    (read-from-minibuffer prompt)))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; selectrum.el ends here
