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

;;;; Libraries

(require 'map)
(require 'seq)

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

(defcustom selectrum-minibuffer-bindings
  '(("<up>" . selectrum-previous-candidate)
    ("<down>" . selectrum-next-candidate))
  "Keybindings enabled in minibuffer. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
entering the minibuffer. The keys are strings and the values are
command symbols."
  :type '(alist
          :key-type string
          :value-type function))

;;;;; Faces

(defface selectrum-current-candidate
  '((t :inherit highlight))
  "Face used to highlight the currently selected candidate.")

;;;; Utility functions

(defun selectrum--clamp (x lower upper)
  "Constrain X to be between LOWER and UPPER inclusive.
If X < LOWER, return LOWER. If X > UPPER, return UPPER. Else
return X."
  (min (max x lower) upper))

;;;; Minibuffer state

(defvar selectrum--start-of-input-marker nil
  "Marker at the start of the minibuffer user input.
This is used to prevent point from moving into the prompt.")

(defvar selectrum--end-of-input-marker nil
  "Marker at the end of the minibuffer user input.
This is used to prevent point from moving into the candidates.")

(defvar selectrum--sorted-candidates nil
  "List of sorted candidates to be displayed.
This only needs to be computed once per use of Selectrum.")

(defvar selectrum--filtered-candidates nil
  "List of sorted *and* filtered candidates to be displayed.
This only needs to be computed every time the user input changes.")

(defvar selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.")

(defvar selectrum--previous-input-string nil
  "Previous user input string in the minibuffer.
Used to check if the user input has changed and candidates need
to be re-filtered.")

;;;; Hook functions

(defun selectrum--minibuffer-post-command-hook ()
  "Update minibuffer in response to user input."
  (goto-char (max (point) selectrum--start-of-input-marker))
  (goto-char (min (point) selectrum--end-of-input-marker))
  (save-excursion
    (let ((inhibit-read-only t)
          (input (buffer-substring selectrum--start-of-input-marker
                                   selectrum--end-of-input-marker))
          (bound (marker-position selectrum--end-of-input-marker)))
      (unless (equal input selectrum--previous-input-string)
        (setq selectrum--previous-input-string input)
        (setq selectrum--filtered-candidates
              ;; This could be made faster although significantly more
              ;; complicated by only doing the filtering as needed to
              ;; show the candidates being displayed.
              (seq-filter
               (apply-partially
                selectrum-candidate-filter-function
                input)
               selectrum--sorted-candidates))
        (setq selectrum--current-candidate-index
              (and (> (length selectrum--filtered-candidates) 0)
                   0)))
      (let ((first-index-displayed
             (and (> (length selectrum--filtered-candidates) 0)
                  (selectrum--clamp
                   (- selectrum--current-candidate-index
                      (/ selectrum-num-candidates-displayed 2))
                   0
                   (- (length selectrum--filtered-candidates)
                      selectrum-num-candidates-displayed)))))
        (delete-region bound (point-max))
        (dolist (candidate (seq-take
                            (nthcdr
                             first-index-displayed
                             selectrum--filtered-candidates)
                            selectrum-num-candidates-displayed))
          (insert "\n" candidate))
        (add-text-properties bound (point-max) '(read-only t))
        (setq selectrum--end-of-input-marker (set-marker (make-marker) bound))
        (set-marker-insertion-type selectrum--end-of-input-marker t)))))

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
  (setq selectrum--start-of-input-marker (point-marker))
  (setq selectrum--end-of-input-marker (point-marker))
  (set-marker-insertion-type selectrum--end-of-input-marker t)
  (setq selectrum--sorted-candidates
        (seq-sort selectrum-candidate-sort-function candidates))
  ;; Make sure to trigger an "user input changed" event, so that
  ;; filtering happens and an index is assigned.
  (setq selectrum--previous-input-string nil)
  (add-hook
   'post-command-hook
   #'selectrum--minibuffer-post-command-hook
   nil 'local))

;;;; Minibuffer commands

(defun selectrum-previous-candidate ()
  "Move selection to previous candidate, unless at beginning already."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (max 0 (1- selectrum--current-candidate-index)))))

(defun selectrum-next-candidate ()
  "Move selection to next candidate, unless at end already."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (min (1- (length selectrum--filtered-candidates))
               (1+ selectrum--current-candidate-index)))))

;;;; Main entry point

(defun selectrum-read (prompt candidates)
  "Prompt user to select one of CANDIDATES, list of strings.
Return the selected string."
  (interactive
   (list "Prompt: " '("apple" "banana" "carrot" "date" "egg" "fig"
                      "guava" "honeyberry" "juniper" "kiwi" "lemon" "mango")))
  (let ((keymap (make-sparse-keymap)))
    (map-do
     (lambda (key cmd)
       (define-key keymap (kbd key) cmd))
     selectrum-minibuffer-bindings)
    (minibuffer-with-setup-hook
        (apply-partially #'selectrum--minibuffer-setup-hook candidates)
      (read-from-minibuffer prompt nil keymap nil t))))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; selectrum.el ends here
