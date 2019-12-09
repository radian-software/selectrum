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

(defun selectrum-default-candidate-filter-function (input candidates)
  "Default value of `selectrum-candidate-filter-function'.
Return only candidates that contain the input as a substring."
  (let ((regexp (regexp-quote input)))
    (seq-filter
     (lambda (candidate)
       (string-match-p regexp candidate))
     candidates)))

(defcustom selectrum-candidate-filter-function
  #'selectrum-default-candidate-filter-function
  "Function used to check which candidates should be displayed.
Receive two arguments, the user input (a string) and the list of
candidates (strings). Return a filtered list of candidates. Do
not modify the input list."
  :type 'function)

(defun selectrum-default-candidate-sort-function (candidates)
  "Default value of `selectrum-candidate-sort-function'.
Sort first by length and then alphabetically."
  (sort candidates
        (lambda (c1 c2)
          (or (< (length c1)
                 (length c2))
              (and (= (length c1)
                      (length c2))
                   (string-lessp c1 c2))))))

(defcustom selectrum-candidate-sort-function
  #'selectrum-default-candidate-sort-function
  "Function used to sort candidates.
Receive one argument, the list of candidates. Return a sorted
list. May modify the input list."
  :type 'function)

(defcustom selectrum-minibuffer-bindings
  '(([remap previous-line]       . selectrum-previous-candidate)
    ([remap next-line]           . selectrum-next-candidate)
    ([remap newline]             . selectrum-select-current-candidate)
    ([remap scroll-down-command] . selectrum-previous-page)
    ([remap scroll-up-command]   . selectrum-next-page)
    ([remap beginning-of-buffer] . selectrum-goto-beginning)
    ([remap end-of-buffer]       . selectrum-goto-end))
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

(defun selectrum--normalize-collection (collection &optional predicate)
  "Normalize COLLECTION into a list of strings.
COLLECTION may be a list of strings or cons cells, an obarray, a
hash table, or a function, as per the docstring of
`try-completion'.

If PREDICATE is non-nil, then it filters the collection as in
`try-completion'."
  (cond
   ((listp collection)
    (when predicate
      (setq collection (seq-filter predicate collection)))
    (seq-map (lambda (elt)
               (or (car-safe elt) elt))
             collection))
   ((hash-table-p collection)
    (let ((lst nil))
      (maphash
       (lambda (key val)
         (when (and (or (symbolp key)
                        (stringp key))
                    (or (null predicate)
                        (funcall predicate key val)))
           (push key lst))))))
   ((obarrayp collection)
    (let ((lst nil))
      (mapatoms
       (lambda (elt)
         (when (or (null predicate)
                   (funcall predicate elt))
           (push (symbol-name elt) lst))))
      lst))
   ((functionp collection)
    (funcall collection "" predicate t))
   (t
    (error "Unsupported collection type %S" (type-of collection)))))

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
              ;; show the candidates being displayed. The API would
              ;; also be more complex.
              (funcall selectrum-candidate-filter-function
                       input selectrum--sorted-candidates))
        (setq selectrum--current-candidate-index
              (and (> (length selectrum--filtered-candidates) 0)
                   0)))
      (let ((first-index-displayed
             (if selectrum--current-candidate-index
                 (selectrum--clamp
                  ;; Adding one here makes it look slightly better, as
                  ;; there are guaranteed to be more candidates shown
                  ;; below the selection than above.
                  (1+ (- selectrum--current-candidate-index
                         (/ selectrum-num-candidates-displayed 2)))
                  0
                  (max (- (length selectrum--filtered-candidates)
                          selectrum-num-candidates-displayed)
                       0))
               0)))
        (delete-region bound (point-max))
        (let ((highlighted-index (and selectrum--current-candidate-index
                                      (- selectrum--current-candidate-index
                                         first-index-displayed))))
          (seq-map-indexed
           (lambda (candidate index)
             (when (equal index highlighted-index)
               (setq candidate (propertize
                                candidate 'face 'selectrum-current-candidate)))
             (insert "\n" candidate))
           (seq-take
            (nthcdr
             first-index-displayed
             selectrum--filtered-candidates)
            selectrum-num-candidates-displayed)))
        (add-text-properties bound (point-max) '(read-only t))
        (setq selectrum--end-of-input-marker (set-marker (make-marker) bound))
        (set-marker-insertion-type selectrum--end-of-input-marker t)))))

(defun selectrum--minibuffer-exit-hook ()
  "Clean up Selectrum from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'selectrum--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook 'local))

(cl-defun selectrum--minibuffer-setup-hook (candidates &key default-candidate)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of strings that was passed to
`selectrum-read'."
  (add-hook
   'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook nil 'local)
  (setq selectrum--start-of-input-marker (point-marker))
  (setq selectrum--end-of-input-marker (point-marker))
  (set-marker-insertion-type selectrum--end-of-input-marker t)
  (setq selectrum--sorted-candidates
        (funcall selectrum-candidate-sort-function candidates))
  ;; If default is provided, sort it at the beginning instead of doing
  ;; Ivy's weird thing where the default selection isn't the first
  ;; element.
  (when default-candidate
    (setq selectrum--sorted-candidates
          (cons default-candidate
                (delete default-candidate selectrum--sorted-candidates))))
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

(defun selectrum-previous-page ()
  "Move selection upwards by one page, unless at beginning already."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (max 0 (- selectrum--current-candidate-index
                    selectrum-num-candidates-displayed)))))

(defun selectrum-next-page ()
  "Move selection downwards by one page, unless at end already."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (min (1- (length selectrum--filtered-candidates))
               (+ selectrum--current-candidate-index
                  selectrum-num-candidates-displayed)))))

(defun selectrum-goto-beginning ()
  "Move selection to first candidate."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index 0)))

(defun selectrum-goto-end ()
  "Move selection to last candidate."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (1- (length selectrum--filtered-candidates)))))

(defun selectrum-select-current-candidate ()
  "Exit minibuffer, picking the currently selected candidate.
If there are no candidates, return the current user input."
  (interactive)
  (let ((value (if selectrum--current-candidate-index
                   (nth selectrum--current-candidate-index
                        selectrum--filtered-candidates)
                 (buffer-substring
                  selectrum--start-of-input-marker
                  selectrum--end-of-input-marker))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert value))
    (exit-minibuffer)))

;;;; Main entry point

(cl-defun selectrum-read (prompt candidates &key default-candidate)
  "Prompt user to select one of CANDIDATES, list of strings.
Return the selected string."
  (interactive
   (list "Prompt: " '("apple" "banana" "carrot" "date" "egg" "fig"
                      "guava" "honeyberry" "juniper" "kiwi" "lemon" "mango"
                      "apricot" "blackberry" "avocado" "blueberry"
                      "breadfruit" "cantaloupe" "clementine" "cherry"
                      "durian")))
  (let ((keymap (make-sparse-keymap)))
    (map-do
     (lambda (key cmd)
       (when (stringp key)
         (setq key (kbd key)))
       (define-key keymap key cmd))
     selectrum-minibuffer-bindings)
    (minibuffer-with-setup-hook
        (lambda ()
          (selectrum--minibuffer-setup-hook
           candidates
           :default-candidate default-candidate))
      (read-from-minibuffer prompt nil keymap nil t))))

(defun selectrum-completing-read
    (prompt collection &optional
            predicate require-match initial-input
            hist def inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'."
  (selectrum-read
   prompt (selectrum--normalize-collection collection predicate)
   :default-candidate (or (car-safe def) def)))

(defvar selectrum--old-completing-read-function nil
  "Previous value of `completing-read-function'.")

;;;###autoload
(define-minor-mode selectrum-mode
  "Minor mode to use Selectrum for `completing-read'."
  :global t
  (if selectrum-mode
      (progn
        (setq selectrum--old-completing-read-function
              (default-value 'completing-read-function))
        (setq-default completing-read-function
                      #'selectrum-completing-read))
    (when (equal (default-value 'completing-read-function)
                 #'selectrum-completing-read)
      (setq-default completing-read-function
                    selectrum--old-completing-read-function))))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; selectrum.el ends here
