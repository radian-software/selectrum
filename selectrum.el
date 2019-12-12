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

(require 'cl-lib)
(require 'map)
(require 'seq)

;;;; Faces

(defface selectrum-current-candidate
  '((t :inherit highlight))
  "Face used to highlight the currently selected candidate."
  :group 'selectrum-faces)

(defface selectrum-primary-highlight
  '((t :weight bold))
  "Face used to highlight the parts of candidates that match the input."
  :group 'selectrum-faces)

(defface selectrum-secondary-highlight
  '((t :inherit selectrum-primary-highlight :underline t))
  "Additional face used to highlight parts of candidates.
May be used to highlight parts of candidates that match specific
parts of the input."
  :group 'selectrum-faces)

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
Return only candidates that contain the input as a substring.
INPUT is a string, CANDIDATES is a list of strings."
  (let ((regexp (regexp-quote input)))
    (cl-remove-if-not
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
Sort first by length and then alphabetically. CANDIDATES is a
list of strings."
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

(defun selectrum-default-candidate-highlight-function (input candidates)
  "Default value of `selectrum-candidate-highlight-function'.
Highlight the substring match with
`selectrum-primary-highlight'. INPUT is a string, CANDIDATES is a
list of strings."
  (let ((regexp (regexp-quote input)))
    (save-match-data
      (mapcar
       (lambda (candidate)
         (when (string-match regexp candidate)
           (setq candidate (copy-sequence candidate))
           (put-text-property
            (match-beginning 0) (match-end 0)
            'face 'selectrum-primary-highlight
            candidate))
         candidate)
       candidates))))

(defcustom selectrum-candidate-highlight-function
  #'selectrum-default-candidate-highlight-function
  "Function used to highlight matched candidates.
Receive two arguments, the input string and the list of
candidates (strings) that are going to be displayed (length at
most `selectrum-num-candidates-displayed'). Return a list of
propertized candidates. Do not modify the input list or
strings."
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

(defcustom selectrum-candidate-selected-hook nil
  "Normal hook run when the user selects a candidate.
It gets the same arguments as `selectrum-read' got, prepended
with the string the user selected."
  :type 'hook)

;;;; Utility functions

(defun selectrum--clamp (x lower upper)
  "Constrain X to be between LOWER and UPPER inclusive.
If X < LOWER, return LOWER. If X > UPPER, return UPPER. Else
return X."
  (min (max x lower) upper))

(defun selectrum--map-destructive (func lst)
  "Apply FUNC to each element of LST, returning the new list.
Modify the original list destructively, instead of allocating a
new one."
  (prog1 lst
    (while lst
      (setcar lst (funcall func (car lst)))
      (setq lst (cdr lst)))))

(defun selectrum--normalize-collection (collection &optional predicate)
  "Normalize COLLECTION into a list of strings.
COLLECTION may be a list of strings or cons cells, an obarray, a
hash table, or a function, as per the docstring of
`try-completion'. The returned list may be mutated without
damaging the original COLLECTION.

If PREDICATE is non-nil, then it filters the collection as in
`try-completion'."
  (cond
   ((listp collection)
    (if predicate
        (setq collection (cl-remove-if-not predicate collection))
      (setq collection (copy-sequence collection)))
    (selectrum--map-destructive
     (lambda (elt)
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
           (push key lst)))
       collection)))
   ;; Use `vectorp' instead of `obarrayp' because the latter isn't
   ;; defined in Emacs 25.
   ((vectorp collection)
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

(defvar selectrum--current-candidate nil
  "Value of currently selected cadndiate, or nil if no candidates.")

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
               0))
            ;; Hack in some behavior where if the input matches a
            ;; candidate exactly then we pretend like it was sorted
            ;; first, without actually building a new list (which
            ;; would be super slow).
            (index-with-exact-match
             (cl-position input selectrum--filtered-candidates :test #'equal)))
        (delete-region bound (point-max))
        (goto-char (point-max))
        ;; If we've got an exact match, we want to emulate moving that
        ;; match to the beginning of the list. First observe that if
        ;; the exact match is before our display window, it doesn't
        ;; affect anything. So we restrict our attention to the case
        ;; where the exact match is in or after our display window. In
        ;; that case we have two more cases: either we're displaying
        ;; the exact match (i.e. we display index 0) or we don't. In
        ;; the case of displaying it, we'll push it at the beginning
        ;; of our list later. In the case of not displaying it, our
        ;; candidates are pushed downwards by one, so we should adjust
        ;; the index by one.
        (when (and index-with-exact-match
                   (>= index-with-exact-match first-index-displayed)
                   (> first-index-displayed 0))
          (cl-decf first-index-displayed))
        (let* ((highlighted-index (and selectrum--current-candidate-index
                                       (- selectrum--current-candidate-index
                                          first-index-displayed)))
               (displayed-candidates
                (seq-take
                 (nthcdr
                  first-index-displayed
                  selectrum--filtered-candidates)
                 ;; Grab one more than needed so that if an exact
                 ;; match with the user input appears, we can remove
                 ;; it to put at the beginning, and still have enough
                 ;; candidates.
                 (1+ selectrum-num-candidates-displayed))))
          ;; See discussion above.
          (when (and index-with-exact-match
                     (>= index-with-exact-match first-index-displayed)
                     (= first-index-displayed 0))
            (setq displayed-candidates (delete input displayed-candidates))
            ;; The use of `nth' could be replaced for performance. We
            ;; don't want to just push the input directly as that
            ;; would lose text properties.
            (push (nth index-with-exact-match selectrum--filtered-candidates)
                  displayed-candidates))
          (setq displayed-candidates
                (seq-take displayed-candidates
                          selectrum-num-candidates-displayed))
          (let ((index 0))
            ;; Reset this in case there are no candidates so it
            ;; doesn't get set to any candidate below.
            (setq selectrum--current-candidate nil)
            (dolist (candidate (funcall
                                selectrum-candidate-highlight-function
                                input
                                displayed-candidates))
              (when (equal index highlighted-index)
                (setq selectrum--current-candidate candidate)
                (setq candidate (propertize
                                 candidate
                                 'face 'selectrum-current-candidate)))
              (insert "\n" candidate)
              (cl-incf index))))
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
`selectrum-read'. DEFAULT-CANDIDATE, if provided, is added to the
list and sorted first."
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
  (let ((value (or selectrum--current-candidate
                   (buffer-substring
                    selectrum--start-of-input-marker
                    selectrum--end-of-input-marker))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert value))
    (exit-minibuffer)))

;;;; Main entry point

(cl-defun selectrum-read (prompt candidates &rest args &key default-candidate)
  "Prompt user with PROMPT to select one of CANDIDATES, list of strings.
Return the selected string. PROMPT should generally end in a
colon and space. Additional keyword ARGS are accepted.
DEFAULT-CANDIDATE, if provided, is added to the list and
presented at the top."
  (let ((keymap (make-sparse-keymap)))
    ;; Use `map-apply' instead of `map-do' as the latter is not
    ;; available in Emacs 25.
    (map-apply
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
      (let ((selected (read-from-minibuffer prompt nil keymap nil t)))
        (prog1 selected
          (apply
           #'run-hook-with-args
           'selectrum-candidate-selected-hook
           selected prompt candidates args))))))

(defun selectrum-completing-read
    (prompt collection &optional
            predicate _require-match _initial-input
            _hist def _inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, and DEF, see `completing-read'."
  (selectrum-read
   prompt (selectrum--normalize-collection collection predicate)
   :default-candidate (or (car-safe def) def)))

(defvar selectrum--old-completing-read-function nil
  "Previous value of `completing-read-function'.")

(defun selectrum-read-buffer (prompt &optional def require-match predicate)
  "Read buffer using Selectrum. Can be used as `read-buffer-function'.
Actually, as long as `selectrum-completing-read' is installed in
`completing-read-function', `read-buffer' already uses Selectrum.
Installing this function in `read-buffer-function' makes sure the
buffers are sorted in the default order (most to least recently
used) rather than in whatever order is defined by
`selectrum-candidate-sort-function', which is likely to be less
appropriate. For PROMPT, DEF, REQUIRE-MATCH, and PREDICATE, see
`read-buffer'."
  (let ((selectrum-candidate-sort-function #'identity)
        (read-buffer-function nil))
    (read-buffer prompt def require-match predicate)))

(defvar selectrum--old-read-buffer-function nil
  "Previous value of `read-buffer-function'.")

;;;###autoload
(define-minor-mode selectrum-mode
  "Minor mode to use Selectrum for `completing-read'."
  :global t
  (if selectrum-mode
      (progn
        (setq selectrum--old-completing-read-function
              (default-value 'completing-read-function))
        (setq-default completing-read-function
                      #'selectrum-completing-read)
        (setq selectrum--old-read-buffer-function
              (default-value 'read-buffer-function))
        (setq-default read-buffer-function
                      #'selectrum-read-buffer))
    (when (equal (default-value 'completing-read-function)
                 #'selectrum-completing-read)
      (setq-default completing-read-function
                    selectrum--old-completing-read-function))
    (when (equal (default-value 'read-buffer-function)
                 #'selectrum-read-buffer)
      (setq-default read-buffer-function
                    selectrum--old-read-buffer-function))))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; selectrum.el ends here
