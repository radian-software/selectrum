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
(require 'regexp-opt)
(require 'seq)
(require 'subr-x)

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

;;;; Variables

(defvar selectrum-should-sort-p t
  "Non-nil if preprocessing and refinement functions should sort.
This is let-bound to nil in some contexts, and should be
respected by user functions for optimal results.")

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

(defun selectrum-default-candidate-refine-function (input candidates)
  "Default value of `selectrum-refine-candidates-function'.
Return only candidates that contain the input as a substring.
INPUT is a string, CANDIDATES is a list of strings."
  (let ((regexp (regexp-quote input)))
    (cl-delete-if-not
     (lambda (candidate)
       (string-match-p regexp candidate))
     (copy-sequence candidates))))

(defcustom selectrum-refine-candidates-function
  #'selectrum-default-candidate-refine-function
  "Function used to decide which candidates should be displayed.
Receives two arguments, the user input (a string) and the list of
candidates (strings) as returned by
`selectrum-preprocess-candidates-function'. Returns a new list of
candidates. Should not modify the input list. The returned list
may be modified by Selectrum, so a copy of the input should be
made. (Beware that `cl-remove-if' doesn't make a copy if there's
nothing to remove.)

Instead of a list of strings, may alternatively return an alist
with the following keys:
- `candidates': list of strings, as above
- `input' (optional): transformed user input, used for
  highlighting"
  :type 'function)

(defun selectrum-default-candidate-preprocess-function (candidates)
  "Default value of `selectrum-preprocess-candidates-function'.
Sort first by length and then alphabetically. CANDIDATES is a
list of strings."
  (if selectrum-should-sort-p
      (sort candidates
            (lambda (c1 c2)
              (or (< (length c1)
                     (length c2))
                  (and (= (length c1)
                          (length c2))
                       (string-lessp c1 c2)))))
    candidates))

(defcustom selectrum-preprocess-candidates-function
  #'selectrum-default-candidate-preprocess-function
  "Function used to preprocess the list of candidates.
Receive one argument, the list of candidates. Return a new list.
May modify the input list. The returned list may be modified by
Selectrum. Note that if you sort a list of candidates, you should
use a stable sort. That way, candidates which differ only in text
properties will retain their ordering, which may be significant
\(e.g. for `load-path' shadows in `read-library-name')."
  :type 'function)

(defun selectrum-default-candidate-highlight-function (input candidates)
  "Default value of `selectrum-highlight-candidates-function'.
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

(defcustom selectrum-highlight-candidates-function
  #'selectrum-default-candidate-highlight-function
  "Function used to highlight matched candidates.
Receive two arguments, the input string and the list of
candidates (strings) that are going to be displayed (length at
most `selectrum-num-candidates-displayed'). Return a list of
propertized candidates. Do not modify the input list or
strings."
  :type 'function)

(defcustom selectrum-minibuffer-bindings
  '(([remap keyboard-quit]       . abort-recursive-edit)
    ([remap previous-line]       . selectrum-previous-candidate)
    ([remap next-line]           . selectrum-next-candidate)
    ([remap newline]             . selectrum-select-current-candidate)
    ([remap scroll-down-command] . selectrum-previous-page)
    ([remap scroll-up-command]   . selectrum-next-page)
    ([remap beginning-of-buffer] . selectrum-goto-beginning)
    ([remap end-of-buffer]       . selectrum-goto-end)
    ([remap kill-ring-save]      . selectrum-kill-ring-save)
    ("C-j"                       . selectrum-submit-exact-input)
    ("TAB"                       . selectrum-insert-current-candidate))
  "Keybindings enabled in minibuffer. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
entering the minibuffer. The keys are strings or raw key events
and the values are command symbols."
  :type '(alist
          :key-type sexp
          :value-type function))

(defcustom selectrum-candidate-selected-hook nil
  "Normal hook run when the user selects a candidate.
It gets the same arguments as `selectrum-read' got, prepended
with the string the user selected."
  :type 'hook)

(defcustom selectrum-candidate-inserted-hook nil
  "Normal hook run when the user inserts a candidate.
\(This happens by typing \\[selectrum-insert-current-candidate].)
It gets the same arguments as `selectrum-read' got, prepended
with the string the user inserted."
  :type 'hook)

(defcustom selectrum-count-style 'matches
  "The style to use for displaying count information before the prompt.

Possible values are:

- \\='matches: Show the total number of matches.
- \\='current/matches: Show the index of current match and the total number of
  matches.
- nil: Show nothing."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Count matches" 'matches)
          (const :tag "Count matches and show current match"
                 'current/matches)))

;;;; Utility functions

;;;###autoload
(progn
  (defmacro selectrum--when-compile (cond &rest body)
    "Like `when', but COND is evaluated at compile time.
If it's nil, BODY is not even compiled."
    (declare (indent 1))
    (when (eval cond)
      `(progn ,@body))))

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

(defun selectrum--move-to-front-destructive (elt lst)
  "Move all instances of ELT to front of LST, if present.
Make comparisons using `equal'. Modify the input list
destructively and return the modified list."
  (let* ((elts nil)
         ;; All problems in computer science are solved by an
         ;; additional layer of indirection.
         (lst (cons (make-symbol "dummy") lst))
         (link lst))
    (while (cdr link)
      (if (equal elt (cadr link))
          (progn
            (push (cadr link) elts)
            (setcdr link (cddr link)))
        (setq link (cdr link))))
    (nconc (nreverse elts) (cdr lst))))

(defun selectrum--normalize-collection (collection &optional predicate)
  "Normalize COLLECTION into a list of strings.
COLLECTION may be a list of strings or cons cells, an obarray, a
hash table, or a function, as per the docstring of
`try-completion'. The returned list may be mutated without
damaging the original COLLECTION.

If PREDICATE is non-nil, then it filters the collection as in
`try-completion'."
  (cond
   ;; Check for `functionp' first, because anonymous functions can be
   ;; mistaken for lists.
   ((functionp collection)
    (funcall collection "" predicate t))
   ((listp collection)
    (setq collection (copy-sequence collection))
    (when predicate
      (setq collection (cl-delete-if-not predicate collection)))
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
   (t
    (error "Unsupported collection type %S" (type-of collection)))))

;;;; Minibuffer state

(defvar selectrum--start-of-input-marker nil
  "Marker at the start of the minibuffer user input.
This is used to prevent point from moving into the prompt.")

(defvar selectrum--end-of-input-marker nil
  "Marker at the end of the minibuffer user input.
This is used to prevent point from moving into the candidates.")

(defvar selectrum--preprocessed-candidates nil
  "Preprocessed list of candidates.
This is derived from the collection passed to `selectrum-read'
just once, and is subsequently passed to
`selectrum-preprocess-candidates-function' every time the user
input changes in order to generate
`selectrum--refined-candidates'.")

(defvar selectrum--refined-candidates nil
  "Refined list of candidates to be displayed.
This is derived from `selectrum--preprocessed-candidates' by
`selectrum-refine-candidates-function' every time the user input
changes, and is subsequently passed to
`selectrum-highlight-candidates-function'.")

(defvar selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.")

(defvar selectrum--previous-input-string nil
  "Previous user input string in the minibuffer.
Used to check if the user input has changed and candidates need
to be re-filtered.")

(defvar selectrum--match-required-p nil
  "Non-nil if the user must select one of the candidates.
Equivalently, nil if the user is allowed to submit their own
input that does not match any of the displayed candidates.")

(defvar selectrum--default-candidate nil
  "Default candidate, or nil if none given.")

;; The existence of this variable is a bit of a mess, but we'll run
;; with it for now.
(defvar selectrum--visual-input nil
  "User input string as transformed by candidate refinement.
See `selectrum-refine-candidates-function'.")

(defvar selectrum--read-args nil
  "List of arguments passed to `selectrum-read'.
Passed to various hook functions.")

(defvar selectrum--count-overlay nil
  "Overlay used to display count information before prompt.")

;;;; Hook functions

(defun selectrum--count-info ()
  "Return a string of count information to be prepended to prompt."
  (let ((total (length selectrum--refined-candidates))
        (current (1+ (or selectrum--current-candidate-index -1))))
    (pcase selectrum-count-style
      ('matches         (format "%-4d " total))
      ('current/matches (format "%-6s " (format "%d/%d" current total)))
      (_                ""))))

(defun selectrum--minibuffer-post-command-hook ()
  "Update minibuffer in response to user input."
  (goto-char (max (point) selectrum--start-of-input-marker))
  (goto-char (min (point) selectrum--end-of-input-marker))
  (save-excursion
    (let ((inhibit-read-only t)
          (input (buffer-substring selectrum--start-of-input-marker
                                   selectrum--end-of-input-marker))
          (bound (marker-position selectrum--end-of-input-marker))
          (keep-mark-active (not deactivate-mark)))
      (unless (equal input selectrum--previous-input-string)
        (setq selectrum--previous-input-string input)
        ;; Reset the persistent input, so that it will be nil if
        ;; there's no special attention needed.
        (setq selectrum--visual-input nil)
        (let ((result (funcall selectrum-refine-candidates-function
                               input selectrum--preprocessed-candidates)))
          (if (stringp (car result))
              (setq selectrum--refined-candidates result)
            (setq selectrum--refined-candidates
                  (alist-get 'candidates result))
            (setq input (or (alist-get 'input result) input))
            (setq selectrum--visual-input input)))
        (setq selectrum--refined-candidates
              (selectrum--move-to-front-destructive
               selectrum--default-candidate
               selectrum--refined-candidates))
        (setq selectrum--refined-candidates
              (selectrum--move-to-front-destructive
               input selectrum--refined-candidates))
        (setq selectrum--current-candidate-index
              (and (> (length selectrum--refined-candidates) 0)
                   0)))
      (overlay-put selectrum--count-overlay
                   'before-string (selectrum--count-info))
      (setq input (or selectrum--visual-input input))
      (let ((first-index-displayed
             (if selectrum--current-candidate-index
                 (selectrum--clamp
                  ;; Adding one here makes it look slightly better, as
                  ;; there are guaranteed to be more candidates shown
                  ;; below the selection than above.
                  (1+ (- selectrum--current-candidate-index
                         (/ selectrum-num-candidates-displayed 2)))
                  0
                  (max (- (length selectrum--refined-candidates)
                          selectrum-num-candidates-displayed)
                       0))
               0)))
        (delete-region bound (point-max))
        (goto-char (point-max))
        (let* ((highlighted-index (and selectrum--current-candidate-index
                                       (- selectrum--current-candidate-index
                                          first-index-displayed)))
               (displayed-candidates
                (seq-take
                 (nthcdr
                  first-index-displayed
                  selectrum--refined-candidates)
                 selectrum-num-candidates-displayed)))
          (setq displayed-candidates
                (seq-take displayed-candidates
                          selectrum-num-candidates-displayed))
          (let ((index 0))
            (dolist (candidate (mapcar
                                (lambda (candidate)
                                  (concat
                                   (get-text-property
                                    0 'selectrum-candidate-display-prefix
                                    candidate)
                                   candidate
                                   (get-text-property
                                    0 'selectrum-candidate-display-suffix
                                    candidate)))
                                (funcall
                                 selectrum-highlight-candidates-function
                                 input
                                 displayed-candidates)))
              (when (equal index highlighted-index)
                (setq candidate (propertize
                                 candidate
                                 'face 'selectrum-current-candidate)))
              (insert "\n" candidate)
              (cl-incf index))))
        (add-text-properties bound (point-max) '(read-only t))
        (setq selectrum--end-of-input-marker (set-marker (make-marker) bound))
        (set-marker-insertion-type selectrum--end-of-input-marker t)
        (selectrum--fix-set-minibuffer-message))
      (when keep-mark-active
        (setq deactivate-mark nil)))))

(defun selectrum--minibuffer-exit-hook ()
  "Clean up Selectrum from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'selectrum--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook 'local)
  (when (overlayp selectrum--count-overlay)
    (delete-overlay selectrum--count-overlay))
  (setq selectrum--count-overlay nil))

(cl-defun selectrum--minibuffer-setup-hook
    (candidates &key default-candidate initial-input require-match)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of strings that was passed to
`selectrum-read'. DEFAULT-CANDIDATE, if provided, is added to the
list and sorted first. INITIAL-INPUT, if provided, is inserted
into the user input area to start with. REQUIRE-MATCH, if
non-nil, means the user has to select one of the candidates
provided, rather than providing one of their own."
  (add-hook
   'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook nil 'local)
  (setq selectrum--match-required-p require-match)
  (setq selectrum--start-of-input-marker (point-marker))
  (when initial-input
    (insert initial-input))
  (setq selectrum--end-of-input-marker (point-marker))
  (set-marker-insertion-type selectrum--end-of-input-marker t)
  (setq selectrum--preprocessed-candidates
        (funcall selectrum-preprocess-candidates-function candidates))
  (setq selectrum--default-candidate default-candidate)
  ;; Make sure to trigger an "user input changed" event, so that
  ;; candidate refinement happens in `post-command-hook' and an index
  ;; is assigned.
  (setq selectrum--previous-input-string nil)
  (setq selectrum--count-overlay (make-overlay (point-min) (point-min)))
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
          (min (1- (length selectrum--refined-candidates))
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
          (min (1- (length selectrum--refined-candidates))
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
          (1- (length selectrum--refined-candidates)))))

(defun selectrum-kill-ring-save ()
  "Save current candidate to kill ring.
Or if there is an active region, save the region to kill ring."
  (interactive)
  (if (or (use-region-p) (not transient-mark-mode))
      (call-interactively #'kill-ring-save)
    (let ((candidate (nth selectrum--current-candidate-index
                          selectrum--refined-candidates)))
      (when selectrum--current-candidate-index
        (kill-new (or (get-text-property
                       0 'selectrum-candidate-full candidate)
                      candidate))))))

(defun selectrum-select-current-candidate ()
  "Exit minibuffer, picking the currently selected candidate.
If there are no candidates, return the current user input, unless
a match is required, in which case do nothing."
  (interactive)
  (when (or selectrum--current-candidate-index
            (not selectrum--match-required-p))
    (let ((value (if selectrum--current-candidate-index
                     (nth selectrum--current-candidate-index
                          selectrum--refined-candidates)
                   (buffer-substring
                    selectrum--start-of-input-marker
                    selectrum--end-of-input-marker))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert value))
      (exit-minibuffer))))

(defun selectrum-submit-exact-input ()
  "Exit minibuffer, using the current user input.
This differs from `selectrum-select-current-candidate' in that it
ignores the currently selected candidate, if one exists."
  (interactive)
  (unless selectrum--match-required-p
    (let ((value (buffer-substring
                  selectrum--start-of-input-marker
                  selectrum--end-of-input-marker)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert value))
      (exit-minibuffer))))

(defun selectrum-insert-current-candidate ()
  "Insert current candidate into user input area."
  (interactive)
  (when selectrum--current-candidate-index
    (delete-region selectrum--start-of-input-marker
                   selectrum--end-of-input-marker)
    (let ((candidate (nth selectrum--current-candidate-index
                          selectrum--refined-candidates)))
      (insert (or (get-text-property
                   0 'selectrum-candidate-full candidate)
                  candidate))
      (apply
       #'run-hook-with-args
       'selectrum-candidate-inserted-hook
       candidate selectrum--read-args))))

;;;; Main entry points

(cl-defun selectrum-read
    (prompt candidates &rest args &key
            default-candidate initial-input require-match)
  "Prompt user with PROMPT to select one of CANDIDATES, list of strings.
Return the selected string. PROMPT should generally end in a
colon and space. Additional keyword ARGS are accepted.
DEFAULT-CANDIDATE, if provided, is sorted first in the list if
it's present. INITIAL-INPUT, if provided, is inserted into the
user input area initially (with point at the end). REQUIRE-MATCH,
if non-nil, means the user must select one of the listed
candidates (so, for example, \\[selectrum-submit-exact-input] has
no effect)."
  (setq selectrum--read-args (cl-list* prompt candidates args))
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
           :default-candidate default-candidate
           :initial-input initial-input
           :require-match (eq require-match t)))
      (let* ((minibuffer-allow-text-properties t)
             ;; Not currently supported as all of our state is global.
             (enable-recursive-minibuffers nil)
             (selected (read-from-minibuffer prompt nil keymap nil t))
             (full
              (or (get-text-property 0 'selectrum-candidate-full selected)
                  selected)))
        (prog1 (if (string-empty-p full)
                   default-candidate
                 full)
          (apply
           #'run-hook-with-args
           'selectrum-candidate-selected-hook
           selected selectrum--read-args))))))

;;;###autoload
(defun selectrum-completing-read
    (prompt collection &optional
            predicate require-match initial-input
            hist def inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'."
  (ignore initial-input hist inherit-input-method)
  (selectrum-read
   prompt (selectrum--normalize-collection collection predicate)
   ;; Don't pass `initial-input'. We use it internally but it's
   ;; deprecated in `completing-read' and doesn't work well with the
   ;; Selectrum paradigm except in specific cases that we control.
   :default-candidate (or (car-safe def) def)
   :require-match require-match))

(defvar selectrum--old-completing-read-function nil
  "Previous value of `completing-read-function'.")

;;;###autoload
(defun selectrum-read-buffer (prompt &optional def require-match predicate)
  "Read buffer using Selectrum. Can be used as `read-buffer-function'.
Actually, as long as `selectrum-completing-read' is installed in
`completing-read-function', `read-buffer' already uses Selectrum.
Installing this function in `read-buffer-function' makes sure the
buffers are sorted in the default order (most to least recently
used) rather than in whatever order is defined by
`selectrum-preprocess-candidates-function', which is likely to be
less appropriate. It also allows you to view hidden buffers,
which is otherwise impossible due to tricky behavior of Emacs'
completion machinery. For PROMPT, DEF, REQUIRE-MATCH, and
PREDICATE, see `read-buffer'."
  (let* ((selectrum-should-sort-p nil)
         (orig-preprocess-function selectrum-preprocess-candidates-function)
         (orig-refine-function selectrum-refine-candidates-function)
         (selectrum-preprocess-candidates-function #'ignore)
         (selectrum-refine-candidates-function
          (lambda (input _)
            (let ((candidates (mapcar #'buffer-name (buffer-list))))
              (if (string-prefix-p " " input)
                  (progn
                    (setq input (substring input 1))
                    (setq candidates
                          (cl-delete-if-not
                           (lambda (name)
                             (string-prefix-p " " name))
                           candidates)))
                (setq candidates
                      (cl-delete-if
                       (lambda (name)
                         (string-prefix-p " " name))
                       candidates)))
              `((candidates . ,(funcall
                                orig-refine-function
                                input
                                (funcall
                                 orig-preprocess-function
                                 candidates)))
                (input . ,input))))))
    (selectrum-completing-read
     prompt nil predicate require-match nil nil def)))

(defvar selectrum--old-read-buffer-function nil
  "Previous value of `read-buffer-function'.")

;;;###autoload
(defun selectrum-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or dir default-directory))))
         (orig-preprocess-function selectrum-preprocess-candidates-function)
         (orig-refine-function selectrum-refine-candidates-function)
         (selectrum-preprocess-candidates-function #'ignore)
         (selectrum-refine-candidates-function
          (lambda (input _)
            (let* ((new-input (file-name-nondirectory input))
                   (dir (or (file-name-directory input) dir))
                   (entries (selectrum--map-destructive
                             (lambda (cell)
                               (let* ((name (car cell))
                                      (type (nth 0 (cdr cell)))
                                      ;; Check if directory (fast) or
                                      ;; symlink to directory
                                      ;; (slower).
                                      (isdir (or (eq t type)
                                                 (and (stringp type)
                                                      (file-directory-p
                                                       (concat dir name))))))
                                 (propertize
                                  name
                                  'selectrum-candidate-display-suffix
                                  (when isdir "/")
                                  'selectrum-candidate-full
                                  (concat dir name (when isdir "/")))))
                             (cl-delete-if
                              (lambda (cell)
                                (and predicate
                                     (not (funcall
                                           predicate
                                           (concat dir (car cell))))))
                              (condition-case _
                                  (directory-files-and-attributes
                                   dir nil "^[^.]\\|^.[^.]" 'nosort)
                                (file-error)
                                ;; May happen in case user quits out
                                ;; of a TRAMP prompt.
                                (quit))))))
              `((candidates . ,(funcall
                                orig-refine-function
                                new-input
                                (funcall
                                 orig-preprocess-function
                                 entries)))
                (input . ,new-input))))))
    (selectrum-read
     prompt nil
     :default-candidate (when-let ((default (or initial default-filename)))
                          (file-name-nondirectory
                           (directory-file-name default)))
     :initial-input dir
     :require-match mustmatch)))

(defvar selectrum--old-read-file-name-function nil
  "Previous value of `read-file-name-function'.")

;;;###autoload
(defun selectrum-read-directory-name
    (prompt &optional dir default-dirname mustmatch initial)
  "Read directory name using Selectrum.
Same as `read-directory-name' except it handles default
candidates a bit better (in particular you can immediately press
\\[selectrum-select-current-candidate] to use the current
directory). For PROMPT, DIR, DEFAULT-DIRNAME, MUSTMATCH, and
INITIAL, see `read-directory-name'."
  (let ((dir (expand-file-name (or dir default-directory)))
        (default (or default-dirname initial dir)))
    (setq default (directory-file-name dir))
    ;; Elisp way of getting the parent directory. If we get nil, that
    ;; means the default was a relative path with only one component,
    ;; so the parent directory is dir.
    (setq dir (or (file-name-directory
                   (directory-file-name default))
                  dir))
    (selectrum-read-file-name
     prompt dir default mustmatch nil #'file-directory-p)))

;;;###autoload
(defun selectrum--fix-dired-read-dir-and-switches (func &rest args)
  "Make \\[dired] do the \"right thing\" with its default candidate.
By default \\[dired] uses `read-file-name' internally, which
causes Selectrum to provide you with the first file inside the
working directory as the default candidate. However, it would
arguably be more semantically appropriate to use
`read-directory-name', and this is especially important for
Selectrum since this causes it to provide you with the working
directory itself as the default candidate.

To test that this advice is working correctly, type \\[dired] and
accept the default candidate. You should have opened the working
directory in Dired, and not a filtered listing for the current
file.

This is an `:around' advice for `dired-read-dir-and-switches'.
FUNC and ARGS are standard as in any `:around' advice."
  (cl-letf* ((orig-read-file-name (symbol-function #'read-file-name))
             ((symbol-function #'read-file-name)
              (lambda (prompt &optional
                              dir default-filename
                              mustmatch initial _predicate)
                (cl-letf (((symbol-function #'read-file-name)
                           orig-read-file-name))
                  (read-directory-name
                   prompt dir default-filename mustmatch initial)))))
    (apply func args)))

(defun selectrum--trailing-components (n path)
  "Take at most N trailing components of PATH.
For large enough N, return PATH unchanged."
  (let* ((n (min n (1+ (cl-count ?/ path))))
         (regexp (concat (string-join (make-list n "[^/]*") "/") "$")))
    (save-match-data
      (string-match regexp path)
      (match-string 0 path))))

;;;###autoload
(defun selectrum-read-library-name ()
  "Read and return a library name.
Similar to `read-library-name' except it handles `load-path'
shadows correctly."
  (eval-and-compile
    (require 'find-func))
  (let ((suffix-regexp (concat (regexp-opt (find-library-suffixes)) "\\'"))
        (table (make-hash-table :test #'equal))
        (lst nil))
    (dolist (dir (or find-function-source-path load-path))
      (condition-case _
          (mapc
           (lambda (entry)
             (unless (string-match-p "^\\.\\.?$" entry)
               (let ((base (file-name-base entry)))
                 (puthash base (cons entry (gethash base table)) table))))
           (directory-files dir 'full suffix-regexp 'nosort))
        (file-error)))
    (maphash
     (lambda (_ paths)
       (setq paths (nreverse (seq-uniq paths)))
       (cl-block nil
         (let ((num-components 1)
               (max-components (apply #'max (mapcar (lambda (path)
                                                      (1+ (cl-count ?/ path)))
                                                    paths))))
           (while t
             (let ((abbrev-paths
                    (seq-uniq
                     (mapcar (lambda (path)
                               (file-name-sans-extension
                                (selectrum--trailing-components
                                 num-components path)))
                             paths))))
               (when (or (= num-components max-components)
                         (= (length paths) (length abbrev-paths)))
                 (let ((candidate-paths
                        (mapcar (lambda (path)
                                  (propertize
                                   (file-name-base path)
                                   'selectrum-candidate-display-prefix
                                   (file-name-directory
                                    (file-name-sans-extension
                                     (selectrum--trailing-components
                                      num-components path)))
                                   'fixedcase 'literal
                                   'selectrum-candidate-full
                                   path))
                                paths)))
                   (setq lst (nconc candidate-paths lst)))
                 (cl-return)))
             (cl-incf num-components)))))
     table)
    (selectrum-read "Library name: " lst :require-match t)))

;;;###autoload
(defun selectrum--fix-set-minibuffer-message (&rest _)
  "Move the minibuffer message overlay to the right place.
This advice fixes the overlay placed by `set-minibuffer-message',
which is different from the one placed by `minibuffer-message'.

By default the overlay is placed at the end, but in the case of
Selectrum this means after all the candidates. We want to move it
instead to just after the user input.

To test that this advice is working correctly, type \\[find-file]
and enter \"/sudo::\", then authenticate. The overlay indicating
that authentication was successful should appear right after the
user input area, not at the end of the candidate list.

This is an `:after' advice for `set-minibuffer-message'."
  (selectrum--when-compile (boundp 'minibuffer-message-overlay)
    (when (overlayp minibuffer-message-overlay)
      (move-overlay minibuffer-message-overlay
                    selectrum--end-of-input-marker
                    selectrum--end-of-input-marker))))

;;;###autoload
(defun selectrum--fix-minibuffer-message (func &rest args)
  "Move the minibuffer message overlay to the right place.
This advice fixes the overlay placed by `minibuffer-message',
which is different from the one placed by
`set-minibuffer-message'.

By default the overlay is placed at the end, but in the case of
Selectrum this means after all the candidates. We want to move it
instead to just after the user input.

To test that this advice is working correctly, type \\[find-file]
twice in a row. The overlay indicating that recursive minibuffers
are not allowed should appear right after the user input area,
not at the end of the candidate list.

This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS are standard as in all `:around' advice."
  (cl-letf* ((orig-make-overlay (symbol-function #'make-overlay))
             ((symbol-function #'make-overlay)
              (lambda (_beg _end &rest args)
                (apply orig-make-overlay
                       selectrum--end-of-input-marker
                       selectrum--end-of-input-marker
                       args))))
    (apply func args)))

;;;###autoload
(progn
  (define-minor-mode selectrum-mode
    "Minor mode to use Selectrum for `completing-read'."
    :global t
    (if selectrum-mode
        (progn
          ;; Make sure not to blow away saved variable values if mode
          ;; is enabled again when already on.
          (selectrum-mode -1)
          (setq selectrum-mode t)
          (setq selectrum--old-completing-read-function
                (default-value 'completing-read-function))
          (setq-default completing-read-function
                        #'selectrum-completing-read)
          (setq selectrum--old-read-buffer-function
                (default-value 'read-buffer-function))
          (setq-default read-buffer-function
                        #'selectrum-read-buffer)
          (setq selectrum--old-read-file-name-function
                (default-value 'read-file-name-function))
          (setq-default read-file-name-function
                        #'selectrum-read-file-name)
          (advice-add #'read-directory-name :override
                      #'selectrum-read-directory-name)
          (with-eval-after-load 'dired
            (eval-and-compile
              (require 'dired))
            (advice-add #'dired-read-dir-and-switches :around
                        #'selectrum--fix-dired-read-dir-and-switches))
          (selectrum--when-compile (fboundp 'read-library-name)
            (advice-add #'read-library-name :override
                        #'selectrum-read-library-name))
          (advice-add #'minibuffer-message :around
                      #'selectrum--fix-minibuffer-message)
          (selectrum--when-compile (fboundp 'set-minibuffer-message)
            (advice-add #'set-minibuffer-message :after
                        #'selectrum--fix-set-minibuffer-message)))
      (when (equal (default-value 'completing-read-function)
                   #'selectrum-completing-read)
        (setq-default completing-read-function
                      selectrum--old-completing-read-function))
      (when (equal (default-value 'read-buffer-function)
                   #'selectrum-read-buffer)
        (setq-default read-buffer-function
                      selectrum--old-read-buffer-function))
      (when (equal (default-value 'read-file-name-function)
                   #'selectrum-read-file-name)
        (setq-default read-file-name-function
                      selectrum--old-read-file-name-function))
      (advice-remove #'read-directory-name
                     #'selectrum-read-directory-name)
      (with-eval-after-load 'dired
        (eval-and-compile
          (require 'dired))
        (advice-remove #'dired-read-dir-and-switches
                       #'selectrum--fix-dired-read-dir-and-switches))
      (selectrum--when-compile (fboundp 'read-library-name)
        (advice-remove #'read-library-name #'selectrum-read-library-name))
      (advice-remove #'minibuffer-message #'selectrum--fix-minibuffer-message)
      (selectrum--when-compile (fboundp 'set-minibuffer-message)
        (advice-remove #'set-minibuffer-message
                       #'selectrum--fix-set-minibuffer-message)))))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; selectrum.el ends here
