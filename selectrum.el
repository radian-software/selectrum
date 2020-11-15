;;; selectrum.el --- Easily select item from list -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 8 Dec 2019
;; Homepage: https://github.com/raxod502/selectrum
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; Selectrum is a better solution for incremental narrowing in Emacs,
;; replacing Helm, Ivy, and IDO. Its design philosophy is based on
;; choosing the right abstractions and prioritizing consistency and
;; predictability over special-cased improvements for particular
;; cases. As such, Selectrum follows existing Emacs conventions where
;; they exist and are reasonable, and it declines to implement
;; features which have marginal benefit compared to the additional
;; complexity of a new interface.

;; Getting started: Selectrum provides a global minor mode,
;; `selectrum-mode', which enhances `completing-read' and all related
;; functions automatically without the need for further configuration.

;; Please see https://github.com/raxod502/selectrum for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'crm)
(require 'map)
(require 'minibuf-eldef)
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

(defface selectrum-completion-annotation
  '((t :inherit completions-annotations))
  "Face used to display annotations in `selectrum-completion-in-region'."
  :group 'selectrum-faces)

(defface selectrum-completion-docsig
  '((t :inherit selectrum-completion-annotation :slant italic))
  "Face used to display docsigs in `selectrum-completion-in-region'."
  :group 'selectrum-faces)

;;;; Variables

(defvar selectrum-should-sort-p t
  "Non-nil if preprocessing and refinement functions should sort.
This is let-bound to nil in some contexts, and should be
respected by user functions for optimal results.")

(defvar selectrum--minibuffer-default-in-prompt-regexps
  (let ((minibuffer-eldef-shorten-default nil))
    (cl-remove-if (lambda (i) (and (consp i) (nth 2 i)))
                  (minibuffer-default--in-prompt-regexps)))
  "Regexps for determining if the prompt message includes the default value.
See `minibuffer-default-in-prompt-regexps', from which this is derived.")

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
candidates (strings).

Returns a new list of candidates. Should not modify the input
list. The returned list may be modified by Selectrum, so a copy
of the input should be made. (Beware that `cl-remove-if' doesn't
make a copy if there's nothing to remove.)"
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

(defvar selectrum-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)

    (define-key map [remap keyboard-quit] #'abort-recursive-edit)
    ;; This is bound in `minibuffer-local-map' by loading `delsel', so
    ;; we have to account for it too.
    (define-key map [remap minibuffer-keyboard-quit]
      #'abort-recursive-edit)
    ;; Override both the arrow keys and C-n/C-p.
    (define-key map [remap previous-line]
      #'selectrum-previous-candidate)
    (define-key map [remap next-line]
      #'selectrum-next-candidate)
    (define-key map [remap previous-line-or-history-element]
      #'selectrum-previous-candidate)
    (define-key map [remap next-line-or-history-element]
      #'selectrum-next-candidate)
    (define-key map [remap exit-minibuffer]
      #'selectrum-select-current-candidate)
    (define-key map [remap scroll-down-command]
      #'selectrum-previous-page)
    (define-key map [remap scroll-up-command]
      #'selectrum-next-page)
    ;; Use `minibuffer-beginning-of-buffer' for Emacs >=27 and
    ;; `beginning-of-buffer' for Emacs <=26.
    (define-key map [remap minibuffer-beginning-of-buffer]
      #'selectrum-goto-beginning)
    (define-key map [remap beginning-of-buffer]
      #'selectrum-goto-beginning)
    (define-key map [remap end-of-buffer]
      #'selectrum-goto-end)
    (define-key map [remap kill-ring-save]
      #'selectrum-kill-ring-save)
    (define-key map [remap previous-matching-history-element]
      #'selectrum-select-from-history)
    (define-key map (kbd "C-M-DEL") #'backward-kill-sexp)
    (define-key map (kbd "C-M-<backspace>") #'backward-kill-sexp)
    (define-key map (kbd "C-j") #'selectrum-submit-exact-input)
    (define-key map (kbd "TAB") #'selectrum-insert-current-candidate)

    ;; Return the map.
    map)
  "Keymap used by Selectrum in the minibuffer.")

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

- `matches': Show the total number of matches.
- `current/matches': Show the index of current match and the
  total number of matches.
- nil: Show nothing."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Count matches" matches)
          (const :tag "Count matches and show current match"
                 current/matches)))

(defcustom selectrum-show-indices nil
  "Non-nil means to add indices to the displayed candidates.
If this is a function, it should take in the row number of the
displayed candidate (starting from 1) as a parameter and it
should return the string to be displayed representing the index
of the candidate. If this is some other non-nil value, it is
treated as if it were (lambda (i) (format \"%2d \" i))."
  :type '(choice function boolean))

(defcustom selectrum-completing-read-multiple-show-help t
  "Non-nil means to show help for `selectrum-completing-read-multiple'.

This options controls insertion of additional usage information
into the prompt when using commands which use
`completing-read-multiple'."
  :type 'boolean)

(defcustom selectrum-fix-minibuffer-height nil
  "Non-nil means the minibuffer always has the same height.
In this case the height will be set to
`selectrum-num-candidates-displayed' lines and will stay at this
height even if there are fewer candidates or the display height
of the candidates take up more space. If this option is nil the
minibuffer height will be determined by the actual display height
of the initial number of candidates and adjusts dynamically to
display up to `selectrum-num-candidates-displayed' candidates."
  :type 'boolean)

(defcustom selectrum-right-margin-padding 1
  "The number of spaces to add after right margin text.
This only takes effect when the
`selectrum-candidate-display-right-margin' property is presented
in candidates.

This option is a workaround for 2 problems:

- Some terminals will wrap the last character of a line when it
  exactly fits.

- Emacs doesn't provide a method to calculate the exact pixel
  width of a unicode char, so a wide char can cause line
  wrapping."
  :type 'integer)

(defcustom selectrum-multiline-display-settings
  '((match      "->"  success)
    (truncation "..." shadow)
    (newline    "\\n" warning)
    (whitespace ".."  shadow))
  "Settings used to configure the formatting of multi-line candidates.

Currently, multi-line candidates are flattened, stripped of
repeated whitespace, and, if need be, truncated. Additionally,
when a multi-line candidate matches the user's input, the
matching line is also displayed at the beginning of the
candidate. This option affects how such formatting looks.

This formatting does not affect the actual value of a candidate.

When customizing this option, a setting for each transformation
\(defined below) must be present in the list.

There are three values that make a setting:
1. A symbol from the following list:
   - `newline' determines the string used to replace line breaks in the
   candidate, which flattens the candidate into one line.
   - `whitespace' determines the string used to replace repeated
   whitespace, which shortens the candidate.
   - `truncation' determines the string to append to a flattened and
   truncated candidate.
   - `match' determines the string to insert between the matching
    line and the flattened candidate.
2. A string to indicate the display change.
3. A face to assign to the indicator string.

Therefore, a setting is represented as a list with three
elements: a symbol, a string, and a face, in that order.
This option is itself a list of 4 sub-lists, one for each
setting."
  :type '(repeat (list :tag "Display settings"
                       (choice (const :tag "Matching line"
                                      match)
                               (const :tag "Line truncation"
                                      truncation)
                               (const :tag "New lines"
                                      newline)
                               (const :tag "Repeated whitespace"
                                      whitespace))
                       (string :tag "Indicator string")
                       (face :tag "Indicator face"))))

(defcustom selectrum-extend-current-candidate-highlight nil
  "Whether to extend highlighting of the current candidate until the margin.

Nil (the default) means to only highlight the displayed text."
  :type 'boolean)

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
COLLECTION may be a list of strings or symbols or cons cells, an
obarray, a hash table, or a function, as per the docstring of
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
       (setq elt (or (car-safe elt) elt))
       (when (symbolp elt)
         (setq elt (symbol-name elt)))
       elt)
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

(defun selectrum--get-annotation-suffix (string annotation-func)
  "Get `selectrum-candidate-display-suffix' value for annotation.
Used to display STRING according to ANNOTATION-FUNC from
metadata."
  ;; Rule out situations where the annotation
  ;; is nil.
  (when-let ((annotation (funcall annotation-func string)))
    (propertize
     annotation
     'face 'selectrum-completion-annotation)))

(defun selectrum--get-margin-docsig (string docsig-func)
  "Get `selectrum-candidate-display-right-margin' value for docsig.
Used to display STRING according to DOCSIG-FUNC from metadata."
  (when-let ((docsig (funcall docsig-func string)))
    (propertize
     (format "%s" docsig)
     'face 'selectrum-completion-docsig)))

(defun selectrum--remove-default-from-prompt (prompt)
  "Remove the indication of the default value from PROMPT.
Selectrum has its own methods for indicating the default value,
making other methods redundant."
  (save-match-data
    (let ((regexps selectrum--minibuffer-default-in-prompt-regexps))
      (cl-dolist (matcher regexps prompt)
        (let ((regex (if (stringp matcher) matcher (car matcher))))
          (when (string-match regex prompt)
            (cl-return
             (replace-match "" nil nil prompt
                            (if (consp matcher)
                                (cadr matcher)
                              0)))))))))

;;;; Minibuffer state

(defvar selectrum--start-of-input-marker nil
  "Marker at the start of the minibuffer user input.
This is used to prevent point from moving into the prompt.")

(defvar selectrum--end-of-input-marker nil
  "Marker at the end of the minibuffer user input.
This is used to prevent point from moving into the candidates.")

(defvar selectrum--candidates-overlay nil
  "Overlay used to display current candidates.")

(defvar selectrum--preprocessed-candidates nil
  "Preprocessed list of candidates.
This is derived from the argument passed to `selectrum-read'. If
the collection is a list it is processed once by
`selectrum-preprocess-candidates-function' and saved in this
variable. If the collection is a function then that function is
stored in this variable instead, so that it can be called to get
a new list of candidates every time the input changes. (See
`selectrum-read' for more details on function collections.)

With a standard candidate list, the value of this variable is
subsequently passed to `selectrum-refine-candidates-function'.
With a dynamic candidate list (generated by a function), then the
returned list is subsequently passed also through
`selectrum-preprocess-candidates-function' each time the user
input changes. Either way, the results end up in
`selectrum--refined-candidates'.")

(defvar selectrum--refined-candidates nil
  "Refined list of candidates to be displayed.
This is derived from `selectrum--preprocessed-candidates' by
`selectrum-refine-candidates-function' (and, in the case of a
dynamic candidate list, also
`selectrum-preprocess-candidates-function') every time the user
input changes, and is subsequently passed to
`selectrum-highlight-candidates-function'.")

(defvar selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.")

(defvar selectrum--first-index-displayed nil
  "Index of the first displayed candidate.")

(defvar selectrum--previous-input-string nil
  "Previous user input string in the minibuffer.
Used to check if the user input has changed and candidates need
to be re-filtered.")

(defvar selectrum--match-required-p nil
  "Non-nil if the user must select one of the candidates.
Equivalently, nil if the user is allowed to submit their own
input that does not match any of the displayed candidates.")

(defvar selectrum--crm-p nil
  "Non-nil for `selectrum-completing-read-multiple' sessions.")

(defvar selectrum--move-default-candidate-p nil
  "Non-nil means move default candidate to start of list.
Nil means select the default candidate initially even if it's not
at the start of the list.")

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

(defvar selectrum--last-command nil
  "Name of last interactive command that invoked Selectrum.")

(defvar selectrum--last-prefix-arg nil
  "Prefix argument given to last interactive command that invoked Selectrum.")

(defvar selectrum--repeat nil
  "Non-nil means try to restore the minibuffer state during setup.
This is used to implement `selectrum-repeat'.")

(defvar selectrum-active-p nil
  "Non-nil means Selectrum is currently active.")

(defvar-local selectrum--skip-updates-p nil
  "If selectrum should skip updates.

In normal operation Selectrum checks for updating its UI after
each command. When this variable is non-nil the computation of
updates is skipped.")

(defvar-local selectrum--init-p nil
  "Non-nil means the current session is initializing.
This is non-nil during the first call of
`selectrum--minibuffer-post-command-hook'.")

(defvar selectrum--total-num-candidates nil
  "Saved number of candidates, used for `selectrum-show-indices'.")

;;;;; Minibuffer state utility functions

(defun selectrum-get-current-candidate (&optional notfull)
  "Return currently selected Selectrum candidate.
If NOTFULL is non-nil don't use canonical representation of
candidate as per `selectrum-candidate-full' text property."
  (when (and selectrum-active-p
             selectrum--current-candidate-index)
    (if notfull
        (selectrum--get-candidate
         selectrum--current-candidate-index)
      (selectrum--get-full
       (selectrum--get-candidate
        selectrum--current-candidate-index)))))

(defun selectrum-get-current-candidates (&optional notfull)
  "Get list of current Selectrum candidates.
If NOTFULL is non-nil don't use canonical representation of
candidates as per `selectrum-candidate-full' text property."
  (when (and selectrum-active-p
             selectrum--refined-candidates)
    (if notfull
        selectrum--refined-candidates
      (cl-loop for cand in selectrum--refined-candidates
               collect (selectrum--get-full cand)))))

(defun selectrum-get-current-input ()
  "Get current Selectrum user input."
  (when selectrum-active-p
    (with-selected-window (active-minibuffer-window)
      (minibuffer-contents))))

(defun selectrum-set-selected-candidate (&optional string)
  "Set currently selected candidate to STRING.
STRING defaults to `minibuffer-contents'. Computation of
candidates is skipped from there on. This is useful for injecting
a candidate in `minibuffer-setup-hook' and immediately exit with
it afterwards. With default completion there is no computation
triggered initially and this function can be used to mimic this
behavior."
  (when selectrum-active-p
    (with-selected-window (active-minibuffer-window)
      (let ((string (or string (minibuffer-contents))))
        (setq selectrum--refined-candidates
              (list string))
        (setq selectrum--current-candidate-index 0)
        ;; Skip updates.
        (setq-local selectrum--skip-updates-p t)))))

(defun selectrum--get-full (candidate)
  "Get full form of CANDIDATE by inspecting text properties."
  (or (get-text-property 0 'selectrum-candidate-full candidate)
      candidate))

(defun selectrum--get-candidate (index)
  "Get candidate at given INDEX. Negative means get the current user input."
  (if (and index (>= index 0))
      (nth index selectrum--refined-candidates)
    (buffer-substring-no-properties
     selectrum--start-of-input-marker
     selectrum--end-of-input-marker)))

(defun selectrum--get-meta (setting &optional table pred input)
  "Get metadata SETTING from TABLE.
TABLE defaults to `minibuffer-completion-table'.
PRED defaults to `minibuffer-completion-predicate'.
INPUT defaults to current selectrum input string."
  (let ((input (or input (minibuffer-contents)))
        (pred (or pred minibuffer-completion-predicate))
        (table (or table minibuffer-completion-table)))
    (when table
      (completion-metadata-get
       (completion-metadata input table pred) setting))))

(defun selectrum--get-candidates-from-table (&optional table pred)
  "Get candidates from TABLE.
TABLE defaults to `minibuffer-completion-table'.
PRED defaults to `minibuffer-completion-predicate'."
  (let ((annotf (or (selectrum--get-meta 'annotation-function table pred)
                    (plist-get completion-extra-properties
                               :annotation-function)))
        (docsigf (plist-get completion-extra-properties
                            :company-docsig))
        (strings (selectrum--normalize-collection
                  (or table minibuffer-completion-table)
                  (or pred minibuffer-completion-predicate))))
    (cond ((or annotf docsigf)
           (let ((cands ()))
             (dolist (string strings (nreverse cands))
               (push (apply #'propertize
                            string
                            (append
                             (when annotf
                               (list 'selectrum-candidate-display-suffix
                                     (selectrum--get-annotation-suffix
                                      string annotf)))
                             (when docsigf
                               (list 'selectrum-candidate-display-right-margin
                                     (selectrum--get-margin-docsig
                                      string docsigf)))))
                     cands))))
          (t strings))))

(defun selectrum-exhibit ()
  "Trigger an update of Selectrum's completion UI."
  (when-let ((mini (active-minibuffer-window)))
    (with-selected-window mini
      (when (and minibuffer-completion-table
                 (not (functionp selectrum--preprocessed-candidates)))
        (setq selectrum--preprocessed-candidates nil))
      (setq selectrum--previous-input-string nil)
      (selectrum--minibuffer-post-command-hook))))

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
  (unless selectrum--skip-updates-p
    ;; Stay within input area.
    (goto-char (max (point) selectrum--start-of-input-marker))
    (goto-char (min (point) selectrum--end-of-input-marker))
    ;; For some reason this resets and thus can't be set in setup hook.
    (setq-local truncate-lines t)
    (let ((inhibit-read-only t)
          ;; Don't record undo information while messing with the
          ;; minibuffer, as per
          ;; <https://github.com/raxod502/selectrum/issues/31>.
          (buffer-undo-list t)
          (input (buffer-substring selectrum--start-of-input-marker
                                   selectrum--end-of-input-marker))
          (bound (marker-position selectrum--end-of-input-marker))
          (keep-mark-active (not deactivate-mark)))
      (unless (equal input selectrum--previous-input-string)
        (when (and (not selectrum--preprocessed-candidates)
                   minibuffer-completion-table)
          ;; No candidates were passed, initialize them from
          ;; `minibuffer-completion-table'.
          (setq selectrum--preprocessed-candidates
                (funcall selectrum-preprocess-candidates-function
                         (selectrum--get-candidates-from-table))))
        (setq selectrum--previous-input-string input)
        ;; Reset the persistent input, so that it will be nil if
        ;; there's no special attention needed.
        (setq selectrum--visual-input nil)
        (let ((cands (if (functionp selectrum--preprocessed-candidates)
                         (funcall
                          selectrum-preprocess-candidates-function
                          (let ((result
                                 (funcall
                                  selectrum--preprocessed-candidates
                                  input)))
                            ;; Avoid modifying the returned
                            ;; candidates to let the function
                            ;; reuse them.
                            (copy-sequence
                             (if (stringp (car result))
                                 result
                               (setq input (or (alist-get 'input result)
                                               input))
                               (setq selectrum--visual-input input)
                               (alist-get 'candidates result)))))
                       selectrum--preprocessed-candidates)))
          (setq selectrum--total-num-candidates (length cands))
          (setq selectrum--refined-candidates
                (funcall selectrum-refine-candidates-function input cands)))
        (when selectrum--move-default-candidate-p
          (setq selectrum--refined-candidates
                (selectrum--move-to-front-destructive
                 selectrum--default-candidate
                 selectrum--refined-candidates)))
        (setq selectrum--refined-candidates
              (selectrum--move-to-front-destructive
               input selectrum--refined-candidates))
        (setq selectrum--refined-candidates
              (delete "" selectrum--refined-candidates))
        (if selectrum--repeat
            (progn
              (setq selectrum--current-candidate-index
                    (and (> (length selectrum--refined-candidates) 0)
                         (min (or selectrum--current-candidate-index 0)
                              (1- (length selectrum--refined-candidates)))))
              (setq selectrum--repeat nil))
          (setq selectrum--current-candidate-index
                (cond
                 ((null selectrum--refined-candidates)
                  (when (not selectrum--match-required-p)
                    -1))
                 ((and selectrum--default-candidate
                       (string-empty-p (minibuffer-contents))
                       (not (member selectrum--default-candidate
                                    selectrum--refined-candidates)))
                  -1)
                 ((and selectrum--init-p
                       minibuffer-completing-file-name
                       (eq minibuffer-completion-predicate
                           'file-directory-p)
                       (equal (minibuffer-contents)
                              selectrum--default-candidate))
                  ;; When reading directories and the default is the
                  ;; prompt, select it initially.
                  -1)
                 (selectrum--move-default-candidate-p
                  0)
                 (t
                  (or (cl-position selectrum--default-candidate
                                   selectrum--refined-candidates
                                   :key #'selectrum--get-full
                                   :test #'equal)
                      0))))))
      (overlay-put selectrum--count-overlay
                   'before-string (selectrum--count-info))
      (overlay-put selectrum--count-overlay
                   'priority 1)
      (setq input (or selectrum--visual-input input))
      (setq selectrum--first-index-displayed
            (if selectrum--current-candidate-index
                (selectrum--clamp
                 ;; Adding one here makes it look slightly better, as
                 ;; there are guaranteed to be more candidates shown
                 ;; below the selection than above.
                 (1+ (- selectrum--current-candidate-index
                        (max 1 (/ selectrum-num-candidates-displayed 2))))
                 0
                 (max (- (length selectrum--refined-candidates)
                         selectrum-num-candidates-displayed)
                      0))
              0))
      (let* ((first-index-displayed selectrum--first-index-displayed)
             (displayed-candidates
              (seq-take
               (nthcdr
                first-index-displayed
                selectrum--refined-candidates)
               selectrum-num-candidates-displayed))
             (highlighted-index (and selectrum--current-candidate-index
                                     (- selectrum--current-candidate-index
                                        first-index-displayed))))
        (setq displayed-candidates
              (seq-take displayed-candidates
                        selectrum-num-candidates-displayed))
        (let ((text (selectrum--candidates-display-string
                     displayed-candidates
                     input
                     highlighted-index))
              (default nil))
          (if (or (and highlighted-index
                       (< highlighted-index 0))
                  (and (not selectrum--match-required-p)
                       (not displayed-candidates))
                  (and selectrum--default-candidate
                       (not minibuffer-completing-file-name)
                       (not (member selectrum--default-candidate
                                    selectrum--refined-candidates))))
              (if (= (minibuffer-prompt-end) bound)
                  (setq default
                        (format " %s %s%s"
                                (propertize
                                 "[default value:"
                                 'face 'minibuffer-prompt)
                                (propertize
                                 (or (and selectrum--default-candidate
                                          (substring-no-properties
                                           selectrum--default-candidate))
                                     "\"\"")
                                 'face
                                 (if (and selectrum--current-candidate-index
                                          (< selectrum--current-candidate-index
                                             0))
                                     'selectrum-current-candidate
                                   'minibuffer-prompt))
                                (propertize "]" 'face 'minibuffer-prompt)))
                (when (and highlighted-index
                           (< highlighted-index 0))
                  (add-text-properties
                   (minibuffer-prompt-end) bound
                   '(face selectrum-current-candidate))))
            (remove-text-properties
             (minibuffer-prompt-end) bound
             '(face selectrum-current-candidate)))
          (move-overlay selectrum--candidates-overlay
                        (point-max) (point-max) (current-buffer))
          (setq text (concat (or default " ") text))
          (put-text-property 0 1 'cursor t text)
          (overlay-put selectrum--candidates-overlay 'after-string text))
        (selectrum--update-minibuffer-height first-index-displayed
                                             highlighted-index
                                             displayed-candidates))
      (setq selectrum--end-of-input-marker (set-marker (make-marker) bound))
      (set-marker-insertion-type selectrum--end-of-input-marker t)
      (when keep-mark-active
        (setq deactivate-mark nil))
      (setq-local selectrum--init-p nil))))

(defun selectrum--update-minibuffer-height (first highlighted cands)
  "Set minibuffer height for candidates display.
FIRST is the index of the first displayed candidate. HIGHLIGHTED
is the index if the highlighted candidate. CANDS are the
currently displayed candidates."
  (when-let ((n (if selectrum-fix-minibuffer-height
                    (1+ selectrum-num-candidates-displayed)
                  (max (window-height)               ; grow only
                       (1+ (length cands)))))
             (win (active-minibuffer-window)))
    ;; Don't attempt to resize a minibuffer frame.
    (unless (frame-root-window-p win)
      ;; Set min initial height.
      (with-selected-window win
        (setf (window-height) n))
      ;; Adjust if needed.
      (unless selectrum-fix-minibuffer-height
        (when (or selectrum--init-p
                  (and selectrum--current-candidate-index
                       ;; Allow size change when navigating, not while
                       ;; typing.
                       (/= first highlighted)
                       ;; Don't allow shrinking.
                       (= (length cands)
                          selectrum-num-candidates-displayed)))
          (let ((dheight (cdr (window-text-pixel-size win)))
                (wheight (window-pixel-height win)))
            (when (/= dheight wheight)
              (window-resize
               win (- dheight wheight) nil nil 'pixelwise))))))))

(defun selectrum--ensure-single-lines (candidates)
  "Return list of single-line CANDIDATES.
Multi-line candidates are merged into a single line. The resulting
single-line candidates are then shortened by replacing repeated
whitespace and maybe truncating the result.

The specific details of the formatting are determined by
`selectrum-multiline-display-settings'."
  (let* ((single/lines ())

         ;; The formatting settings are the same for all multi-line
         ;; candidates, and so only need to be gotten once from
         ;; `selectrum-multiline-display-settings'.
         ;;
         ;; - Matching lines
         (match/transformation
          (alist-get 'match selectrum-multiline-display-settings))
         (match/display (car match/transformation))
         (match/face (cadr match/transformation))
         ;; - Truncated candidate
         (truncation/transformation
          (alist-get 'truncation selectrum-multiline-display-settings))
         (truncation/display (car truncation/transformation))
         (truncation/face (cadr truncation/transformation))
         ;; - Newlines
         (newline/transformation
          (alist-get 'newline selectrum-multiline-display-settings))
         (newline/display (car newline/transformation))
         (newline/face (cadr newline/transformation))
         ;; - Repeated whitespace
         (whitespace/transformation
          (alist-get 'whitespace selectrum-multiline-display-settings))
         (whitespace/display (car whitespace/transformation))
         (whitespace/face (cadr whitespace/transformation)))

    (dolist (cand candidates (nreverse single/lines))
      (push
       (if (string-match-p "\n" cand)
           (replace-regexp-in-string
            "\n" (propertize newline/display 'face newline/face)
            (replace-regexp-in-string
             "[ \t][ \t]+"
             (propertize whitespace/display 'face whitespace/face)
             (concat (unless (string-empty-p (minibuffer-contents))
                       ;; Show first matched line.
                       (when-let ((match
                                   (car (funcall
                                         selectrum-refine-candidates-function
                                         (minibuffer-contents)
                                         (split-string cand "\n")))))
                         (concat match
                                 (propertize match/display 'face match/face))))
                     (if (< (length cand) 1000)
                         cand
                       (concat
                        (substring cand 0 1000)
                        (propertize truncation/display
                                    'face truncation/face))))
             ;; Replacements should be fixed-case and literal, to make things
             ;; simpler.
             'fixed-case 'literal)
            'fixed-case 'literal)
         cand)
       single/lines))))

(defun selectrum--candidates-display-string (candidates
                                             input
                                             highlighted-index)
  "Get string to display CANDIDATES.
INPUT is the current user input. CANDIDATES are the candidates
for display. HIGHLIGHTED-INDEX is the currently selected index
and FIRST-INDEX-DISPLAYED is the index of the top most
candidate."
  (let ((index 0)
        (lines
         (selectrum--ensure-single-lines
          ;; First pass the candidates to the highlight function
          ;; before stripping multi-lines because it might expect
          ;; getting passed the same candidates as were passed
          ;; to the filter function (for example `orderless'
          ;; requires this).
          (funcall selectrum-highlight-candidates-function
                   input candidates))))
    (with-temp-buffer
      (dolist (candidate lines)
        (let ((displayed-candidate
               (concat
                (get-text-property
                 0 'selectrum-candidate-display-prefix
                 candidate)
                candidate
                (get-text-property
                 0 'selectrum-candidate-display-suffix
                 candidate)))
              (right-margin (get-text-property
                             0 'selectrum-candidate-display-right-margin
                             candidate))
              (formatting-current-candidate
               (equal index highlighted-index)))
          ;; Add the ability to interact with candidates via the mouse.
          (add-text-properties
           0 (length displayed-candidate)
           (list
            'mouse-face 'highlight
            'help-echo
            "mouse-1: select candidate\nmouse-3: insert candidate"
            'keymap
            (let ((keymap (make-sparse-keymap)))
              (define-key keymap [mouse-1]
                `(lambda ()
                   (interactive)
                   (selectrum-select-current-candidate ,(1+ index))))
              (define-key keymap [mouse-3]
                `(lambda ()
                   (interactive)
                   (selectrum-insert-current-candidate ,(1+ index))))
              keymap))
           displayed-candidate)
          (when formatting-current-candidate
            (setq displayed-candidate
                  (copy-sequence displayed-candidate))
            ;; Avoid trampling highlighting done by
            ;; `selectrum-highlight-candidates-function'. In
            ;; Emacs<27 `add-face-text-property' has a bug but
            ;; in Emacs>=27 `font-lock-prepend-text-property'
            ;; doesn't work. Even though these functions are
            ;; both supposed to do the same thing.
            ;;
            ;; Anyway, no need to clean up the text properties
            ;; afterwards, as an update will cause all these
            ;; strings to be thrown away and re-generated from
            ;; scratch.
            ;;
            ;; See:
            ;; <https://github.com/raxod502/selectrum/issues/21>
            ;; <https://github.com/raxod502/selectrum/issues/58>
            ;; <https://github.com/raxod502/selectrum/pull/76>
            (if (version< emacs-version "27")
                (font-lock-prepend-text-property
                 0 (length displayed-candidate)
                 'face 'selectrum-current-candidate displayed-candidate)
              (add-face-text-property
               0 (length displayed-candidate)
               'selectrum-current-candidate
               'append displayed-candidate)))
          (insert "\n")
          (when selectrum-show-indices
            (let* ((display-fn (if (functionp selectrum-show-indices)
                                   selectrum-show-indices
                                 (lambda (i) (format "%2d " i))))
                   (curr-index (substring-no-properties
                                (funcall display-fn (1+ index)))))
              (insert
               (propertize curr-index 'face 'minibuffer-prompt))))
          (insert displayed-candidate)
          (cond
           (right-margin
            (insert
             (concat
              (propertize
               " "
               'face
               (when formatting-current-candidate
                 'selectrum-current-candidate)
               'display
               `(space :align-to (- right-fringe
                                    ,(string-width right-margin)
                                    selectrum-right-margin-padding)))
              (propertize right-margin
                          'face
                          (when formatting-current-candidate
                            'selectrum-current-candidate)))))
           ((and selectrum-extend-current-candidate-highlight
                 formatting-current-candidate)
            (insert
             (propertize
              " "
              'face 'selectrum-current-candidate
              'display
              `(space :align-to (- right-fringe
                                   selectrum-right-margin-padding)))))))
        (cl-incf index))
      (buffer-string))))

(defun selectrum--minibuffer-exit-hook ()
  "Clean up Selectrum from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'selectrum--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook 'local)
  (when (overlayp selectrum--count-overlay)
    (delete-overlay selectrum--count-overlay))
  (setq selectrum--count-overlay nil))

(cl-defun selectrum--minibuffer-setup-hook
    (candidates &key default-candidate initial-input)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of strings that was passed to
`selectrum-read'. DEFAULT-CANDIDATE, if provided, is added to the
list and sorted first. INITIAL-INPUT, if provided, is inserted
into the user input area to start with."
  (add-hook
   'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook nil 'local)
  (setq-local selectrum--init-p t)
  (unless selectrum--candidates-overlay
    (setq selectrum--candidates-overlay
          (make-overlay (point) (point) nil 'front-advance 'rear-advance)))
  (setq selectrum--start-of-input-marker (point-marker))
  (if selectrum--repeat
      (insert selectrum--previous-input-string)
    (when initial-input
      (insert initial-input)))
  (setq selectrum--end-of-input-marker (point-marker))
  (set-marker-insertion-type selectrum--end-of-input-marker t)
  ;; If metadata specifies a custom sort function use it as
  ;; `selectrum-preprocess-candidates-function' for this session.
  (when-let ((sortf (selectrum--get-meta 'display-sort-function)))
    (setq-local selectrum-preprocess-candidates-function sortf))
  (setq selectrum--preprocessed-candidates
        (if (functionp candidates)
            candidates
          (funcall selectrum-preprocess-candidates-function candidates)))
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
          (max (if (and selectrum--match-required-p
                        (cond (minibuffer-completing-file-name
                               (not (file-exists-p
                                     (substitute-in-file-name
                                      (minibuffer-contents)))))
                              (t
                               (not (string-empty-p
                                     (minibuffer-contents))))))
                   0
                 -1)
               (1- selectrum--current-candidate-index)))))

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
    (when selectrum--current-candidate-index
      (kill-new
       (selectrum-get-current-candidate)))))

(defun selectrum--exit-with (candidate)
  "Exit minibuffer with given CANDIDATE.
If `selectrum--crm-p' is non-nil exit with the choosen candidates
plus CANDIDATE."
  (let* ((result (cond ((and selectrum--crm-p
                             (string-match crm-separator
                                           selectrum--previous-input-string))
                        (with-temp-buffer
                          (insert selectrum--previous-input-string)
                          (goto-char (point-min))
                          (while (re-search-forward crm-separator nil t))
                          (delete-region (point) (point-max))
                          (insert (selectrum--get-full candidate))
                          (buffer-string)))
                       (t
                        (apply
                         #'run-hook-with-args
                         'selectrum-candidate-selected-hook
                         candidate selectrum--read-args)
                        (selectrum--get-full candidate))))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (if (string-empty-p result)
                (or selectrum--default-candidate result)
              result))
    (exit-minibuffer)))

(defun selectrum--index-for-arg (arg)
  "Get candidate index for interactive argument ARG.
This is a helper function for commands which allow choosing a
candidate via prefix argument."
  (if arg
      (min
       (+ (prefix-numeric-value arg)
          (1- selectrum--first-index-displayed))
       (1- (length selectrum--refined-candidates)))
    selectrum--current-candidate-index))

(defun selectrum-select-current-candidate (&optional arg)
  "Exit minibuffer, picking the currently selected candidate.
If there are no candidates, return the current user input, unless
a match is required, in which case do nothing.

Give a prefix argument ARG to select the nth displayed candidate.
Zero means to select the current user input. See
`selectrum-show-indices' which can be used to show candidate
indices."
  (interactive "P")
  (unless selectrum-active-p
    (user-error "Cannot select a candidate when Selectrum is not active"))
  (let ((index (selectrum--index-for-arg arg)))
    (when (or (not selectrum--match-required-p)
              (and index (>= index 0))
              (and minibuffer-completing-file-name
                   (file-exists-p
                    (substitute-in-file-name
                     (minibuffer-contents))))
              (string-empty-p
               (minibuffer-contents)))
      (selectrum--exit-with
       (selectrum--get-candidate index)))))

(defun selectrum-submit-exact-input ()
  "Exit minibuffer, using the current user input.
This differs from `selectrum-select-current-candidate' in that it
ignores the currently selected candidate, if one exists."
  (interactive)
  (unless selectrum-active-p
    (user-error "Cannot select a candidate when Selectrum is not active"))
  (unless selectrum--match-required-p
    (selectrum--exit-with
     (buffer-substring-no-properties
      selectrum--start-of-input-marker
      selectrum--end-of-input-marker))))

(defvar selectrum--crm-separator-alist
  '((":\\|,\\|\\s-" . ",")
    ("[ \t]*:[ \t]*" . ":")
    ("[ \t]*,[ \t]*" . ",")
    (" " . " "))
  "Values of `crm-separator' mapped to separator strings.
If current `crm-separator' has a mapping the separator gets
inserted automatically when using
`selectrum-insert-current-candidate'.")

(defun selectrum-insert-current-candidate (&optional arg)
  "Insert current candidate into user input area.

Give a prefix argument ARG to select the nth displayed candidate.
Zero means to select the current user input. See
`selectrum-show-indices' which can be used to show candidate
indices."
  (interactive "P")
  (if-let ((index (selectrum--index-for-arg arg))
           (candidate (selectrum--get-candidate index))
           (full (selectrum--get-full candidate)))
      (progn
        (if (not selectrum--crm-p)
            (progn
              (delete-region selectrum--start-of-input-marker
                             selectrum--end-of-input-marker)
              (insert full))
          (goto-char
           (if (re-search-backward crm-separator
                                   (minibuffer-prompt-end) t)
               (match-end 0)

             (minibuffer-prompt-end)))
          (delete-region (point) selectrum--end-of-input-marker)
          (insert full)
          (when-let ((match
                      (assoc crm-separator selectrum--crm-separator-alist)))
            (insert (cdr match))))
        (apply
         #'run-hook-with-args
         'selectrum-candidate-inserted-hook
         candidate selectrum--read-args))
    (unless completion-fail-discreetly
      (ding)
      (minibuffer-message "No match"))))

(defun selectrum-select-from-history ()
  "Select a candidate from the minibuffer history.
If Selectrum isn't active, insert this candidate into the
minibuffer."
  (interactive)
  (let ((selectrum-should-sort-p nil)
        (enable-recursive-minibuffers t)
        (history (symbol-value minibuffer-history-variable)))
    (when (eq history t)
      (user-error "No history is recorded for this command"))
    (let ((result
           (let ((selectrum-candidate-inserted-hook nil)
                 (selectrum-candidate-selected-hook nil))
             (selectrum-read "History: " history :history t))))
      (if (and selectrum--match-required-p
               (not (member result selectrum--refined-candidates)))
          (user-error "That history element is not one of the candidates")
        (if selectrum-active-p
            (selectrum--exit-with result)
          (insert result))))))

(defvar selectrum--minibuffer-local-filename-syntax
  (let ((table (copy-syntax-table minibuffer-local-filename-syntax)))
    (modify-syntax-entry ?\s "_" table)
    table)
  "Syntax table for reading file names.
Same as `minibuffer-local-filename-syntax' but considers spaces
as symbol constituents.")

;;;; Main entry points

(defmacro selectrum--let-maybe (pred varlist &rest body)
  "If PRED evaluates to non-nil, bind variables in VARLIST and eval BODY.
Otherwise, just eval BODY."
  (declare (indent 0))
  `(if ,pred
       (let ,varlist
         ,@body)
     ,@body))

(defmacro selectrum--save-global-state (&rest body)
  "Eval BODY, restoring all Selectrum global variables afterward."
  (declare (indent 0))
  `(let (,@(mapcar
            (lambda (var)
              `(,var ,var))
            '(selectrum--start-of-input-marker
              selectrum--end-of-input-marker
              selectrum--preprocessed-candidates
              selectrum--refined-candidates
              selectrum--match-required-p
              selectrum--move-default-candidate-p
              selectrum--default-candidate
              selectrum--visual-input
              selectrum--read-args
              selectrum--count-overlay
              selectrum--repeat
              selectrum-active-p)))
     ;; https://github.com/raxod502/selectrum/issues/39#issuecomment-618350477
     (selectrum--let-maybe
       selectrum-active-p
       (,@(mapcar
           (lambda (var)
             `(,var ,var))
           '(selectrum--current-candidate-index
             selectrum--first-index-displayed
             selectrum--previous-input-string
             selectrum--last-command
             selectrum--last-prefix-arg)))
       ,@body)))

(cl-defun selectrum-read
    (prompt candidates &rest args &key
            default-candidate initial-input require-match
            history no-move-default-candidate
            may-modify-candidates
            minibuffer-completion-table
            minibuffer-completion-predicate)
  "Prompt user with PROMPT to select one of CANDIDATES.
Return the selected string.

CANDIDATES is a list of strings or a function to dynamically
generate them. If CANDIDATES is a function, then it receives one
argument, the current user input, and returns the list of
strings. If CANDIDATES are nil the candidates will be computed
from MINIBUFFER-COMPLETION-TABLE.

Instead of a list of strings, the function may alternatively
return an alist with the following keys:
- `candidates': list of strings, as above.
- `input' (optional): transformed user input, used for
  highlighting (see `selectrum-highlight-candidates-function').

PROMPT should generally end in a colon and space. Additional
keyword ARGS are accepted.

DEFAULT-CANDIDATE, if provided, is sorted first in the list if
it's present.

INITIAL-INPUT, if provided, is inserted into the user input area
initially (with point at the end).

REQUIRE-MATCH, if non-nil, means the user must select one of the
listed candidates (so, for example,
\\[selectrum-submit-exact-input] has no effect).

HISTORY is the `minibuffer-history-variable' to use (by default
`minibuffer-history').

NO-MOVE-DEFAULT-CANDIDATE, if non-nil, means that the default
candidate is not sorted first. Instead, it is left at its
original position in the candidate list. However, it is still
selected initially. This is handy for `switch-to-buffer' and
friends, for which getting the candidate list out of order at all
is very confusing.

MAY-MODIFY-CANDIDATES, if non-nil, means that Selectrum is
allowed to modify the CANDIDATES list destructively. Otherwise a
copy is made.

For MINIBUFFER-COMPLETION-TABLE and
MINIBUFFER-COMPLETION-PREDICATE see `minibuffer-completion-table'
and `minibuffer-completion-predicate'. They are used for internal
purposes and compatibility to Emacs completion API. By passing
these as keyword arguments they will be dynamically bound as per
semantics of `cl-defun'."
  (unless (or may-modify-candidates
              (functionp candidates))
    (setq candidates (copy-sequence candidates)))
  (selectrum--save-global-state
    (setq selectrum--read-args (cl-list* prompt candidates args))
    (unless selectrum--repeat
      (setq selectrum--last-command this-command)
      (setq selectrum--last-prefix-arg current-prefix-arg))
    (setq selectrum--match-required-p require-match)
    (setq selectrum--move-default-candidate-p (not no-move-default-candidate))
    (minibuffer-with-setup-hook
        (lambda ()
          (selectrum--minibuffer-setup-hook
           candidates
           :default-candidate default-candidate
           :initial-input initial-input))
      (let* ((minibuffer-allow-text-properties t)
             (resize-mini-windows 'grow-only)
             (max-mini-window-height
              (1+ selectrum-num-candidates-displayed))
             (prompt (selectrum--remove-default-from-prompt prompt))
             ;; <https://github.com/raxod502/selectrum/issues/99>
             (icomplete-mode nil)
             (selectrum-active-p t)
             (res (read-from-minibuffer
                   prompt nil selectrum-minibuffer-map nil
                   (or history 'minibuffer-history))))
        (cond (minibuffer-completion-table
               ;; Behave like completing-read-default which strips the text
               ;; properties but keeps them when submitting the empty prompt
               ;; to get the default (see #180, #107).
               (if (and selectrum--previous-input-string
                        (string-empty-p selectrum--previous-input-string)
                        (equal res selectrum--default-candidate))
                   selectrum--default-candidate
                 (substring-no-properties res)))
              (t res))))))

;;;###autoload
(defun selectrum-completing-read
    (prompt collection &optional
            predicate require-match initial-input
            hist def inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'."
  (ignore initial-input inherit-input-method)
  (selectrum-read
   prompt nil
   ;; Don't pass `initial-input'. We use it internally but it's
   ;; deprecated in `completing-read' and doesn't work well with the
   ;; Selectrum paradigm except in specific cases that we control.
   :default-candidate (or (car-safe def) def)
   :require-match (eq require-match t)
   :history hist
   :may-modify-candidates t
   :minibuffer-completion-table collection
   :minibuffer-completion-predicate predicate))

(defvar selectrum--old-completing-read-function nil
  "Previous value of `completing-read-function'.")

;;;###autoload
(defun selectrum-completing-read-multiple
    (prompt table &optional predicate require-match initial-input
            hist def _inherit-input-method)
  "Read one or more choices using Selectrum.
Replaces `completing-read-multiple'. For PROMPT, TABLE,
PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD, see `completing-read-multiple'.

The option `selectrum-completing-read-multiple-show-help' can be
used to control insertion of additional usage information into
the prompt."
  (let* ((crm-completion-table table)
         (crm-separator crm-separator)
         (coll (all-completions "" #'crm--collection-fn predicate))
         (candidates
          (lambda (input)
            (let ((beg 0)
                  (inputs ()))
              (while (string-match crm-separator input beg)
                (push (substring input beg (match-beginning 0))
                      inputs)
                (setq beg (match-end 0)))
              (let ((coll (cl-delete-if
                           (lambda (i)
                             (member i inputs))
                           (copy-sequence coll)))
                    (ninput (substring input beg)))
                `((input . ,ninput)
                  (candidates . ,coll))))))
         (res nil))
    (setq
     res
     (minibuffer-with-setup-hook
         (lambda ()
           (setq-local selectrum--crm-p t)
           (when selectrum-completing-read-multiple-show-help
             (let ((inhibit-read-only t))
               (save-excursion
                 (goto-char (minibuffer-prompt-end))
                 (when (search-backward ":" nil t)
                   (insert
                    (apply
                     #'propertize
                     (format " [add more using %s%s]"
                             (substitute-command-keys
                              "\\[selectrum-insert-current-candidate]")
                             (if (assoc crm-separator
                                        selectrum--crm-separator-alist)
                                 ;; Separator will be automatically
                                 ;; inserted.
                                 ""
                               "and crm-separator"))
                     (text-properties-at (point)))))))))
       (selectrum-read
        prompt
        candidates
        :require-match require-match
        :initial-input initial-input
        :history hist
        :default-candidate def
        :may-modify-candidates t
        :minibuffer-completion-table table
        :minibuffer-completion-predicate predicate)))
    (split-string res crm-separator t)))

;;;###autoload
(defun selectrum-completion-in-region
    (start end collection predicate)
  "Complete in-buffer text using a list of candidates.
Can be used as `completion-in-region-function'. For START, END,
COLLECTION, and PREDICATE, see `completion-in-region'."
  (let* ((input (buffer-substring-no-properties start end))
         (meta (completion-metadata input collection predicate))
         (category (completion-metadata-get meta 'category))
         (bound (pcase category
                  ('file start)
                  (_ (+ start (car (completion-boundaries
                                    input collection predicate ""))))))
         (exit-func (plist-get completion-extra-properties
                               :exit-function))
         (cands (nconc
                 ;; `completion-styles' is used for the initial
                 ;; filtering here internally! Selectrum doesn't use
                 ;; `completion-styles' in other places yet. For
                 ;; completion in region this matches the expected
                 ;; behavior because the candidates should be
                 ;; determined according to the sourrounding text
                 ;; that gets completed for which
                 ;; `completion-styles' is typically configured.
                 (completion-all-completions input collection predicate
                                             (- end start) meta)
                 nil))
         ;; See doc of `completion-extra-properties'.
         (exit-status nil)
         (result nil))
    (if (null cands)
        (progn (unless completion-fail-discreetly (ding))
               (message "No match"))
      (pcase category
        ('file
         (setq result
               (selectrum--completing-read-file-name
                "Completion: " collection predicate
                nil input)
               exit-status 'finished))
        (_
         (setq result
               (if (not (cdr cands))
                   (car cands)
                 (selectrum-completing-read
                  "Completion: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        meta
                      (complete-with-action action cands string pred)))))
               exit-status (cond ((not (member result cands)) 'sole)
                                 (t 'finished)))))
      (delete-region bound end)
      (insert (substring-no-properties result))
      (when exit-func
        (funcall exit-func result exit-status)))))

(defvar selectrum--old-completion-in-region-function nil
  "Previous value of `completion-in-region-function'.")

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
         (buffalist (mapcar (lambda (buf)
                              (cons (buffer-name buf) buf))
                            (buffer-list)))
         (buffers (mapcar #'car (if predicate
                                    (cl-delete-if-not predicate buffalist)
                                  buffalist)))
         (candidates
          (lambda (input)
            (let ((candidates (copy-sequence buffers)))
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
              `((candidates . ,candidates)
                (input . ,input))))))
    (selectrum-read
     prompt candidates
     :default-candidate def
     :require-match (eq require-match t)
     :history 'buffer-name-history
     :no-move-default-candidate t
     :may-modify-candidates t
     :minibuffer-completion-table #'internal-complete-buffer
     :minibuffer-completion-predicate predicate)))

(defvar selectrum--old-read-buffer-function nil
  "Previous value of `read-buffer-function'.")

(defun selectrum--completing-read-file-name
    (prompt collection &optional
            predicate require-match initial-input
            hist def _inherit-input-method)
  "Selectrums completing read function for `read-file-name-default'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
            HIST, DEF, _INHERIT-INPUT-METHOD see `completing-read'."
  (let ((coll
         (lambda (input)
           (let* (;; Full path of input dir (might include shadowed parts).
                  (dir (or (file-name-directory input) ""))
                  ;; The input used for matching current dir entries.
                  (ematch (file-name-nondirectory input))
                  ;; Adjust original collection for Selectrum.
                  (cands
                   (selectrum--map-destructive
                    (lambda (i)
                      (when (string-suffix-p "/" i)
                        (setq i (substring i 0 (1- (length i))))
                        (put-text-property
                         0 (length i)
                         'selectrum-candidate-display-suffix
                         "/"
                         i))
                      i)
                    (condition-case _
                        (funcall collection dir
                                 (lambda (i)
                                   (when (and (or (not predicate)
                                                  (funcall predicate i))
                                              (not (member
                                                    i '("./" "../"))))
                                     (prog1 t
                                       (add-text-properties
                                        0 (length i)
                                        `(selectrum-candidate-full
                                          ,(concat dir i))
                                        i))))
                                 t)
                      ;; May happen in case user quits out
                      ;; of a TRAMP prompt.
                      (quit)))))
             `((input . ,ematch)
               (candidates . ,cands))))))
    (selectrum-read
     prompt coll
     :default-candidate (or (car-safe def) def)
     :initial-input (or (car-safe initial-input) initial-input)
     :history hist
     :require-match (eq require-match t)
     :may-modify-candidates t
     :minibuffer-completion-table collection
     :minibuffer-completion-predicate predicate)))

;;;###autoload
(defun selectrum-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'."
  (minibuffer-with-setup-hook
      (:append (lambda ()
                 (when (and default-filename
                            ;; ./ should be omitted.
                            (not (equal
                                  (expand-file-name default-filename)
                                  (expand-file-name default-directory))))
                   (setq selectrum--default-candidate
                         ;; Sort for directories needs any final
                         ;; slash removed.
                         (directory-file-name
                          ;; The candidate should be sorted by it's
                          ;; relative name.
                          (file-relative-name default-filename
                                              default-directory))))
                 (set-syntax-table
                  selectrum--minibuffer-local-filename-syntax)))
    (let* ((crf completing-read-function)
           ;; See <https://github.com/raxod502/selectrum/issues/61>.
           ;; When you invoke another `completing-read' command
           ;; recursively then it inherits the
           ;; `completing-read-function' binding, and unless it's
           ;; another file reading command using
           ;; `selectrum--completing-read-file-name' this will cause
           ;; an error. To circumvent this we use the function to
           ;; reset the variable when called.
           (completing-read-function
            (lambda (&rest args)
              (setq completing-read-function crf)
              (apply #'selectrum--completing-read-file-name args))))
      (read-file-name-default
       prompt dir
       ;; We don't pass default-candidate here to avoid that
       ;; submitting the selected prompt results in the default file
       ;; name. This is the stock Emacs behavior where there is no
       ;; concept of an active selection. Instead we pass the initial
       ;; prompt as default so it gets returned when submitted. In
       ;; addition to that we set `selectrum--default-candidate' in
       ;; the setup hook above so the actual default gets sorted to
       ;; the top. This should give the same convenience as in default
       ;; completion (where you can press RET at the initial prompt to
       ;; get the default). The downside is that this convenience is
       ;; gone when sorting is disabled or the default-filename is
       ;; outside the prompting directory but this should be rare
       ;; case.
       (concat
        (expand-file-name
         (or dir
             default-directory))
        initial)
       mustmatch initial predicate))))

(defvar selectrum--old-read-file-name-function nil
  "Previous value of `read-file-name-function'.")

;;;###autoload
(defun selectrum--fix-dired-read-dir-and-switches (func &rest args)
  "Make \\[dired] do the \"right thing\" with its default candidate.
By default \\[dired] uses `read-file-name' internally, which
causes Selectrum to provide you with the first file inside the
working directory as the default candidate. However, it would
arguably be more semantically appropriate to use
`read-directory-name', and this is especially important for
Selectrum since this causes it to select the working directory
initially.

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
                                   (file-name-base
                                    (file-name-sans-extension path))
                                   'selectrum-candidate-display-prefix
                                   (file-name-directory
                                    (file-name-sans-extension
                                     (selectrum--trailing-components
                                      num-components path)))
                                   'fixedcase 'literal
                                   'selectrum--lib-path path))
                                paths)))
                   (setq lst (nconc candidate-paths lst)))
                 (cl-return)))
             (cl-incf num-components)))))
     table)
    (get-text-property
     0 'selectrum--lib-path
     (selectrum-read
      "Library name: " lst :require-match t :may-modify-candidates t))))

(defun selectrum-repeat ()
  "Repeat the last command that used Selectrum, and try to restore state."
  (interactive)
  (unless selectrum--last-command
    (user-error "No Selectrum command has been run yet"))
  (let ((selectrum--repeat t))
    (setq current-prefix-arg selectrum--last-prefix-arg)
    (call-interactively selectrum--last-command)))

;;;###autoload
(defun selectrum--fix-minibuffer-message (func &rest args)
  "Ensure the cursor stays at the front of the minibuffer message.
This advice adjusts where the cursor gets placed for the overlay
of `minibuffer-message' and ensures the overlay gets displayed at
the right place without blocking the display of candidates.

To test that this advice is working correctly, type \\[find-file]
twice in a row with `enable-recursive-minibuffers' set to nil.
The overlay indicating that recursive minibuffers are not allowed
should appear right after the user input area, not at the end of
the candidate list and the cursor should stay at the front.

This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS are standard as in all `:around' advice."
  (if (bound-and-true-p selectrum-active-p)
      ;; Delay execution so candidates get displayed first.
      (run-at-time
       0 nil
       (lambda ()
         (cl-letf* ((orig-put-text-property
                     (symbol-function #'put-text-property))
                    ((symbol-function #'put-text-property)
                     (lambda (beg end key val &rest args)
                       ;; Set cursor property like
                       ;; `set-minibuffer-message' in Emacs 27.
                       (apply orig-put-text-property
                              beg end key (if (eq key 'cursor) 1 val)
                              args)))
                    (orig-make-overlay
                     (symbol-function #'make-overlay))
                    ((symbol-function #'make-overlay)
                     (lambda (&rest args)
                       (let ((ov (apply orig-make-overlay args)))
                         ;; Set overlay priority like
                         ;; `set-minibuffer-message' in Emacs 27.
                         (overlay-put ov 'priority 1100)
                         ov))))
           (apply func args))))
    (apply func args)))

;; You may ask why we copy the entire minor-mode definition into the
;; autoloads file, and autoload several private functions as well.
;; This is because enabling `selectrum-mode' does not actually require
;; any of the code in Selectrum. So, to improve startup time, we avoid
;; loading Selectrum when enabling `selectrum-mode'.

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
          (setq selectrum--old-completion-in-region-function
                (default-value 'completion-in-region-function))
          (setq-default completion-in-region-function
                        #'selectrum-completion-in-region)
          (advice-add #'completing-read-multiple :override
                      #'selectrum-completing-read-multiple)
          ;; No sharp quote because Dired may not be loaded yet.
          (advice-add 'dired-read-dir-and-switches :around
                      #'selectrum--fix-dired-read-dir-and-switches)
          ;; No sharp quote because `read-library-name' is not defined
          ;; in older Emacs versions.
          (advice-add 'read-library-name :override
                      #'selectrum-read-library-name)
          (advice-add #'minibuffer-message :around
                      #'selectrum--fix-minibuffer-message)
          (define-key minibuffer-local-map
            [remap previous-matching-history-element]
            'selectrum-select-from-history))
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
      (when (equal (default-value 'completion-in-region-function)
                   #'selectrum-completion-in-region)
        (setq-default completion-in-region-function
                      selectrum--old-completion-in-region-function))
      (advice-remove #'completing-read-multiple
                     #'selectrum-completing-read-multiple)
      ;; No sharp quote because Dired may not be loaded yet.
      (advice-remove 'dired-read-dir-and-switches
                     #'selectrum--fix-dired-read-dir-and-switches)
      ;; No sharp quote because `read-library-name' is not defined in
      ;; older Emacs versions.
      (advice-remove 'read-library-name #'selectrum-read-library-name)
      (advice-remove #'minibuffer-message #'selectrum--fix-minibuffer-message)
      (when (eq (lookup-key minibuffer-local-map
                            [remap previous-matching-history-element])
                #'selectrum-select-from-history)
        (define-key minibuffer-local-map
          [remap previous-matching-history-element] nil)))))

;;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; sentence-end-double-space: nil
;; End:

;;; selectrum.el ends here
