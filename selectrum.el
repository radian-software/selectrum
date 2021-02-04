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
  "Face used to display annotations of completion tables."
  :group 'selectrum-faces)

(defface selectrum-completion-docsig
  '((t :inherit selectrum-completion-annotation :slant italic))
  "Face used to display docsigs of completion tables."
  :group 'selectrum-faces)

;;;; Variables

(defvar selectrum-should-sort-p t
  "Non-nil if preprocessing and refinement functions should sort.
This is let-bound to nil in some contexts, and should be
respected by user functions for optimal results.")

(defvar selectrum--minibuffer-default-in-prompt-regexps
  (let ((minibuffer-eldef-shorten-default nil))
    (minibuffer-default--in-prompt-regexps))
  "Regexps for determining if the prompt message includes the default value.
See `minibuffer-default-in-prompt-regexps', from which this is derived.")

;;;; User options

(defgroup selectrum nil
  "Simple incremental narrowing framework with sane API."
  :group 'convenience
  :prefix "selectrum-"
  :link '(url-link "https://github.com/raxod502/selectrum"))

(defcustom selectrum-max-window-height 10
  "Maximal window height to expand to.
The display window or minibuffer window will expand up to this
height when it is to small to show the candidates. If this option
is nil it defaults to `max-mini-window-height'. See its docstring
for further information of possible values."
  :type 'number)

(defcustom selectrum-num-candidates-displayed 'auto
  "Configures how many candidates are displayed.
When `auto' the appropriate number will be determined
automatically according to the available space of the displaying
window and the height allowed by `selectrum-max-window-height'.
To configure a constant height for vertical display see
`selectrum-fix-vertical-window-height'."
  :type '(choice (const :tag "Automatic" auto) integer))

(defcustom selectrum-fix-vertical-window-height nil
  "Configure a fixed window height for vertical display.
If candidates are displayed vertically and this option is non-nil
the height will be determined by `selectrum-max-window-height'."
  :type 'boolean)

(defun selectrum-display-full-frame (buf _alist)
  "Display BUF in full frame.
Can be used as `selectrum-display-action' to display candidates
in a single window spanning the current frame:

    (setq selectrum-display-action
        \\='(selectrum-display-full-frame)."
  (delete-other-windows)
  (set-window-buffer (selected-window) buf)
  (selected-window))

(defcustom selectrum-display-action nil
  "Display action to show the candidates buffer.

If this is nil the candidates are shown in the minibuffer.
Otherwise the candidates are shown in the window as determined
from the display action. Note that if you specify a window height
lower than `selectrum-max-window-height' the window will be
resized if needed to display that number of candidates.

For the format see the ACTION argument of `display-buffer'. For
example to display candidates in some available window use:

    \\='(display-buffer-use-some-window)

Or to display them in a bottom side window:

   \\='(display-buffer-in-side-window
       (side . bottom)
       (slot . -1))

Display buffer actions can also spawn a separate frame where
candidates can be displayed. To display candidates in the current
frame you can use the provided action function
`selectrum-display-full-frame'."
  :type '(cons (choice function (repeat :tag "Functions" function))
               alist))

(defcustom selectrum-display-style
  '(vertical)
  "Current display style for candidates.
The car is a symbol of the current display style. Currently
available styles are `vertical' and `horizontal'. The cdr is a
plist of settings. Currently there are only settings for the
`horizontal' style:

`:prompt-separator' for the string to display after the prompt if
the candidates are displayed in the minibuffer,
`:before-candidates' for the string to insert before the
candidate listing, `:candidates-separator' for the string to
insert between candidates, `:more-candidates' for the string to
indicate that more candidates are following after the currently
displayed ones and `:after-candidates' for a string to display
after the displayed candidates."
  :type 'list)

(defcustom selectrum-display-style-cycle-list
  '((vertical)
    (horizontal))
  "List of `selectrum-display-style' styles.
Use `selectrum-cycle-display-style' to cycle through these."
  :type 'list)

(defun selectrum-refine-candidates-using-completions-styles (input candidates)
  "Use INPUT to filter and highlight CANDIDATES.
Uses `completion-styles'."
  (nconc
   (completion-all-completions
    input candidates nil (length input)
    (completion-metadata input
                         minibuffer-completion-table
                         minibuffer-completion-predicate))
   nil))

(defcustom selectrum-refine-candidates-function
  #'selectrum-refine-candidates-using-completions-styles
  "Function used to decide which candidates should be displayed.
The function receives two arguments, the user input (a string)
and the list of candidates (strings). Returns a new list of
candidates. Should not modify the input list. The returned list
may be modified by Selectrum, so a copy of the input should be
made. (Beware that `cl-remove-if' doesn't make a copy if there's
nothing to remove.)"
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

(defcustom selectrum-completion-in-region-styles
  '(basic partial-completion emacs22)
  "The `completion-styles' used by `selectrum-completion-in-region'.
These are used for the initial filtering of candidates according
to the text around point. The initial filtering styles for
completion in region might generally differ from the styles you
want to use for usual completion. If this option is nil the
candidates will be filtered by `all-completions'."
  :type 'completion--styles-type)

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

(defun selectrum-candidates-identity (_input candidates)
  "Return CANDIDATES unchanged."
  candidates)

(defcustom selectrum-highlight-candidates-function
  #'selectrum-candidates-identity
  "Function used to highlight matched candidates for display.
The function receives two arguments, the input string and the
list of candidates (strings) that are going to be displayed.
Return a list of propertized candidates. Do not modify the input
list or strings."
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
    (define-key map (kbd "M-q") 'selectrum-cycle-display-style)
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
  '((match      ":"  success)
    (line-count "%d lines" success)
    (newline    "\\n" warning)
    (truncation "..." shadow)
    (whitespace " "  shadow))
  "Settings used to configure the formatting of multi-line candidates.

Currently, multi-line candidates are flattened, stripped of
repeated whitespace, and, if need be, truncated. The first line
is displayed truncated followed by a line count and trunctated
matches. This option configures how the formatting is done.

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
   - `match' determines the string to insert between the first
   line and the matched lines.
   - `line-count' determines the string for displaying the line count.
2. A string to indicate the display change. For `line-count' it should
   be a format string for a decimal or the empty string for no display.
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
                                      whitespace)
                               (const :tag "Line count"
                                      line-count))
                       (string :tag "Indicator string")
                       (face :tag "Indicator face"))))

(defcustom selectrum-extend-current-candidate-highlight nil
  "Whether to extend highlighting of the current candidate until the margin.

Nil (the default) means to only highlight the displayed text."
  :type 'boolean)

;;;###autoload
(defcustom selectrum-complete-in-buffer t
  "If non-nil, use Selectrum for `completion-in-region'.
This option needs to be set before activating `selectrum-mode'."
  :type 'boolean
  :group 'selectrum)

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

(defmacro selectrum--minibuffer-with-setup-hook (fun &rest body)
  "Variant of `minibuffer-with-setup-hook' using a symbol and `fset'.
This macro is only needed to prevent memory leaking issues with
the upstream `minibuffer-with-setup-hook' macro. FUN is the hook
function and BODY opens the minibuffer."
  ;; Copied from https://github.com/minad/consult/commit/27e055e.
  (declare (indent 1) (debug t))
  (let ((hook (make-symbol "hook"))
        (append))
    (when (eq (car-safe fun) :append)
      (setq append '(t) fun (cadr fun)))
    `(let ((,hook (make-symbol "selectrum--minibuffer-setup")))
       (fset ,hook (lambda ()
                     (remove-hook 'minibuffer-setup-hook ,hook)
                     (funcall ,fun)))
       (unwind-protect
           (progn
             (add-hook 'minibuffer-setup-hook ,hook ,@append)
             ,@body)
         (remove-hook 'minibuffer-setup-hook ,hook)))))

;;;; Minibuffer state

(defvar-local selectrum--last-buffer nil
  "The buffer that was current before the active session")

(defvar selectrum--candidates-overlay nil
  "Overlay used to display current candidates.")

(defvar selectrum--dynamic-candidates nil
  "The dynamic candidate function passed to `selectrum-read'.
When set the dynamic candidate function is called on each input
change. The results are subsequently preprocessed by
`selectrum-preprocess-candidates-function' and saved as
`selectrum--preprocessed-candidates'. See `selectrum-read' for
more details on function collections.")

(defvar selectrum--preprocessed-candidates nil
  "Preprocessed list of candidates.
This list contains the candidates of the current session after
preprocessing them with
`selectrum-preprocess-candidates-function'. The list is
subsequently passed to `selectrum-refine-candidates-function'.
For the refined candidates see `selectrum--refined-candidates'.")

(defvar selectrum--refined-candidates nil
  "Refined list of candidates to be displayed.
This is derived from `selectrum--preprocessed-candidates' by
`selectrum-refine-candidates-function' every time the user input
changes, and is subsequently passed to
`selectrum-highlight-candidates-function'.")

(defvar selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.")

(defvar-local selectrum--first-index-displayed nil
  "Index of the first displayed candidate.")

(defvar-local selectrum--actual-num-candidates-displayed nil
  "The actual number of candidates displayed.")

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

(defvar selectrum--candidates-buffer " *selectrum*"
  "Buffer to display candidates using `selectrum-display-action'.")

(defvar-local selectrum--virtual-default-file nil
  "If set used as a virtual file to prompt with.")

;;;;; Minibuffer state utility functions

(defun selectrum--normalize-collection (collection &optional predicate)
  "Normalize COLLECTION into a list of strings.
COLLECTION may be a list of strings or symbols or cons cells, an
obarray, a hash table, or a function, as per the docstring of
`all-completions'. The returned list may be mutated without
damaging the original COLLECTION.

If PREDICATE is non-nil, then it filters the collection as in
`all-completions'."
  ;; Making the last buffer current avoids the cost of potential
  ;; buffer switching for each candidate within the predicate (see
  ;; `describe-variable').
  (with-current-buffer (if (eq collection 'help--symbol-completion-table)
                           selectrum--last-buffer
                         (current-buffer))
    (let ((completion-regexp-list nil))
      (all-completions "" collection predicate))))

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

(defun selectrum-get-current-candidate (&optional notfull)
  "Return currently selected Selectrum candidate if there is one.
If NOTFULL is non-nil don't use canonical representation of
candidate and return the candidate as displayed."
  (when (and selectrum-active-p
             selectrum--current-candidate-index
             (or selectrum--refined-candidates
                 (< selectrum--current-candidate-index 0)))
    (if notfull
        (selectrum--get-candidate
         selectrum--current-candidate-index)
      (selectrum--get-full
       (selectrum--get-candidate
        selectrum--current-candidate-index)))))

(defun selectrum-get-current-candidates (&optional notfull)
  "Get list of current Selectrum candidates.
If NOTFULL is non-nil don't use canonical representation of
candidate and return the candidate as displayed."
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
  "Get full form of CANDIDATE."
  (or (get-text-property 0 'selectrum--candidate-full candidate)
      (when minibuffer-completing-file-name
        (if (and selectrum--current-candidate-index
                 (< selectrum--current-candidate-index 0))
            candidate
          (let* ((input (minibuffer-contents))
                 (path (substitute-in-file-name input))
                 (dirlen (length (file-name-directory path)))
                 (prefixlen (car (completion--sifn-requote dirlen input)))
                 (prefix (substring input 0 prefixlen)))
            (concat prefix candidate))))
      candidate))

(defun selectrum--get-candidate (index)
  "Get candidate at given INDEX. Negative means get the current user input."
  (if (and index (>= index 0))
      (nth index selectrum--refined-candidates)
    (buffer-substring-no-properties
     (minibuffer-prompt-end)
     (point-max))))

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

(defun selectrum-exhibit (&optional keep-selection)
  "Trigger an update of Selectrum's completion UI.
If KEEP-SELECTION is non-nil keep the current candidate selected
when possible (it is still a member of the candidate set)."
  (when-let ((mini (active-minibuffer-window)))
    (with-selected-window mini
      (when (and minibuffer-completion-table
                 (not selectrum--dynamic-candidates))
        (setq selectrum--preprocessed-candidates nil))
      (setq selectrum--previous-input-string nil)
      (selectrum--update
       (and keep-selection
            (selectrum-get-current-candidate))))))

;;;; Hook functions

(defun selectrum--count-info ()
  "Return a string of count information to be prepended to prompt."
  (let ((total (length selectrum--refined-candidates))
        (current (1+ (or selectrum--current-candidate-index -1))))
    (pcase selectrum-count-style
      ('matches         (format "%-4d " total))
      ('current/matches (format "%-6s " (format "%d/%d" current total)))
      (_                ""))))

(defvar display-line-numbers) ; Undefined in Emacs 25.
(defun selectrum--get-display-window ()
  "Get candidate display window.

Window will be created by `selectrum-display-action'."
  (let ((buf (or (get-buffer selectrum--candidates-buffer)
                 (with-current-buffer
                     (get-buffer-create selectrum--candidates-buffer)
                   (setq cursor-type nil)
                   (setq-local cursor-in-non-selected-windows nil)
                   (setq display-line-numbers nil)
                   (setq buffer-undo-list t)
                   (setq buffer-read-only t)
                   (setq show-trailing-whitespace nil)
                   (goto-char (point-min))
                   (current-buffer))))
        (action selectrum-display-action))
    (or (get-buffer-window buf 'visible)
        (with-selected-window (minibuffer-selected-window)
          (let* ((frame (selected-frame))
                 (window (display-buffer buf action)))
            (select-frame-set-input-focus frame)
            window)))))

(defun selectrum--expand-window-for-content-p (window)
  "Return non-nil if WINDOW should be expanded.
This is the case when the height of WINDOW fits in the range as
determined by `selectrum--max-num-candidate-lines' and the
content height is greater than the window height."
  (and (<= (window-body-height window)
           (selectrum--max-num-candidate-lines window))
       (>= (cdr (window-text-pixel-size window))
           (window-body-height window 'pixelwise))))

(defun selectrum--vertical-display-style
    (win cb nrows ncols
         &optional index max-index first-index-displayed last-index-displayed
         max-num
         settings)
  "Insert candidates vertically into current buffer.
Used as insertion function for `vertical' display style, see
`selectrum-display-style'. WIN is the window where buffer will
get displayed in. Callback CB returns the candidates to be
inserted. The callback has four arguments, the index position and
the number of candidates and optionally the third argument which
allows passing and annotation function. If given the function
receives three optional arguments: a prefix, suffix and a right
margin annotation of the currently selected candidate and should
take care of displaying them. The annotations display of others
candidates than the current is disabled in this case. The
optional forth argument of the callback should be non-nil if
candidates are supposed to be displayed horizontally. NROWS is
the number of lines available and NCOLS the number of available
columns. If there are candidates INDEX is the index of the
currently selected candidate and MAX-INDEX is the index of the
maximal index of the collection. When candidates are already
displayed FIRST-INDEX-DISPLAYED is the index of the candidate
that is displayed first and LAST-INDEX-DISPLAYED the index of the
last one and MAX-NUM if given specifies the maximal number of
candidates to be displayed, the callback won't return more
candidates than that anyway but the number can be useful if the
insertion function behaviour depends on the number of candidates
that get displayed. SETTINGS are a plist of additional settings
as specified in `selectrum-display-style', this function
currently doesn't have any."
  (ignore ncols first-index-displayed last-index-displayed settings)
  (let* ((rows (or max-num nrows))
         (first-index-displayed
          (if (not index)
              0
            (selectrum--clamp
             ;; Adding one here makes it look slightly better, as
             ;; there are guaranteed to be more candidates shown
             ;; below the selection than above.
             (1+ (- index (max 1 (/ rows 2))))
             0
             (max (- (1+ max-index) rows)
                  0))))
         (displayed-candidates
          (funcall cb first-index-displayed rows)))
    (when (window-minibuffer-p win)
      (insert "\n"))
    (let ((n 0))
      (dolist (cand displayed-candidates)
        (cl-incf n)
        (insert cand "\n"))
      n)))

(defun selectrum--horizontal-display-style
    (win cb nrows ncols
         &optional index max-index first-index-displayed last-index-displayed
         max-num
         settings)
  "Insert candidates horizontally into buffer BUF.
For BUF, WIN, CB, NROWS, NCOLS, INDEX, MAX-INDEX,
FIRST-INDEX-DISPLAYED, LAST-INDEX-DISPLAYED, MAX-NUM and SETTINGS
see `selectrum--vertical-display-style'. For known keys see
the `horizontal' description of `selectrum-display-style'."
  (ignore nrows max-num)
  (let* ((before-cands (or (plist-get settings :before-candidates)
                           "{"))
         (prompt-sep (if (window-minibuffer-p win)
                         (or (plist-get settings :prompt-separator)
                             "")
                       ""))
         (start (concat prompt-sep before-cands))
         (end (or (plist-get settings :after-candidates)
                  "}"))
         (separator (or (plist-get settings :candidates-separator)
                        " | "))
         (more (or (plist-get settings :more-candidates)
                   (propertize "..." 'face 'shadow)))
         (first-index-displayed
          (cond ((or (not index)
                     (not first-index-displayed)
                     (not last-index-displayed))
                 0)
                ((> index last-index-displayed)
                 (if (= index max-index)
                     max-index
                   (1+ last-index-displayed)))
                ((< index first-index-displayed)
                 index)
                (t
                 first-index-displayed)))
         (cands
          (funcall cb first-index-displayed
                   (floor ncols (1+ (length separator)))
                   #'ignore 'horizontal))
         (n 0)
         (insert nil))
    (when cands
      (let ((ncols ncols))
        (while (and cands (> ncols 0))
          (let ((cand (pop cands)))
            (when (zerop n)
              (setq ncols (- ncols (length start)))
              (push start insert))
            (setq ncols (- ncols (length cand) (length separator)))
            (when (or (zerop n)
                      (>= ncols 0))
              (put-text-property 0 (length cand) 'cand t cand)
              (push cand insert)
              (push separator insert)
              (cl-incf n)))))
      (if (= max-index (1- (+ first-index-displayed n)))
          (progn
            (pop insert)
            (push end insert))
        (while (and insert
                    (not (= n 1))
                    (or (not (equal (car insert) separator))
                        (>= (+ (length (apply #'concat insert))
                               (length more)
                               (length end))
                            ncols)))
          (when (get-text-property 0 'cand (car insert))
            (cl-decf n))
          (pop insert))
        (push more insert)
        (push end insert))
      (setq insert (nreverse insert))
      (while insert
        (insert (pop insert))))
    n))

(defun selectrum-cycle-display-style ()
  "Change current `selectrum-display-style'.
Cycles from current style through styles listed in
`selectrum-display-style-cycle-list'. With an active minibuffer
the display style is only changed for the current session.
Without that the global default value will be changed."
  (interactive)
  (let* ((miniw (active-minibuffer-window))
         (buf (if miniw
                  (window-buffer miniw)
                (current-buffer))))
    (with-current-buffer buf
      (when miniw
        (make-local-variable 'selectrum-display-style-cycle-list)
        (make-local-variable 'selectrum-display-style))
      (unless (eq last-command 'selectrum-cycle-display-style)
        (setq selectrum-display-style-cycle-list
              (cons selectrum-display-style
                    (delete selectrum-display-style
                            selectrum-display-style-cycle-list))))
      (setq selectrum-display-style-cycle-list
            (append (cdr selectrum-display-style-cycle-list)
                    (list (car selectrum-display-style-cycle-list))))
      (setq selectrum-display-style
            (car selectrum-display-style-cycle-list))
      (unless miniw
        (message "Switched to %s" selectrum-display-style)))))

(defun selectrum--insert-candidates
    (insert-settings candidates buf win input plen
                     &optional index mindex findex num)
  "Use INSERT-SETTINGS to insert CANDIDATES into BUF for display.
BUF is supposed to be displayed in window WIN. INPUT is the
current user input. PLEN is the prompt prefix length. INDEX
is the index of the currently selected candidate if any. MINDEX
is the maximum and FINDEX the first index. NUM is the number of
currently displayed candidates. Returns a cons: The car is
non-nil if candidates are supposed to be displayed horizontally
and the cdr is the number of candidates that were inserted."
  (let* ((horizp  nil)
         (nlines (selectrum--max-num-candidate-lines win))
         (ncols (if (window-minibuffer-p win)
                    (- (window-body-width win)
                       (- (point-max)
                          (window-hscroll win))
                       plen)
                  (window-body-width win)))
         (ncands (when (numberp selectrum-num-candidates-displayed)
                   selectrum-num-candidates-displayed))
         (insert-variant (car insert-settings))
         (insert-fun (cond ((eq insert-variant 'horizontal)
                            #'selectrum--horizontal-display-style)
                           (t
                            #'selectrum--vertical-display-style)))
         (settings
          (cdr insert-settings))
         (cb (lambda (first-index-displayed
                      ncands &optional annot-fun horizontalp)
               (with-current-buffer (window-buffer (active-minibuffer-window))
                 (setq-local selectrum--first-index-displayed
                             first-index-displayed)
                 (setq horizp horizontalp)
                 (selectrum--candidates-display-strings
                  (funcall
                   selectrum-highlight-candidates-function
                   input
                   (seq-take
                    (nthcdr
                     first-index-displayed
                     candidates)
                    ;; Never allow more candidates than configured.
                    (if (numberp selectrum-num-candidates-displayed)
                        selectrum-num-candidates-displayed
                      ncands)))
                  (when (and first-index-displayed index)
                    (- index first-index-displayed))
                  annot-fun horizontalp))))
         (lindex (when (and findex num)
                   (+ findex
                      (max 0 (1- num)))))
         (n (with-current-buffer buf
              (funcall insert-fun win cb
                       nlines ncols index mindex findex lindex
                       ncands settings))))
    (cons horizp
          (if (or (not index) (not findex)
                  (>= (+ findex n) index))
              n
            ;; When the insertion function was switched the current index
            ;; might be out of sight in this case reinsert with the current
            ;; index displayed as the first one.
            (with-current-buffer buf
              (erase-buffer)
              (funcall insert-fun win cb
                       nlines ncols index mindex index lindex
                       ncands settings))))))

(defun selectrum--at-existing-prompt-path-p ()
  "Return non-nil when current file prompt exists."
  (and minibuffer-completing-file-name
       (file-exists-p
        (substitute-in-file-name (minibuffer-contents)))))

(defun selectrum--minibuffer-post-command-hook ()
  "Update minibuffer in response to user input."
  (selectrum--update))

(defun selectrum--max-window-height (&optional frame max)
  "Return maximal window height for frame.
The height is determined by the `frame-height' of FRAME which
defaults to the current one and MAX which defaults to
`selectrum-max-window-height' and falls back to
`max-mini-window-height' if the former is unset."
  (let* ((max (or max
                  selectrum-max-window-height
                  max-mini-window-height
                  0))
         (fh (frame-height
              (or frame
                  (window-frame (minibuffer-selected-window))))))
    (if (floatp max)
        (round (* fh max))
      max)))

(defun selectrum--max-num-candidate-lines (window)
  "Return maximum number of lines to use for display in WINDOW."
  (let ((n (selectrum--max-window-height)))
    (if selectrum-display-action
        (max (window-body-height window) n)
      n)))

(defun selectrum--update (&optional keep-selected)
  "Update state.
KEEP-SELECTED can be a candidate which should stay selected after
the update."
  (unless selectrum--skip-updates-p
    ;; Stay within input area.
    (goto-char (max (point) (minibuffer-prompt-end)))
    ;; Scroll the minibuffer when current prompt exceeds window width.
    (let* ((width (window-width)))
      (if (< (point) (- width (/ width 3)))
          (set-window-hscroll nil 0)
        (set-window-hscroll nil (- (point) (/ width 3)))))
    ;; For some reason this resets and thus can't be set in setup hook.
    (setq-local truncate-lines t)
    (let ((inhibit-read-only t)
          ;; Don't record undo information while messing with the
          ;; minibuffer, as per
          ;; <https://github.com/raxod502/selectrum/issues/31>.
          (buffer-undo-list t)
          (input (buffer-substring (minibuffer-prompt-end)
                                   (point-max)))
          (keep-mark-active (not deactivate-mark)))
      (unless (equal input selectrum--previous-input-string)
        (setq selectrum--previous-input-string input)
        ;; Reset the persistent input, so that it will be nil if
        ;; there's no special attention needed.
        (setq selectrum--visual-input nil)
        (let ((dynamic (functionp selectrum--dynamic-candidates))
              (init-table (and (not selectrum--preprocessed-candidates)
                               minibuffer-completion-table)))
          ;; Compute `selectrum--preprocessed-candidates' if necessary.
          (when (or dynamic init-table)
            (setq selectrum--preprocessed-candidates
                  (cond (dynamic
                         (let* ((result
                                 ;; Ensure dynamic functions won't
                                 ;; break in post command hook.
                                 (condition-case-unless-debug err
                                     (funcall
                                      selectrum--dynamic-candidates
                                      input)
                                   (error (message (error-message-string err))
                                          nil)))
                                (cands
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
                           (funcall selectrum-preprocess-candidates-function
                                    cands)))
                        (init-table
                         ;; No candidates were passed, initialize them
                         ;; from `minibuffer-completion-table'.
                         (funcall selectrum-preprocess-candidates-function
                                  (selectrum--normalize-collection
                                   minibuffer-completion-table
                                   minibuffer-completion-predicate)))))
            (setq selectrum--total-num-candidates
                  (length selectrum--preprocessed-candidates))))
        ;; Do refinement.
        (let* ((cands selectrum--preprocessed-candidates)
               (completion-styles-alist
                ;; Remap partial-style for file completions
                ;; computed from partial input.
                (if (and cands
                         (get-text-property
                          0 'selectrum--partial (car cands)))
                    (cons '(partial-completion
                            ignore
                            selectrum--completion-pcm-all-completions "")
                          completion-styles-alist)
                  completion-styles-alist)))
          (setq selectrum--refined-candidates
                (funcall selectrum-refine-candidates-function
                         input cands)))
        (when selectrum--virtual-default-file
          (setq selectrum--refined-candidates
                (cons (propertize
                       selectrum--virtual-default-file
                       'face 'shadow)
                      selectrum--refined-candidates))
          (setq-local selectrum--virtual-default-file nil))
        (when (and selectrum--move-default-candidate-p
                   selectrum--default-candidate)
          (setq selectrum--refined-candidates
                (selectrum--move-to-front-destructive
                 selectrum--default-candidate
                 selectrum--refined-candidates)))
        (setq selectrum--refined-candidates
              (selectrum--move-to-front-destructive
               ;; Make sure matching dirnames are sorted first.
               (if (and minibuffer-completing-file-name
                        (member (file-name-as-directory input)
                                selectrum--refined-candidates))
                   (file-name-as-directory input)
                 input)
               selectrum--refined-candidates))
        (setq selectrum--refined-candidates
              (delete "" selectrum--refined-candidates))
        (setq input (or selectrum--visual-input input))
        (setq-local selectrum--first-index-displayed nil)
        (setq-local selectrum--actual-num-candidates-displayed nil)
        (if selectrum--repeat
            (progn
              (setq selectrum--current-candidate-index
                    (and (> (length selectrum--refined-candidates) 0)
                         (min (or selectrum--current-candidate-index 0)
                              (1- (length selectrum--refined-candidates)))))
              (setq selectrum--repeat nil))
          (setq selectrum--current-candidate-index
                (cond
                 ;; Check for candidates needs to be first!
                 ((null selectrum--refined-candidates)
                  (when (or (not selectrum--match-required-p)
                            (selectrum--at-existing-prompt-path-p))
                    -1))
                 (keep-selected
                  (or (cl-position keep-selected
                                   selectrum--refined-candidates
                                   :key #'selectrum--get-full
                                   :test #'equal)
                      0))
                 ((and selectrum--default-candidate
                       (string-empty-p (minibuffer-contents))
                       (not (member selectrum--default-candidate
                                    selectrum--refined-candidates)))
                  -1)
                 ((or (and selectrum--init-p
                           (equal selectrum--default-candidate
                                  (minibuffer-contents)))
                      (and (not (= (minibuffer-prompt-end) (point-max)))
                           (or (and minibuffer-history-position
                                    (not (zerop minibuffer-history-position))
                                    isearch-mode)
                               (memq this-command
                                     '(next-history-element
                                       previous-history-element)))
                           (or (not selectrum--match-required-p)
                               (selectrum--at-existing-prompt-path-p))))
                  -1)
                 (selectrum--move-default-candidate-p
                  0)
                 (t
                  (or (cl-position selectrum--default-candidate
                                   selectrum--refined-candidates
                                   :key #'selectrum--get-full
                                   :test #'equal)
                      0))))))
      ;; Handle prompt selection.
      (if (and selectrum--current-candidate-index
               (< selectrum--current-candidate-index 0))
          (add-text-properties
           (minibuffer-prompt-end) (point-max)
           '(face selectrum-current-candidate))
        (remove-text-properties
         (minibuffer-prompt-end) (point-max)
         '(face selectrum-current-candidate)))
      (let* ((count-info (selectrum--count-info))
             (window (if selectrum-display-action
                         (and selectrum--refined-candidates
                              (selectrum--get-display-window))
                       (active-minibuffer-window)))
             (buffer (with-current-buffer
                         (get-buffer-create selectrum--candidates-buffer)
                       (erase-buffer)
                       (current-buffer)))
             (default
               (when (and (= (minibuffer-prompt-end) (point-max))
                          (or
                           (and selectrum--current-candidate-index
                                (< selectrum--current-candidate-index 0))
                           (and (not selectrum--match-required-p)
                                (not selectrum--refined-candidates))
                           (and selectrum--default-candidate
                                (not minibuffer-completing-file-name)
                                (not (member selectrum--default-candidate
                                             selectrum--refined-candidates)))))
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
                         (propertize "]" 'face 'minibuffer-prompt))))
             (minibuf-after-string (or default " "))
             (inserted-res
              (selectrum--insert-candidates
               selectrum-display-style
               selectrum--refined-candidates
               buffer
               window
               input
               ;; FIXME: This only takes our count overlay into
               ;; account there might be other overlays prefixing the
               ;; prompt.
               (length count-info)
               ;; Exclude selected prompt.
               (when (and selectrum--current-candidate-index
                          (not (< selectrum--current-candidate-index 0)))
                 selectrum--current-candidate-index)
               (1- (length selectrum--refined-candidates))
               selectrum--first-index-displayed
               selectrum--actual-num-candidates-displayed))
             (horizp (car inserted-res))
             (inserted-num (cdr inserted-res)))
        (setq-local selectrum--actual-num-candidates-displayed inserted-num)
        ;; Add padding for scrolled prompt.
        (when (and (window-minibuffer-p window)
                   (not horizp)
                   (not (zerop (window-hscroll window))))
          (let ((padding (make-string (window-hscroll window) ?\s)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (while (not (eobp))
                (insert padding)
                (forward-line 1)))))
        (unless (or selectrum-display-action
                    (zerop selectrum--actual-num-candidates-displayed)
                    (not selectrum--refined-candidates))
          (setq minibuf-after-string
                (concat minibuf-after-string
                        (with-current-buffer buffer
                          (buffer-string)))))
        (move-overlay selectrum--candidates-overlay
                      (point-max) (point-max) (current-buffer))
        (put-text-property 0 1 'cursor t minibuf-after-string)
        (overlay-put selectrum--candidates-overlay
                     'after-string minibuf-after-string)
        (overlay-put selectrum--count-overlay
                     'before-string count-info)
        (overlay-put selectrum--count-overlay
                     'priority 1)
        (when window
          (selectrum--update-window-height
           window (not horizp)))
        (when keep-mark-active
          (setq deactivate-mark nil))
        (setq-local selectrum--init-p nil)))))

(defun selectrum--update-window-height (window vertical)
  "Update window height of WINDOW.
WINDOW is the display window of current candidates and will be
updated to fit its content. If VERTICAL is non-nil the content of
window is supposed to be shown vertically."
  (cond ((frame-root-window-p window))
        ((not vertical)
         (when (or (window-minibuffer-p window)
                   (and (window-at-side-p window 'bottom)
                        (not (window-at-side-p window 'top))))
           (set-window-text-height window 1)))
        ((and (or (bound-and-true-p selectrum-fix-minibuffer-height)
                  selectrum-fix-vertical-window-height)
              vertical)
         (let* ((max (selectrum--max-window-height))
                (height (if selectrum-display-action
                            max
                          ;; Add one for prompt.
                          (1+ max))))
           (set-window-text-height window height)))
        (t
         (when-let ((expand (selectrum--expand-window-for-content-p window)))
           (cond (selectrum-display-action
                  (selectrum--update-display-window-height window))
                 (t
                  (selectrum--update-minibuffer-height window)))))))

(defun selectrum--update-display-window-height (window)
  "Update window height of WINDOW.
Also works for frames if WINDOW is the root window of its frame."
  (let ((window-resize-pixelwise t)
        (window-size-fixed nil)
        (fit-frame-to-buffer 'vertically)
        (fit-window-to-buffer-horizontally nil))
    (fit-window-to-buffer window nil 1)))

(defun selectrum--update-minibuffer-height (window)
  "Update window height of minibuffer WINDOW.
WINDOW will be updated to fit its content vertically."
  (let ((dheight (cdr (window-text-pixel-size window)))
        (wheight (window-pixel-height window))
        (window-resize-pixelwise t))
    (window-resize
     window (- dheight wheight) nil nil 'pixelwise)))

(defun selectrum--ensure-single-lines (candidates settings)
  "Return list of single-line CANDIDATES.

Multi-line candidates are merged into a single line. The
resulting single-line candidates are then shortened by replacing
repeated whitespace and maybe truncating the result.

The specific details of the formatting are determined by
SETTINGS, see `selectrum-multiline-display-settings'."
  (let* ((single/lines ())

         ;; The formatting settings are the same for all multi-line
         ;; candidates, and so only need to be gotten once from
         ;; `settings'.
         ;;
         ;; - Matching lines
         (match/transformation
          (alist-get 'match settings))
         (match/display (car match/transformation))
         (match/face (cadr match/transformation))
         ;; - Truncated candidate
         (truncation/transformation
          (alist-get 'truncation settings))
         (truncation/display (car truncation/transformation))
         (truncation/face (cadr truncation/transformation))
         ;; - Newlines
         (newline/transformation
          (alist-get 'newline settings))
         (newline/display (car newline/transformation))
         (newline/face (cadr newline/transformation))
         ;; - Repeated whitespace
         (whitespace/transformation
          (alist-get 'whitespace settings))
         (whitespace/display (car whitespace/transformation))
         (whitespace/face (cadr whitespace/transformation))
         ;; - Line count
         (nline/info
          (alist-get 'line-count settings))
         (nlines/display (car nline/info))
         (nlines/face (cdr nline/info)))

    (dolist (cand candidates (nreverse single/lines))
      (let ((line
             (if (not (string-match-p "\n" cand))
                 cand
               (let* ((lines (split-string cand "\n"))
                      (len (length lines))
                      (input (minibuffer-contents))
                      (first-line (with-temp-buffer
                                    (insert cand)
                                    (goto-char (point-min))
                                    (skip-chars-forward " \t\n")
                                    (buffer-substring
                                     (line-beginning-position)
                                     (line-end-position))))
                      (matches (delete
                                first-line
                                (if (string-empty-p input)
                                    lines
                                  (funcall
                                   selectrum-highlight-candidates-function
                                   input
                                   (funcall
                                    selectrum-refine-candidates-function
                                    input
                                    lines)))))
                      (nlines (unless (string-empty-p nlines/display)
                                (propertize (format nlines/display len)
                                            'face nlines/face)))
                      (truncated-first-line
                       (replace-regexp-in-string
                        "[ \t][ \t]+"
                        (propertize whitespace/display
                                    'face whitespace/face)
                        first-line 'fixed-case 'literal))
                      (shortened-line
                       (if (< (length truncated-first-line) 78)
                           truncated-first-line
                         (substring truncated-first-line 0 78)))
                      (concated-matches
                       (mapconcat #'identity matches
                                  (propertize newline/display
                                              'face newline/face)))
                      (truncated-matches
                       (replace-regexp-in-string
                        "[ \t][ \t]+"
                        (propertize whitespace/display
                                    'face whitespace/face)
                        concated-matches
                        'fixed-case 'literal))
                      (shortened-matches
                       (if (< (length truncated-matches) 1000)
                           truncated-matches
                         (concat
                          (substring truncated-matches 0 1000)
                          (propertize truncation/display
                                      'face truncation/face))))
                      (line (concat shortened-line
                                    (propertize truncation/display
                                                'face truncation/face)
                                    nlines
                                    (unless (string-empty-p shortened-matches)
                                      (propertize match/display
                                                  'face match/face))
                                    shortened-matches)))
                 line))))
        (push line single/lines)))))

(defun selectrum--annotation (fun cand face)
  "Return annotation for candidate.
Get annotation by calling FUN with CAND and apply FACE to it if
CAND does not have any face property defined."
  (when-let ((str (funcall fun cand)))
    (if (text-property-not-all 0 (length str) 'face nil str)
        str
      (propertize str 'face face))))

(cl-defun selectrum--annotate (cands &key annotf docsigf)
  "Transform CANDS using ANNOTF and DOCSIGF.
ANNOTF results will annotate a candidate with a suffix using
`selectrum-candidate-display-suffix' and
`selectrum-completion-annotation' face unless the annotation
already has a face property. DOCSIGF results will annotate a
candidate with a margin annotation using
`selectrum-candidate-display-suffix' and
`selectrum-completion-docsig' face unless the annotation already
has a face property."
  (let ((res ()))
    (dolist (cand cands (nreverse res))
      (let* ((annot (when annotf
                      (selectrum--annotation
                       annotf
                       cand
                       'selectrum-completion-annotation)))
             (docsig (when docsigf
                       (selectrum--annotation
                        docsigf
                        cand
                        'selectrum-completion-docsig)))
             (new (if (or annot docsig)
                      (apply #'propertize
                             cand
                             `(,@(when annot
                                   (list
                                    'selectrum-candidate-display-suffix
                                    annot))
                               ,@(when docsig
                                   (list
                                    'selectrum-candidate-display-right-margin
                                    docsig))))
                    cand)))
        (push new res)))))

(defun selectrum--display-string (str)
  "Return display string of STR.
Any replacing display specs in STR are included. This avoids
display problems with strings that contain replacing display
specs."
  (let ((len (length str))
        (display "")
        (start 0)
        (end 0))
    (while (not (eq len end))
      (setq end (next-single-property-change start 'display str len))
      (let ((val  (get-text-property start 'display str)))
        (if (and val (stringp val))
            (setq display (concat display val))
          (setq display (concat display (substring str start end)))))
      (setq start end))
    display))

(defun selectrum--add-face (str face)
  "Return copy of STR with FACE added."
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
  (setq str (copy-sequence str))
  (if (version< emacs-version "27")
      (font-lock-prepend-text-property
       0 (length str)
       'face face str)
    (add-face-text-property
     0 (length str)
     face
     'append str))
  str)

(defun selectrum--affixate (fun candidates)
  "Use affixation FUN to transform CANDIDATES.
FUN takes CANDIDATES as argument and returns a list of strings or
a list of list items. In case of a string no annotations are
added and the string is the one to use for completion. In case of
a list the first item is the completion string. If the list has
two items the second one is used as a suffix and if there are
three items the second one is used as a prefix and the third as
suffix."
  (let ((items (funcall fun candidates))
        (res ()))
    (dolist (item items (nreverse res))
      (push (if (stringp item)
                item
              ;; See `completion--insert-strings'.
              (let ((prefix (when (nth 2 item) (nth 1 item)))
                    (suffix (or (nth 2 item) (nth 1 item))))
                (apply #'propertize
                       (nth 0 item)
                       `(,@(when prefix
                             (list 'selectrum-candidate-display-prefix
                                   prefix))
                         ,@(when suffix
                             (list 'selectrum-candidate-display-suffix
                                   suffix))))))
            res))))

(defun selectrum--candidates-display-strings (candidates
                                              highlighted-index
                                              annot-fun
                                              horizontalp
                                              &optional table pred props)
  "Get display strings for CANDIDATES.
HIGHLIGHTED-INDEX is the currently selected index. If ANNOT-FUN
is non-nil don't add any annotations but call the function with
the annotations of the currently highlighted candidate. If
HORIZONTALP is non-nil candidates are supposed to be displayed
horizontally. TABLE defaults to `minibuffer-completion-table'.
PRED defaults to `minibuffer-completion-predicate'. PROPS
defaults to `completion-extra-properties'."
  (let* ((index 0)
         (props (or props completion-extra-properties))
         (annotf (or (selectrum--get-meta 'annotation-function table pred)
                     (plist-get props :annotation-function)))
         (aff (or (selectrum--get-meta 'affixation-function table pred)
                  (plist-get completion-extra-properties
                             :affixation-function)))
         (docsigf (plist-get props :company-docsig))
         (candidates (cond (aff
                            (selectrum--affixate aff candidates))
                           ((or annotf docsigf)
                            (selectrum--annotate candidates
                                                 :annotf annotf
                                                 :docsigf docsigf))
                           (t candidates)))
         (extend (and selectrum-extend-current-candidate-highlight
                      (not horizontalp)))
         (show-indices selectrum-show-indices)
         (margin-padding selectrum-right-margin-padding)
         (lines (selectrum--ensure-single-lines
                 candidates
                 selectrum-multiline-display-settings)))
    (with-temp-buffer
      (dolist (candidate lines)
        (let* ((prefix (get-text-property
                        0 'selectrum-candidate-display-prefix
                        candidate))
               (suffix (get-text-property
                        0 'selectrum-candidate-display-suffix
                        candidate))
               (right-margin (get-text-property
                              0 'selectrum-candidate-display-right-margin
                              candidate))
               (displayed-candidate
                (selectrum--display-string
                 (if annot-fun
                     candidate
                   (concat prefix candidate suffix))))
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
                  (selectrum--add-face
                   displayed-candidate 'selectrum-current-candidate))
            (when annot-fun
              (funcall annot-fun prefix suffix right-margin)))
          (insert "\n")
          (when show-indices
            (let* ((display-fn (if (functionp show-indices)
                                   show-indices
                                 (lambda (i) (format "%2d " i))))
                   (curr-index (substring-no-properties
                                (funcall display-fn (1+ index)))))
              (insert
               (propertize curr-index 'face 'minibuffer-prompt))))
          (insert displayed-candidate)
          (cond
           ((and right-margin (not annot-fun))
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
                                    ,margin-padding)))
              (if formatting-current-candidate
                  (selectrum--add-face
                   right-margin'selectrum-current-candidate)
                right-margin))))
           ((and extend
                 formatting-current-candidate)
            (insert
             (propertize
              " "
              'face 'selectrum-current-candidate
              'display
              `(space :align-to (- right-fringe
                                   ,margin-padding)))))))
        (cl-incf index))
      (split-string (buffer-string) "\n" t))))

(defun selectrum--minibuffer-exit-hook ()
  "Clean up Selectrum from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'selectrum--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook 'local)
  (when (overlayp selectrum--count-overlay)
    (delete-overlay selectrum--count-overlay))
  (setq selectrum--count-overlay nil))

(defun selectrum--minibuffer-setup-hook (candidates default buf)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of strings that was passed to
`selectrum-read'. DEFAULT is the default value which can be
overridden and BUF the buffer the session was started from."
  (setq-local selectrum-active-p t)
  (setq-local selectrum--last-buffer buf)
  (setq-local auto-hscroll-mode nil)
  (add-hook
   'minibuffer-exit-hook #'selectrum--minibuffer-exit-hook nil 'local)
  (setq-local selectrum--init-p t)
  (when selectrum--repeat
    (delete-minibuffer-contents)
    (insert selectrum--previous-input-string))
  (unless selectrum--candidates-overlay
    (setq selectrum--candidates-overlay
          (make-overlay (point) (point) nil 'front-advance 'rear-advance)))
  ;; If metadata specifies a custom sort function use it as
  ;; `selectrum-preprocess-candidates-function' for this session.
  (when-let ((sortf (selectrum--get-meta 'display-sort-function)))
    (setq-local selectrum-preprocess-candidates-function sortf))
  (cond ((functionp candidates)
         (setq selectrum--preprocessed-candidates nil)
         (setq selectrum--total-num-candidates 0)
         (setq-local selectrum--dynamic-candidates candidates))
        (t
         (setq selectrum--preprocessed-candidates
               (funcall selectrum-preprocess-candidates-function
                        candidates))
         (setq selectrum--total-num-candidates (length candidates))))
  (setq selectrum--default-candidate
        (if (and default (symbolp default))
            (symbol-name default)
          default))
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

(defun selectrum-previous-candidate (&optional arg)
  "Move selection ARG candidates up, stopping at the beginning."
  (interactive "p")
  (selectrum-next-candidate (- (or arg 1))))

(defun selectrum-next-candidate (&optional arg)
  "Move selection ARG candidates down, stopping at the end."
  (interactive "p")
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (selectrum--clamp
           (+ selectrum--current-candidate-index (or arg 1))
           (if (and selectrum--match-required-p
                    (cond (minibuffer-completing-file-name
                           (not (selectrum--at-existing-prompt-path-p)))
                          (t
                           (not (string-empty-p
                                 (minibuffer-contents))))))
               0
             -1)
           (1- (length selectrum--refined-candidates))))))

(defun selectrum-previous-page (&optional arg)
  "Move selection upwards by ARG pages, stopping at the beginning."
  (interactive "p")
  (selectrum-next-page (- (or arg 1))))

(defun selectrum-next-page (&optional arg)
  "Move selection downwards by ARG pages, stopping at the end."
  (interactive "p")
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (selectrum--clamp
           (+ selectrum--current-candidate-index
              (* (or arg 1) selectrum--actual-num-candidates-displayed))
           0
           (1- (length selectrum--refined-candidates))))))

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
                        (let ((crm
                               (if (and selectrum--current-candidate-index
                                        (< selectrum--current-candidate-index
                                           0))
                                   candidate
                                 (with-temp-buffer
                                   (insert selectrum--previous-input-string)
                                   (goto-char (point-min))
                                   (while (re-search-forward
                                           crm-separator nil t))
                                   (delete-region (point) (point-max))
                                   (insert (selectrum--get-full candidate))
                                   (buffer-string)))))
                          (dolist (cand (split-string crm crm-separator t))
                            (apply
                             #'run-hook-with-args
                             'selectrum-candidate-selected-hook
                             (selectrum--get-full cand)
                             selectrum--read-args))
                          crm))
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
  (with-selected-window (active-minibuffer-window)
    (let ((index (selectrum--index-for-arg arg)))
      (if (or (not selectrum--match-required-p)
              (string-empty-p
               (minibuffer-contents))
              (and index (>= index 0))
              (if minibuffer-completing-file-name
                  (selectrum--at-existing-prompt-path-p)
                (member (minibuffer-contents)
                        selectrum--refined-candidates)))
          (selectrum--exit-with
           (selectrum--get-candidate index))
        (minibuffer-message
         (propertize "Match required" 'face 'minibuffer-prompt))))))

(defun selectrum-submit-exact-input ()
  "Exit minibuffer, using the current user input.
This differs from `selectrum-select-current-candidate' in that it
ignores the currently selected candidate, if one exists."
  (interactive)
  (let ((selectrum--current-candidate-index -1))
    (selectrum-select-current-candidate)))

(defvar selectrum--crm-separator-alist
  '((":\\|,\\|\\s-" . ",")
    ("[ \t]*:[ \t]*" . ":")
    ("[ \t]*,[ \t]*" . ",")
    (" " . " "))
  "Values of `crm-separator' mapped to separator strings.
If current `crm-separator' has a mapping the separator gets
inserted automatically when using
`selectrum-insert-current-candidate'.")

(defun selectrum--reset-minibuffer-history-state ()
  "Reset history for current prompt."
  (setq-local minibuffer-history-position 0)
  (setq-local minibuffer-text-before-history
              (minibuffer-contents-no-properties)))

(defvar-local selectrum--inserted-file-completion nil
  "Non-nil when command should trigger refresh.")

(defun selectrum-insert-current-candidate (&optional arg)
  "Insert current candidate into user input area.

Give a prefix argument ARG to select the nth displayed candidate.
Zero means to select the current user input. See
`selectrum-show-indices' which can be used to show candidate
indices. When the prompt is selected this command triggers a
refresh."
  (interactive "P")
  (with-selected-window (active-minibuffer-window)
    (if-let ((index (selectrum--index-for-arg arg))
             (candidate (selectrum--get-candidate index))
             (full (selectrum--get-full candidate)))
        (progn
          (if (and selectrum--current-candidate-index
                   (< selectrum--current-candidate-index 0))
              (if (and (= (minibuffer-prompt-end) (point-max))
                       selectrum--default-candidate)
                  (insert selectrum--default-candidate)
                (goto-char (point-max)))
            (cond ((not selectrum--crm-p)
                   (delete-region (minibuffer-prompt-end)
                                  (point-max))
                   (insert full))
                  (t
                   (goto-char
                    (if (re-search-backward crm-separator
                                            (minibuffer-prompt-end) t)
                        (match-end 0)
                      (minibuffer-prompt-end)))
                   (delete-region (point) (point-max))
                   (insert full)
                   (when-let ((match
                               (assoc crm-separator
                                      selectrum--crm-separator-alist)))
                     (insert (cdr match)))))
            (apply
             #'run-hook-with-args
             'selectrum-candidate-inserted-hook
             candidate selectrum--read-args))
          ;; Ensure refresh of UI. The input input string might be the
          ;; same when the prompt was reinserted. When the prompt was
          ;; selected this will switch selection to first candidate.
          (setq selectrum--previous-input-string nil)
          (when minibuffer-completing-file-name
            ;; Possibly force a refresh for files.
            (setq-local selectrum--inserted-file-completion t))
          (when minibuffer-history-position
            (selectrum--reset-minibuffer-history-state)))
      (unless completion-fail-discreetly
        (ding)
        (minibuffer-message "No match")))))

;;;###autoload
(defun selectrum-select-from-history ()
  "Submit or insert candidate from minibuffer history.
To insert the history item into the previous session use the
binding for `selectrum-insert-current-candidate'. To submit the
history item and exit use `selectrum-select-current-candidate'."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (let ((history (symbol-value minibuffer-history-variable)))
    (when (eq history t)
      (user-error "No history is recorded for this command"))
    (let* ((enable-recursive-minibuffers t)
           (result
            (selectrum--minibuffer-with-setup-hook
                (lambda ()
                  (setq-local selectrum-should-sort-p nil)
                  (setq-local selectrum-candidate-inserted-hook nil)
                  (setq-local selectrum-candidate-selected-hook nil)
                  (use-local-map
                   (make-composed-keymap nil (current-local-map)))
                  (define-key (current-local-map)
                    [remap selectrum-insert-current-candidate]
                    'selectrum--insert-history)
                  (let ((inhibit-read-only t))
                    (goto-char (or (search-backward ":" nil t)
                                   (1- (minibuffer-prompt-end))))
                    (insert
                     (apply
                      #'propertize
                      " [history]"
                      (text-properties-at (point))))))
              (catch 'selectrum-insert-action
                (completing-read
                 (minibuffer-prompt) history nil t nil t)))))
      (if (get-text-property 0 'selectum--insert result)
          (progn
            (delete-minibuffer-contents)
            (insert result)
            (selectrum--reset-minibuffer-history-state))
        (if (and selectrum--match-required-p
                 (not (member result selectrum--refined-candidates)))
            (user-error "That history element is not one of the candidates")
          (selectrum--exit-with result))))))

(defun selectrum--insert-history ()
  "Insert history item.
Only to be used from `selectrum-select-from-history'"
  (interactive)
  (throw 'selectrum-insert-action
         (propertize (selectrum-get-current-candidate 'notfull)
                     'selectum--insert t)))

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
            '(selectrum--preprocessed-candidates
              selectrum--refined-candidates
              selectrum--match-required-p
              selectrum--move-default-candidate-p
              selectrum--default-candidate
              selectrum--visual-input
              selectrum--read-args
              selectrum--count-overlay
              selectrum--repeat)))
     ;; https://github.com/raxod502/selectrum/issues/39#issuecomment-618350477
     (selectrum--let-maybe
       selectrum-active-p
       (,@(mapcar
           (lambda (var)
             `(,var ,var))
           '(selectrum--current-candidate-index
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
    (let* ((minibuffer-allow-text-properties t)
           (resize-mini-windows 'grow-only)
           (prompt (selectrum--remove-default-from-prompt prompt))
           ;; <https://github.com/raxod502/selectrum/issues/99>
           (icomplete-mode nil)
           (buf (current-buffer))
           (res
            (selectrum--minibuffer-with-setup-hook
                (lambda ()
                  ;; Already set the active flag as early as possible
                  ;; so client setup hooks can use it to detect if
                  ;; they are running in a Selectrum session.
                  (setq-local selectrum-active-p t))
              (selectrum--minibuffer-with-setup-hook
                  (:append (lambda ()
                             (selectrum--minibuffer-setup-hook
                              candidates
                              (or (car-safe minibuffer-default)
                                  minibuffer-default
                                  default-candidate)
                              buf)))
                (read-from-minibuffer
                 prompt initial-input selectrum-minibuffer-map nil
                 (or history 'minibuffer-history) default-candidate)))))
      (cond (minibuffer-completion-table
             ;; Behave like completing-read-default which strips the
             ;; text properties but leaves the default unchanged
             ;; when submitting the empty prompt to get it (see
             ;; #180, #107).
             (if (and selectrum--previous-input-string
                      (string-empty-p selectrum--previous-input-string)
                      (equal res selectrum--default-candidate))
                 default-candidate
               (substring-no-properties res)))
            (t res)))))

;;;###autoload
(defun selectrum-completing-read
    (prompt collection &optional
            predicate require-match initial-input
            hist def inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'."
  (ignore inherit-input-method)
  (selectrum-read
   prompt nil
   :initial-input initial-input
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
     (selectrum--minibuffer-with-setup-hook
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
  (let* ((enable-recursive-minibuffers t)
         (input (buffer-substring-no-properties start end))
         (meta (completion-metadata input collection predicate))
         (category (completion-metadata-get meta 'category))
         (bound (pcase category
                  ('file start)
                  (_ (+ start (car (completion-boundaries
                                    input collection predicate ""))))))
         (exit-func (plist-get completion-extra-properties
                               :exit-function))
         (cands (if (not selectrum-completion-in-region-styles)
                    (let ((completion-regexp-list nil))
                      (all-completions input collection predicate))
                  (nconc
                   (let ((completion-styles
                          selectrum-completion-in-region-styles))
                     (completion-all-completions
                      input collection predicate
                      (- end start) meta))
                   nil)))
         ;; See doc of `completion-extra-properties'.
         (exit-status nil)
         (result nil))
    (if (null cands)
        (prog1 nil
          (unless completion-fail-discreetly (ding))
          (message "No match"))
      (prog1 t
        (pcase category
          ('file
           (let ((try nil))
             (setq result
                   (if (and (not (cdr cands))
                            (stringp (setq try (try-completion
                                                input collection predicate))))
                       try
                     (selectrum--completing-read-file-name
                      "Completion: " collection predicate
                      nil input))
                   exit-status 'sole)))
          (_
           (setq result
                 (if (not (cdr cands))
                     (car cands)
                   (selectrum-read
                    "Completion: " cands
                    :minibuffer-completion-table collection
                    :minibuffer-completion-predicate predicate))
                 exit-status (cond ((not (member result cands)) 'sole)
                                   (t 'finished)))))
        (delete-region bound end)
        (push-mark (point) 'no-message)
        (insert (substring-no-properties result))
        (when exit-func
          (funcall exit-func result exit-status))))))

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

(defun selectrum--partial-file-completions
    (path collection predicate &optional raw)
  "Get partial comps for PATH, file COLLECTION and PREDICATE.
Candidates are filtered for ./ and ../ and propertized for
Selectrum unless RAW is non-nil."
  (pcase-let ((`(,pattern ,all ,prefix ,suffix)
               (completion-pcm--find-all-completions
                path collection predicate
                (length path))))
    (when all
      (let ((matches (completion-pcm--hilit-commonality
                      pattern all)))
        (if raw
            matches
          (cl-loop for match in matches
                   unless (string-suffix-p "../" match)
                   collect
                   (let* ((path (string-remove-suffix
                                 "./" match))
                          (full (concat prefix path suffix)))
                     (propertize path
                                 'selectrum--candidate-full
                                 full
                                 'selectrum--partial
                                 prefix))))))))

(defun selectrum--completion-pcm-all-completions (_string cands pred _point)
  "Used for partial-style file completions.
For STRING, CANDS, PRED and POINT see
`completion-pcm-all-completions'."
  (when cands
    (let* ((prefix (get-text-property 0 'selectrum--partial (car cands)))
           (len (length prefix))
           (string (substitute-in-file-name (minibuffer-contents)))
           (cands (cl-loop for cand in cands
                           collect
                           (propertize (concat prefix cand)
                                       'selectrum--candidate-full
                                       (get-text-property
                                        0 'selectrum--candidate-full cand))))
           (res (selectrum--partial-file-completions string cands pred 'raw)))
      (cl-loop for cand in res
               collect (substring cand len)))))

(defun selectrum--completing-read-file-name
    (prompt collection &optional
            predicate require-match initial-input
            hist def _inherit-input-method)
  "Selectrums completing read function for `read-file-name-default'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
            HIST, DEF, _INHERIT-INPUT-METHOD see `completing-read'."
  (let* ((last-dir nil)
         (msg "Press \\[selectrum-insert-current-candidate] to refresh")
         (sortf nil)
         (is-env-completion nil)
         (coll
          (lambda (input)
            (let* (;; Path of input dir might include shadowed paths.
                   (path (substitute-in-file-name input))
                   (is-remote-path
                    (file-remote-p path))
                   (is-connected
                    (and is-remote-path
                         (file-remote-p path nil t)))
                   (dir (or (file-name-directory path) ""))
                   (maybe-tramp (equal dir "/"))
                   ;; The input used for matching current dir entries.
                   (matchstr (file-name-nondirectory path))
                   (cands
                    (cond
                     ;; Guard against automatic tramp connections.
                     ((and (not selectrum--inserted-file-completion)
                           (not is-connected)
                           is-remote-path)
                      (prog1 nil
                        (minibuffer-message
                         (substitute-command-keys msg))))
                     ;; Env var completion.
                     ((string-prefix-p "$" matchstr)
                      (setq is-env-completion t)
                      (setq matchstr (substring matchstr 1))
                      (cl-loop for var in
                               (funcall
                                collection (concat dir "$") predicate t)
                               for val = (getenv var)
                               collect
                               (propertize
                                var
                                'selectrum--candidate-full
                                (concat dir val)
                                'selectrum-candidate-display-right-margin
                                val)))
                     ;; Use cache.
                     ((and (equal last-dir dir)
                           (not maybe-tramp)
                           (not is-env-completion)
                           (or (not selectrum--inserted-file-completion)
                               ;; Reuse cache if inserting file names
                               ;; in same dir.
                               (and (not (directory-name-p matchstr))
                                    (or (not is-remote-path)
                                        is-connected)
                                    (file-exists-p dir)))
                           (not (and minibuffer-history-position
                                     (zerop minibuffer-history-position)
                                     (memq this-command
                                           '(previous-history-element
                                             next-history-element)))))
                      (setq-local selectrum-preprocess-candidates-function
                                  #'identity)
                      selectrum--preprocessed-candidates)
                     ;; Use partial completion.
                     ((and (not selectrum--inserted-file-completion)
                           (not (string-empty-p dir))
                           (or (not is-remote-path)
                               is-connected)
                           (not (file-exists-p dir)))
                      (setq is-env-completion nil)
                      (setq-local selectrum-preprocess-candidates-function
                                  sortf)
                      (setq-local selectrum--inserted-file-completion nil)
                      (selectrum--partial-file-completions
                       path collection predicate))
                     ;; Compute from file table.
                     (t
                      (setq is-env-completion nil)
                      (setq-local selectrum-preprocess-candidates-function
                                  sortf)
                      (setq-local selectrum--inserted-file-completion nil)
                      (let ((files
                             (condition-case _
                                 (delete
                                  "./"
                                  (delete
                                   "../"
                                   (funcall collection
                                            (if maybe-tramp
                                                path
                                              dir)
                                            predicate t)))
                               ;; May happen in case user quits out
                               ;; of a TRAMP prompt.
                               (quit 'quit))))
                        (unless (eq files 'quit)
                          (if (and (not files)
                                   is-remote-path
                                   (not is-connected)
                                   (not (file-exists-p dir)))
                              ;; On first connection when there aren't
                              ;; any results and dir doesn't exist try
                              ;; to get partial completions.
                              (selectrum--partial-file-completions
                               path collection predicate)
                            ;; Remove duplicate tramp entries.
                            (if maybe-tramp
                                (delete-dups files)
                              files))))))))
              (setq last-dir dir)
              `((input . ,matchstr)
                (candidates . ,cands))))))
    (selectrum--minibuffer-with-setup-hook
        ;; The hook needs to run late as `read-file-name-default' sets
        ;; its own syntax table in `minibuffer-with-setup-hook'.
        (:append (lambda ()
                   ;; Pickup the value as configured for current
                   ;; session.
                   (setq sortf selectrum-preprocess-candidates-function)
                   ;; Ensure the variable is also set when
                   ;; selectrum--completing-read-file-name is called
                   ;; directly.
                   (setq-local minibuffer-completing-file-name t)
                   (set-syntax-table
                    selectrum--minibuffer-local-filename-syntax)))
      (selectrum-read
       prompt coll
       :default-candidate (or (car-safe def) def)
       :initial-input (or (car-safe initial-input) initial-input)
       :history hist
       :require-match (eq require-match t)
       :may-modify-candidates t
       :minibuffer-completion-table collection
       :minibuffer-completion-predicate predicate))))

;;;###autoload
(defun selectrum-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'."
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
            (if (not default-filename)
                (apply #'selectrum--completing-read-file-name args)
              (let* ((default (if (consp default-filename)
                                  (car default-filename)
                                default-filename))
                     (df (expand-file-name default))
                     (dd (expand-file-name default-directory))
                     (default-in-prompt-dir
                       (equal (file-name-directory
                               (directory-file-name df))
                              (file-name-directory dd)))
                     (virtual (when (or (not default-in-prompt-dir)
                                        (not (file-exists-p default)))
                                (propertize
                                 (string-remove-prefix dd df)
                                 'selectrum--candidate-full
                                 default))))
                ;; Ajudst default for internal handling as it wasn't
                ;; passed to `read-file-name-default'. See comment at
                ;; `read-file-name-default' call below.
                (unless (equal df dd)   ; Prompt selection.
                  ;; Make the default sorted first by its relative name
                  ;; when it is inside the prompting directory.
                  (when default-in-prompt-dir
                    (setq default
                          (file-relative-name default default-directory)))
                  (if (consp default-filename)
                      (setcar default-filename default)
                    (setq default-filename default))
                  ;; Adjust the DEFAULT arg.
                  (setf (nth 6 args) default-filename))
                (selectrum--minibuffer-with-setup-hook
                    (lambda ()
                      (when virtual
                        (setq-local selectrum--virtual-default-file virtual)))
                  (apply #'selectrum--completing-read-file-name args)))))))
    (read-file-name-default
     prompt dir
     ;; We don't pass default-candidate here to avoid that
     ;; submitting the selected prompt results in the default file
     ;; name. This is the stock Emacs behavior where there is no
     ;; concept of an active selection. Instead we pass the initial
     ;; prompt as default so it gets returned when submitted. In
     ;; addition to that we adjust the DEF argument passed to
     ;; `selectrum--completing-read-file-name' above so the actual
     ;; default gets sorted to the top. This should give the same
     ;; convenience as in default completion (where you can press
     ;; RET at the initial prompt to get the default). The downside
     ;; is that this convenience is gone when sorting is disabled or
     ;; the default-filename is outside the prompting directory but
     ;; this should be rare case.
     (concat
      (expand-file-name
       (or dir
           default-directory))
      initial)
     mustmatch initial predicate)))

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
       (let ((timeout minibuffer-message-timeout))
         (lambda ()
           (cl-letf* ((minibuffer-message-timeout timeout)
                      (orig-put-text-property
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
             (apply func args)))))
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
          (when selectrum-complete-in-buffer
            (setq-default completion-in-region-function
                          #'selectrum-completion-in-region))
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
