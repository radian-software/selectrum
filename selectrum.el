;;; selectrum.el --- Easily select item from list -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+selectrum@radian.codes>
;; Created: 8 Dec 2019
;; Homepage: https://github.com/radian-software/selectrum
;; Keywords: extensions
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: MIT
;; Version: 3.1

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

;; Please see https://github.com/radian-software/selectrum for more
;; information.

;;; Code:

(require 'cl-lib)
(require 'crm)
(require 'minibuf-eldef)
(require 'regexp-opt)
(require 'seq)
(require 'subr-x)

;;; Faces

(defface selectrum-quick-keys-highlight
  '((t :inherit lazy-highlight))
  "Face used for `selectrum-quick-keys'."
  :group 'selectrum-faces)

(defface selectrum-quick-keys-match
  '((t :inherit isearch))
  "Face used for matches of `selectrum-quick-keys'."
  :group 'selectrum-faces)

(defface selectrum-group-title
  '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines."
  :group 'selectrum-faces)

(defface selectrum-group-separator
  '((t :inherit shadow :strike-through t))
  "Face used for the separator lines of the candidate groups."
  :group 'selectrum-faces)

(defface selectrum-current-candidate
  '((t :inherit highlight :extend t))
  "Face used to highlight the currently selected candidate.

In Emacs 27 and greater, this face is assumed to have the
`:extend' attribute set to a non-nil value (the default).
 This is expected by user options like
`selectrum-extend-current-candidate-highlight'."
  :group 'selectrum-faces)

(defface selectrum-completion-annotation
  '((t :inherit completions-annotations))
  "Face used to display annotations of completion tables."
  :group 'selectrum-faces)

(defface selectrum-completion-docsig
  '((t :inherit selectrum-completion-annotation :slant italic))
  "Face used to display docsigs of completion tables."
  :group 'selectrum-faces)

(defface selectrum-mouse-highlight
  '((t :inherit selectrum-current-candidate :underline t))
  "Face used for candidates during mouse hovering."
  :group 'selectrum-faces)

;;; User options

(defgroup selectrum nil
  "Simple incremental narrowing framework with sane API."
  :group 'convenience
  :prefix "selectrum-"
  :link '(url-link "https://github.com/radian-software/selectrum"))

(defcustom selectrum-default-value-format
  (propertize " [default: %s]" 'face 'minibuffer-prompt)
  "Format string for the default value in the minibuffer."
  :type '(choice (const nil) string))

(defcustom selectrum-quick-keys '(?a ?s ?d ?f ?j ?k ?l ?i ?g ?h)
  "Keys for quick selection.
Used by `selectrum-quick-select' and `selectrum-quick-insert'."
  :type '(repeat character))

(defcustom selectrum-group-format
  (concat
   #("    " 0 4 (face selectrum-group-separator))
   #(" %s " 0 4 (face selectrum-group-title))
   #(" " 0 1 (face selectrum-group-separator display (space :align-to right))))
  "Format string used for the group title."
  :type '(choice (const nil) string))

(defcustom selectrum-should-sort t
  "Non-nil if preprocessing function should sort.
This should be respected by user functions for optimal results."
  :type 'boolean)

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

(defcustom selectrum-display-action-hook nil
  "Hook to run when initializing the candidates buffer.
See `selectrum-display-action'."
  :type 'hook)

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

(defcustom selectrum-completion-in-region-styles nil
  "The `completion-styles' used by `selectrum-completion-in-region'.
These are used for the initial filtering of candidates according
to the text around point. The initial filtering styles for
completion in region might generally differ from the styles you
want to use for usual completion. If this option is nil the
candidates will be filtered by `all-completions' first and if
that doesn't reveal any matches the completion is retried using
`completion-styles', honoring adjustments according to
`completion-category-overrides' and
`completion-category-defaults'."
  :type completion--styles-type)

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

(defcustom selectrum-candidate-selected-hook nil
  "Normal hook run when the user selects a candidate.
It gets the string the user selected as argument."
  :type 'hook)

(defcustom selectrum-candidate-inserted-hook nil
  "Normal hook run when the user inserts a candidate.
\(This happens by typing \\[selectrum-insert-current-candidate].)
It gets the string the user inserted as argument."
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

(defcustom selectrum-extend-current-candidate-highlight 'auto
  "Whether to extend highlighting of the current candidate until the margin.

When set to nil, only highlight the displayed text. When set to
`auto' (the default), the highlighting is automatically extended
when the session defines any annotations. Any other non-nil value
means to always extend the highlighting.

In Emacs 27 and greater, this option requires that the face
`selectrum-current-candidate' have a non-nil value for the
`:extend' attribute (the default)."
  :type '(choice (const :tag "Automatic" auto) boolean))

(defcustom selectrum-files-select-input-dirs nil
  "Whether to select the input for directories.
When this is non-nil the input in file completions will get
selected when it contains a directory name."
  :type 'boolean)

;;;###autoload
(defcustom selectrum-complete-in-buffer t
  "If non-nil, use Selectrum for `completion-in-region'.
This option needs to be set before activating `selectrum-mode'."
  :type 'boolean
  :group 'selectrum)

(defcustom selectrum-cycle-movement nil
  "Whether commands that move past the last candidate move to the first.

See also the user option `selectrum-count-style' for displaying
the position of the currently selected candidate."
  :type 'boolean
  :group 'selectrum)

;;; Variables

(defvar selectrum-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)

    ;; Previously, we needed to explicitly bind `abort-recursive-edit'
    ;; to handle recursive minibuffers, but the new function
    ;; `abort-minibuffers' already handles them.
    ;; `minibuffer-keyboard-quit' now uses this function.
    (unless (fboundp 'abort-minibuffers)
      (define-key map [remap keyboard-quit] #'abort-recursive-edit)
      ;; This is bound in `minibuffer-local-map' by loading `delsel', so
      ;; we have to account for it too.
      (define-key map [remap minibuffer-keyboard-quit]
        #'abort-recursive-edit))

    ;; Next/prev cand:
    ;; Override both the arrow keys and C-n/C-p.
    (define-key map [remap previous-line]
      #'selectrum-previous-candidate)
    (define-key map [remap next-line]
      #'selectrum-next-candidate)
    (define-key map [remap previous-line-or-history-element]
      #'selectrum-previous-candidate)
    (define-key map [remap next-line-or-history-element]
      #'selectrum-next-candidate)

    ;; Next/prev page:
    (define-key map [remap scroll-down-command]
      #'selectrum-previous-page)
    (define-key map [remap scroll-up-command]
      #'selectrum-next-page)

    ;; First/last cand:
    ;; Use `minibuffer-beginning-of-buffer' for Emacs >=27 and
    ;; `beginning-of-buffer' for Emacs <=26.
    (define-key map [remap minibuffer-beginning-of-buffer]
      #'selectrum-goto-beginning)
    (define-key map [remap beginning-of-buffer]
      #'selectrum-goto-beginning)
    (define-key map [remap end-of-buffer]
      #'selectrum-goto-end)

    ;; Next/prev group:
    (define-key map (kbd "C-M-n")
      #'selectrum-next-group)
    (define-key map (kbd "C-M-p")
      #'selectrum-previous-group)
    (define-key map [remap forward-paragraph]
      #'selectrum-next-group)
    (define-key map [remap backward-paragraph]
      #'selectrum-previous-group)

    ;; Other:
    (define-key map [remap exit-minibuffer]
      #'selectrum-select-current-candidate)
    (define-key map [remap kill-ring-save]
      #'selectrum-kill-ring-save)
    (define-key map [remap previous-matching-history-element]
      #'selectrum-select-from-history)
    (define-key map [remap backward-kill-sexp]
      #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-M-DEL") #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-M-<backspace>") #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-j") #'selectrum-submit-exact-input)
    (define-key map (kbd "TAB") #'selectrum-insert-current-candidate)
    (define-key map (kbd "M-q") 'selectrum-cycle-display-style)
    (define-key map (kbd "M-i") 'selectrum-quick-insert)
    (define-key map (kbd "M-m") 'selectrum-quick-select)
    ;; Return the map.
    map)
  "Keymap used by Selectrum in the minibuffer.")

(defvar-local selectrum-move-default-candidate t
  "Non-nil means move default candidate to start of list.
Nil means select the default candidate initially even if it's not
at the start of the list.")

(defvar selectrum--display-action-buffer " *selectrum*"
  "Buffer to display candidates using `selectrum-display-action'.")

(defvar selectrum--quick-fun nil
  "Function for quick selection.
Used by `selectrum-quick-select' and `selectrum-quick-insert'.
Receives the display index and candidate and should return the
new candidate string used for display.")

(defvar selectrum--crm-separator-alist
  '((":\\|,\\|\\s-" . ",")
    ("[ \t]*:[ \t]*" . ":")
    ("[ \t]*,[ \t]*" . ",")
    ("\\s-*&\\s-*" . " & ")
    (" " . " "))
  "Values of `crm-separator' mapped to separator strings.
If current `crm-separator' has a mapping the separator gets
inserted automatically when using
`selectrum-insert-current-candidate'.")

(defvar selectrum--minibuffer-default-in-prompt-regexps
  (let ((minibuffer-eldef-shorten-default nil))
    (minibuffer-default--in-prompt-regexps))
  "Regexps for determining if the prompt message includes the default value.
See `minibuffer-default-in-prompt-regexps', from which this is derived.")

(defvar selectrum--minibuffer-local-filename-syntax
  (let ((table (copy-syntax-table minibuffer-local-filename-syntax)))
    (modify-syntax-entry ?\s "_" table)
    table)
  "Syntax table for reading file names.
Same as `minibuffer-local-filename-syntax' but considers spaces
as symbol constituents.")

(defvar selectrum--old-completing-read-function nil
  "Previous value of `completing-read-function'.")

(defvar selectrum--old-completion-in-region-function nil
  "Previous value of `completion-in-region-function'.")

(defvar selectrum--old-read-buffer-function nil
  "Previous value of `read-buffer-function'.")

(defvar selectrum--old-read-file-name-function nil
  "Previous value of `read-file-name-function'.")

;;; Session state

(defvar-local selectrum--history-hash nil
  "History hash table.")

(defvar-local selectrum--last-buffer nil
  "The buffer that was current before the active session.")

(defvar-local selectrum--candidates-overlay nil
  "Overlay used to display current candidates.")

(defvar-local selectrum--count-overlay nil
  "Overlay used to display count information before prompt.")

(defvar-local selectrum--dynamic-candidates-function nil
  "The dynamic candidate function passed to `selectrum--read'.
When set the dynamic candidate function is called on each input
change. The results are subsequently preprocessed by
`selectrum-preprocess-candidates-function' and saved as
`selectrum--preprocessed-candidates'. See `selectrum--read' for
more details on function collections.")

(defvar-local selectrum--preprocessed-candidates nil
  "Preprocessed list of candidates.
This list contains the candidates of the current session after
preprocessing them with
`selectrum-preprocess-candidates-function'. The list is
subsequently passed to `selectrum-refine-candidates-function'.
For the refined candidates see `selectrum--refined-candidates'.")

(defvar-local selectrum--refined-candidates nil
  "Refined list of candidates to be displayed.
This is derived from `selectrum--preprocessed-candidates' by
`selectrum-refine-candidates-function' every time the user input
changes, and is subsequently passed to
`selectrum-highlight-candidates-function'.")

(defvar-local selectrum--current-candidate-index nil
  "Index of currently selected candidate, or nil if no candidates.
The value -1 means prompt selection.")

(defvar-local selectrum--first-index-displayed nil
  "Index of the first displayed candidate.")

(defvar-local selectrum--actual-num-candidates-displayed nil
  "The actual number of candidates displayed.")

(defvar-local selectrum--previous-input-string nil
  "Previous user input string in the minibuffer.
Used to check if the user input has changed and candidates need
to be re-filtered.")

(defvar-local selectrum--virtual-input nil
  "Input used for refinement and highlighting.
What is considered the current input can be changed by
`selectrum--dynamic-candidates-function'.")

(defvar-local selectrum--match-is-required nil
  "Non-nil if the user must select one of the candidates.
Equivalently, nil if the user is allowed to submit their own
input that does not match any of the displayed candidates.")

(defvar-local selectrum--is-crm-session nil
  "Non-nil for `selectrum-completing-read-multiple' sessions.")

(defvar-local selectrum--default-candidate nil
  "Default candidate, or nil if none given.")

(defvar-local selectrum--last-command nil
  "Name of last interactive command that invoked Selectrum.")

(defvar-local selectrum--last-prefix-arg nil
  "Prefix argument given to last interactive command that invoked Selectrum.")

(defvar-local selectrum--last-input nil
  "Input of last Selectrum session.
This is different from `selectrum--previous-input-string' which
reflects the previous input within a session.")

(defvar-local selectrum--repeat nil
  "Non-nil means try to restore the minibuffer state during setup.
This is used to implement `selectrum-repeat' and also to restore
a candidate index on next computation.")

(defvar-local selectrum-is-active nil
  "Non-nil means Selectrum is currently active.")

(defvar-local selectrum--is-initializing nil
  "Non-nil means the current session is initializing.
This is non-nil during the first call of
`selectrum--update'.")

(defvar-local selectrum--virtual-default-file nil
  "If set used as a virtual file to prompt with.")

(defvar-local selectrum--get-full-prompt-prefix nil
  "Cons of prompt and cached prefix for `selectrum--get-full'.")

(defvar-local selectrum--line-height nil
  "The `line-pixel-height' of current session.")

(defvar-local selectrum--inserted-file-completion nil
  "Non-nil when command should trigger refresh.")

;;; Utility functions
(defun selectrum--select-active-minibuffer-window ()
  "Select the active minibuffer window.

This function is added locally to `pre-command-hook' in buffers
displayed via `selectrum-display-action'. This is important for
using the mouse in such displayed buffers, since otherwise focus
would move away from the minibuffer window when clicking on
candidates."
  (select-window (active-minibuffer-window)))

(defun selectrum-refine-candidates-using-completions-styles (input candidates)
  "Use INPUT to filter and highlight CANDIDATES.
Uses `completion-styles'."
  (nconc
   (completion-all-completions
    input candidates nil (length input))
   nil))

(defun selectrum--pred (x y)
  "Compare X and Y."
  (or (< (cdr x) (cdr y))
      (and (= (cdr x) (cdr y))
           (string< (car x) (car y)))))

(defun selectrum-default-candidate-preprocess-function (candidates)
  "Sort CANDIDATES by history position, length and alphabetically.
See `selectrum-preprocess-candidates-function'."
  (when selectrum-should-sort
    (unless selectrum--history-hash
      ;; History disabled if `minibuffer-history-variable' eq `t'.
      (let ((list (and (not (eq minibuffer-history-variable t))
                       (symbol-value minibuffer-history-variable)))
            (hist-idx 0))
        (setq-local selectrum--history-hash
                    (make-hash-table :test #'equal
                                     :size (length list)))
        ;; Store the history position first in a hashtable in order to
        ;; allow O(1) history lookup.
        (dolist (elem list)
          (unless (gethash elem selectrum--history-hash)
            (puthash elem hist-idx selectrum--history-hash))
          (setq hist-idx (1+ hist-idx)))))
    ;; Decorate each candidate with (hist-idx<<13) + length. This way we
    ;; sort first by hist-idx and then by length. We assume that the
    ;; candidates are shorter than 2**13 characters and that the history
    ;; is shorter than 2**16 entries.
    (let ((cand candidates))
      (while cand
        (setcar cand
                (cons (car cand)
                      (+ (lsh (gethash (car cand)
                                       selectrum--history-hash #xFFFF)
                              13)
                         (length (car cand)))))
        (setq cand (cdr cand))))
    (setq candidates (sort candidates #'selectrum--pred))
    ;; Drop decoration from the candidates
    (let ((cand candidates))
      (while cand
        (setcar cand (caar cand))
        (setq cand (cdr cand)))))
  candidates)

(defun selectrum--clamp (x lower upper)
  "Constrain X to be between LOWER and UPPER inclusive.
If X < LOWER, return LOWER. If X > UPPER, return UPPER. Else
return X.

See the function `selectrum--cycle' for a wrapping version of
this function."
  (min (max x lower) upper))

(defun selectrum--cycle (x max-index selectable-prompt)
  "Wrap around index X to be inclusively between the min and MAX-INDEX.

If SELECTABLE-PROMPT is non-nil, the min is -1.  Otherwise, it is 0.

For example:
    (selectrum--cycle 8 6 nil)  ; => 1
    (selectrum--cycle -3 6 nil) ; => 4
    (selectrum--cycle -14 4 t)  ; => 4
    (selectrum--cycle 12 4 t)   ; => 0

See the function `selectrum--clamp' for a non-wrapping version of
this function."
  (let ((step) (min))
    (if selectable-prompt
        (setq step (+ 2 max-index)
              min -1)
      (setq step (1+ max-index)
            min 0))
    (cond
     ;; If no candidates, stay on prompt.
     ((= max-index -1) -1)
     ;; If only one candidate, move to that or input line.
     ((zerop max-index)
      (cond ((not selectable-prompt) 0)
            ((cl-oddp x) -1)
            (t 0)))
     ;; If past the limits, wrap around. Maybe include the input line.
     ((> x max-index)
      (while (> x max-index)
        (cl-decf x step))
      x)
     ((< x min)
      (while (< x min)
        (cl-incf x step))
      x)
     ;; Otherwise, if within limits, return x.
     (t x))))

(defun selectrum--move-to-front-destructive (elt lst)
  "Move ELT to front of LST, if present.
Make comparisons using `equal'. Modify the input list
destructively and return the modified list."
  ;; We can't use something like "(cons elt (filter elt lst))"
  ;; in case there are multiple instances of `elt' in `lst'.
  (let ((matches)
        (others))
    (dolist (i lst)
      (if (equal i elt)
          (push i matches)
        (push i others)))
    ;; Maintain the order of the list.
    (nconc (nreverse matches) (nreverse others))))

(defmacro selectrum--minibuffer-with-setup-hook (fun &rest body)
  "Variant of `minibuffer-with-setup-hook' using a symbol and `fset'.
This macro is only needed to prevent memory leaking issues with
the upstream `minibuffer-with-setup-hook' macro. FUN is the hook
function and BODY opens the minibuffer."
  ;; Copied from https://github.com/minad/consult/commit/27e055e.
  (declare (indent 1) (debug ([&or (":append" form) [&or symbolp form]] body)))
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

(defun selectrum--match-strictly-required-p ()
  "Return non-nil if a match is strictly required."
  (and selectrum--match-is-required
       (not (memq selectrum--match-is-required
                  '(confirm confirm-after-completion)))))

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
  (with-current-buffer (if (and (eq collection 'help--symbol-completion-table)
                                (buffer-live-p selectrum--last-buffer))
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
  (when (and selectrum-is-active
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
  (when (and selectrum-is-active
             selectrum--refined-candidates)
    (if notfull
        selectrum--refined-candidates
      (mapcar #'selectrum--get-full selectrum--refined-candidates))))

(defun selectrum-get-current-input ()
  "Get current Selectrum user input."
  (when selectrum-is-active
    (with-selected-window (active-minibuffer-window)
      (minibuffer-contents))))
(make-obsolete 'selectrum-get-current-input nil "3.1")

(defun selectrum-set-selected-candidate (&optional string)
  "Set currently selected candidate to STRING.
STRING defaults to `minibuffer-contents'. Computation of
candidates is skipped from there on. This is useful for injecting
a candidate in `minibuffer-setup-hook' and immediately exit with
it afterwards. With default completion there is no computation
triggered initially and this function can be used to mimic this
behavior."
  (when selectrum-is-active
    (with-selected-window (active-minibuffer-window)
      (let ((string (or string (minibuffer-contents))))
        (setq-local selectrum--refined-candidates
                    (list string))
        (setq-local selectrum--current-candidate-index 0)
        ;; Skip updates.
        (remove-hook 'post-command-hook #'selectrum--update 'local)))))

(defun selectrum--get-full (candidate)
  "Get full form of CANDIDATE."
  (or (get-text-property 0 'selectrum--candidate-full candidate)
      (get-text-property 0 'selectrum-candidate-full candidate)
      (when minibuffer-completing-file-name
        (if (and selectrum--current-candidate-index
                 (< selectrum--current-candidate-index 0))
            candidate
          (let ((input (minibuffer-contents)))
            (if (and selectrum--get-full-prompt-prefix
                     (string-equal (car selectrum--get-full-prompt-prefix)
                                   input))
                (concat (cdr selectrum--get-full-prompt-prefix) candidate)
              (let* ((path (substitute-in-file-name input))
                     (dirlen (length (file-name-directory path)))
                     (prefixlen (car (completion--sifn-requote dirlen input)))
                     (prefix (substring input 0 prefixlen)))
                (setq-local selectrum--get-full-prompt-prefix
                            (cons input prefix))
                (concat prefix candidate))))))
      candidate))

(defun selectrum--get-candidate (index)
  "Get candidate at given INDEX. Negative means get the current user input."
  (if (and index (>= index 0))
      (nth index selectrum--refined-candidates)
    (buffer-substring-no-properties
     (minibuffer-prompt-end)
     (point-max))))

(defun selectrum--metadata ()
  "Get completion metadata.
Demotes any errors to messages."
  (condition-case-unless-debug err
      (completion-metadata (minibuffer-contents)
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
    (error (message (error-message-string err)) nil)))

(defun selectrum--get-meta (setting)
  "Get metadata SETTING from completion table."
  (completion-metadata-get (selectrum--metadata) setting))

(defun selectrum-exhibit (&optional keep-selection)
  "Trigger an update of Selectrum's completion UI.
If KEEP-SELECTION is non-nil keep the current candidate selected
when possible (it is still a member of the candidate set)."
  (when-let ((mini (active-minibuffer-window)))
    (with-selected-window mini
      (when (and minibuffer-completion-table
                 (not selectrum--dynamic-candidates-function))
        (setq-local selectrum--preprocessed-candidates nil))
      (setq-local selectrum--previous-input-string nil)
      (selectrum--update
       (and keep-selection
            (selectrum-get-current-candidate))))))

;;; Hook functions

(defun selectrum--count-info ()
  "Return a string of count information to be prepended to prompt."
  (let ((total (length selectrum--refined-candidates))
        (current (1+ (or selectrum--current-candidate-index -1))))
    (pcase selectrum-count-style
      ('matches         (format "%-4d " total))
      ('current/matches (format "%-6s " (format "%d/%d" current total)))
      (_                ""))))

(defun selectrum--create-display-buffer ()
  "Create and return a buffer named by `selectrum--display-action-buffer'."
  (with-current-buffer
      (get-buffer-create selectrum--display-action-buffer)
    (setq cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil)
    (setq display-line-numbers nil)
    (setq buffer-undo-list t)
    (setq buffer-read-only t)
    (setq show-trailing-whitespace nil)
    (goto-char (point-min))
    (run-hooks 'selectrum-display-action-hook)
    ;; We want to prevent interacting with the buffer.
    ;; Ideally, users only interact with the
    ;; minibuffer, but we need to reselect the
    ;; minibuffer window in case the user clicks on a
    ;; candidate.
    (add-hook
     'pre-command-hook
     #'selectrum--select-active-minibuffer-window
     nil t)
    (current-buffer)))

(defun selectrum--get-display-window ()
  "Get candidate display window.

Window will be created by `selectrum-display-action'."
  (let ((buf (get-buffer selectrum--display-action-buffer))
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

(defun selectrum--group-by (fun elems)
  "Group ELEMS by FUN."
  (when elems
    (let ((group-list)
          (group-hash (make-hash-table :test #'equal)))
      (while elems
        (let* ((key (funcall fun (car elems) nil))
               (group (gethash key group-hash)))
          (if group
              ;; Append to tail of group
              (setcdr group (setcdr (cdr group) elems))
            (setq group (cons elems elems)) ;; (head . tail)
            (push group group-list)
            (puthash key group group-hash))
          (setq elems (cdr elems))))
      (setcdr (cdar group-list) nil) ;; Unlink last tail
      (setq group-list (nreverse group-list))
      (prog1 (caar group-list)
        (while (cdr group-list) ;; Link groups
          (setcdr (cdar group-list) (caadr group-list))
          (setq group-list (cdr group-list)))))))

(defun selectrum--vertical-display-style
    (win input nrows _ncols index
         max-index _first-index-displayed _last-index-displayed)
  "Insert candidates vertically into current buffer.
Used as insertion function for `vertical' display style, see
`selectrum-display-style'. WIN is the window where buffer will get
displayed in. INPUT is the input string used to highlight the
candidates. NROWS is the number of lines available and NCOLS the
number of available columns. If there are candidates INDEX is the
index of the currently selected candidate and MAX-INDEX is the index
of the maximal index of the collection. When candidates are already
displayed FIRST-INDEX-DISPLAYED is the index of the candidate that is
displayed first and LAST-INDEX-DISPLAYED the index of the last one."
  (let* ((first-index-displayed
          (if (not index)
              0
            (selectrum--clamp
             ;; Adding one here makes it look slightly better, as
             ;; there are guaranteed to be more candidates shown
             ;; below the selection than above.
             (1+ (- index (max 1 (/ nrows 2))))
             0
             (max (- (1+ max-index) nrows)
                  0))))
         (i first-index-displayed)
         (highlighted-candidates
          (selectrum--highlighted-candidates
           input
           first-index-displayed nrows))
         (metadata (selectrum--metadata))
         (annotf (or (completion-metadata-get metadata 'annotation-function)
                     (plist-get completion-extra-properties
                                :annotation-function)))
         (aff (or (completion-metadata-get metadata 'affixation-function)
                  (plist-get completion-extra-properties
                             :affixation-function)))
         (docsigf (plist-get completion-extra-properties :company-docsig))
         (groupf (and selectrum-group-format
                      (completion-metadata-get metadata 'group-function)))
         (candidates (cond (aff
                            (selectrum--affixate aff highlighted-candidates))
                           ((or annotf docsigf)
                            (selectrum--annotate
                             highlighted-candidates annotf docsigf))
                           (t highlighted-candidates)))
         (last-title nil)
         (lines ()))
    (dolist (cand candidates)
      (when-let (new-title (and groupf (funcall groupf cand nil)))
        (unless (equal last-title new-title)
          (push (format selectrum-group-format (setq last-title new-title))
                lines)
          (push "\n" lines))
        (setq cand (funcall groupf cand 'transform)))
      (let* ((formatting-current-candidate
              (eq i index))
             (newline
              (if (and formatting-current-candidate
                       (if (eq selectrum-extend-current-candidate-highlight
                               'auto)
                           (or aff annotf docsigf)
                         selectrum-extend-current-candidate-highlight))
                  (selectrum--selection-highlight "\n")
                "\n"))
             (full-cand (selectrum--format-candidate
                         input cand i index
                         first-index-displayed
                         'should-annotate)))
        (push full-cand lines)
        (push newline lines)
        (cl-incf i)))
    (list
     (length highlighted-candidates)
     first-index-displayed
     (if highlighted-candidates
         (apply #'concat
                (and (window-minibuffer-p win) "\n")
                (nreverse lines))
       ""))))

(defun selectrum--horizontal-display-style
    (win input _nrows ncols index
         max-index first-index-displayed last-index-displayed)
  "Insert candidates horizontally.
For WIN, INPUT, NROWS, NCOLS, INDEX, MAX-INDEX,
FIRST-INDEX-DISPLAYED and LAST-INDEX-DISPLAYED
see `selectrum--vertical-display-style'. For known keys see
the `horizontal' description of `selectrum-display-style'."
  (let* ((settings (cdr selectrum-display-style))
         (before-cands (or (plist-get settings :before-candidates)
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
          (selectrum--highlighted-candidates
           input first-index-displayed
           (floor ncols (1+ (length separator)))))
         (n 0)
         (insert nil))
    (when cands
      (let ((ncols ncols))
        (while (and cands (> ncols 0))
          (let ((cand (pop cands)))
            (setq cand
                  (selectrum--format-candidate
                   input cand (+ n first-index-displayed)
                   index first-index-displayed))
            (setq cand (string-trim (replace-regexp-in-string
                                     "[ \t][ \t]+" " " cand)))
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
      (setq insert (nreverse insert)))
    (list n
          first-index-displayed
          (apply #'concat insert))))

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

(defun selectrum--insert-candidates (win input plen)
  "Return rendered candidates to be displayed in WIN.
INPUT is the current user input. PLEN is the prompt prefix length.
Returns a list with three elements, number of displayed candidates,
the index of the first displayed candidate and the string to display."
  (let* ((index (when (and selectrum--current-candidate-index
                           (not (< selectrum--current-candidate-index 0)))
                  selectrum--current-candidate-index))
         (nlines (selectrum--max-num-candidate-lines win))
         (mindex (1- (length selectrum--refined-candidates)))
         (findex selectrum--first-index-displayed)
         (num selectrum--actual-num-candidates-displayed)
         (ncols (if (window-minibuffer-p win)
                    (- (window-body-width win)
                       (- (point-max)
                          (window-hscroll win))
                       plen)
                  (window-body-width win)))
         (horizp (eq (car selectrum-display-style) 'horizontal))
         (insert-fun (if horizp
                         #'selectrum--horizontal-display-style
                       #'selectrum--vertical-display-style))
         (lindex (when (and findex num)
                   (+ findex
                      (max 0 (1- num)))))
         (insert-res (funcall insert-fun win input
                              nlines ncols index mindex findex lindex)))
    (cons
     horizp
     (if (or (not index) (not findex)
             (>= (+ findex (car insert-res)) index))
         insert-res
       ;; When the insertion function was switched the current index
       ;; might be out of sight in this case reinsert with the current
       ;; index displayed as the first one.
       (funcall insert-fun win input
                nlines ncols index mindex index lindex)))))

(defun selectrum--at-existing-prompt-path-p ()
  "Return non-nil when current file prompt exists."
  (and minibuffer-completing-file-name
       (file-exists-p
        (substitute-in-file-name (minibuffer-contents)))))

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

(defun selectrum--preprocess (candidates)
  "Preprocess CANDIDATES list.
The preprocessing applies the `selectrum-preprocess-candidates-function'
and the `group-function'."
  (setq-local selectrum--preprocessed-candidates
              (funcall selectrum-preprocess-candidates-function
                       candidates))
  ;; Empty candidates aren't indicated or can be selected in the
  ;; default UI so we ignore them to avoid display issues.
  (setq-local selectrum--preprocessed-candidates
              (delete "" selectrum--preprocessed-candidates)))

(defun selectrum--update-dynamic-candidates (input)
  "Update dynamic candidate set with new INPUT."
  (cond
   ((functionp selectrum--dynamic-candidates-function)
    (let ((result
           ;; Ensure dynamic functions won't
           ;; break in post command hook.
           (condition-case-unless-debug err
               (funcall
                selectrum--dynamic-candidates-function
                input)
             (error (message (error-message-string err))
                    nil))))
      ;; Avoid modifying the returned
      ;; candidates to let the function
      ;; reuse them.
      (selectrum--preprocess
       (copy-sequence
        (if (stringp (car result))
            result
          (setq input (or (alist-get 'input result)
                          input))
          (alist-get 'candidates result))))))
   ;; No candidates were passed, initialize them
   ;; from `minibuffer-completion-table'.
   ((and (not selectrum--preprocessed-candidates)
         minibuffer-completion-table)
    (selectrum--preprocess
     (selectrum--normalize-collection
      minibuffer-completion-table
      minibuffer-completion-predicate))))
  ;; Return input which may have been modified
  input)

(defun selectrum--update-refined-candidates (input)
  "Update refined candidates according to current INPUT."
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
    (setq-local selectrum--refined-candidates
                (funcall selectrum-refine-candidates-function
                         input cands)))
  ;; Group candidates. This has to be done after refinement, since
  ;; refinement can reorder the candidates.
  (when-let (groupf (selectrum--get-meta 'group-function))
    ;; Ensure that default candidate appears at the top if
    ;; `selectrum-move-default-candidate' is set. It is redundant to
    ;; do this here, since we move the default candidate also
    ;; afterwards. However this way the grouping function is informed
    ;; of the desired candidate order.
    (when (and selectrum-move-default-candidate
               selectrum--default-candidate)
      (setq-local selectrum--refined-candidates
                  (selectrum--move-to-front-destructive
                   selectrum--default-candidate
                   selectrum--refined-candidates)))
    (setq-local
     selectrum--refined-candidates
     (selectrum--group-by groupf selectrum--refined-candidates)))
  (when selectrum--virtual-default-file
    (unless (equal selectrum--virtual-default-file "")
      (setq-local selectrum--refined-candidates
                  (cons (propertize
                         selectrum--virtual-default-file
                         'face 'shadow)
                        selectrum--refined-candidates)))
    (setq-local selectrum--virtual-default-file nil))
  ;; Ensure that default candidate appears at the top if
  ;; `selectrum-move-default-candidate' is set. We have to do this
  ;; here since the refined candidates could have been reordered by
  ;; the refinement function.
  (when (and selectrum-move-default-candidate
             selectrum--default-candidate)
    (setq-local selectrum--refined-candidates
                (selectrum--move-to-front-destructive
                 selectrum--default-candidate
                 selectrum--refined-candidates)))
  ;; Ensure that exactly matching input appears at the top.
  (setq-local selectrum--refined-candidates
              (selectrum--move-to-front-destructive
               ;; Make sure matching dirnames are sorted first.
               (if (and minibuffer-completing-file-name
                        (member (file-name-as-directory input)
                                selectrum--refined-candidates))
                   (file-name-as-directory input)
                 input)
               selectrum--refined-candidates)))

(defun selectrum--update-input-changed (input keep-selected)
  "Update state when INPUT string has changed.
KEEP-SELECTED can be a candidate which should stay selected after
the update."
  ;; Track current input globally and in last buffer for
  ;; selectrum-repeat.
  (setq-default selectrum--last-input input)
  (when (buffer-live-p selectrum--last-buffer)
    (with-current-buffer selectrum--last-buffer
      (setq-local selectrum--last-input input)))
  (setq-local selectrum--previous-input-string input)
  (setq-local selectrum--virtual-input
              (selectrum--update-dynamic-candidates input))
  (selectrum--update-refined-candidates selectrum--virtual-input)
  (setq-local selectrum--first-index-displayed nil)
  (setq-local selectrum--actual-num-candidates-displayed nil)
  (setq-local selectrum--current-candidate-index
              (selectrum--compute-current-candidate-index keep-selected)))

(defun selectrum--compute-current-candidate-index (keep-selected)
  "Compute the index of the current candidate.
KEEP-SELECTED can be a candidate which should stay selected after
the update."
  ;; Be careful with reordering of the cond clauses because they are
  ;; assumed to get checked in this order!
  (cond
   ;; Try to restore the old index when repeating.
   (selectrum--repeat
    (setq-local selectrum--repeat nil)
    (if (and selectrum--refined-candidates
             selectrum--current-candidate-index)
        selectrum--current-candidate-index
      (selectrum--compute-current-candidate-index nil)))
   ;; If there are no candidates the prompt should be selected.
   ((null selectrum--refined-candidates)
    (when (or (not (selectrum--match-strictly-required-p))
              (selectrum--at-existing-prompt-path-p))
      -1))
   (keep-selected
    (or (cl-position keep-selected
                     selectrum--refined-candidates
                     :key #'selectrum--get-full
                     :test #'equal)
        0))
   ;; Prompt selection.
   ((or
     ;; When the default isn't available via candidate selection, select
     ;; the prompt.
     (and selectrum--default-candidate
          (string-empty-p (minibuffer-contents))
          (not (member selectrum--default-candidate
                       selectrum--refined-candidates)))
     ;; When the prompt contains the the default select it right away.
     (and selectrum--is-initializing
          (equal selectrum--default-candidate
                 (minibuffer-contents)))
     ;; For history navigation select the prompt as the suggested
     ;; candidates are inserted there.
     (and (not (= (minibuffer-prompt-end) (point-max)))
          (or (and minibuffer-history-position
                   (not (zerop
                         minibuffer-history-position))
                   isearch-mode)
              (memq this-command
                    '(next-history-element
                      previous-history-element)))
          (or (not (selectrum--match-strictly-required-p))
              (selectrum--at-existing-prompt-path-p))))
    -1)
   ;; Select first candidate if the first candidate is known to be the
   ;; default.
   ((or (not selectrum--default-candidate)
        selectrum-move-default-candidate)
    0)
   ;; Select the default initially.
   (selectrum--is-initializing
    ;; FIXME: For file completions the default can be relative, also
    ;; there might be a mismatch because of name abbreviation.
    (or (cl-position selectrum--default-candidate
                     selectrum--refined-candidates
                     :key #'selectrum--get-full
                     :test #'equal)
        0))
   (t 0)))

(defun selectrum--format-default ()
  "Format default value using `selectrum-default-value-format'.
The default value is shown in the minibuffer if it is not
part of the candidate set."
  (when (and selectrum-default-value-format
             (= (minibuffer-prompt-end) (point-max))
             (or
              (and selectrum--current-candidate-index
                   (< selectrum--current-candidate-index 0))
              (and (not (selectrum--match-strictly-required-p))
                   (not selectrum--refined-candidates))
              (and selectrum--default-candidate
                   (not minibuffer-completing-file-name)
                   (not (member selectrum--default-candidate
                                selectrum--refined-candidates)))))
    (format selectrum-default-value-format
            (propertize
             (or selectrum--default-candidate "\"\"")
             'face
             (if (and selectrum--current-candidate-index
                      (< selectrum--current-candidate-index
                         0))
                 'selectrum-current-candidate
               (get-text-property
                0 'face
                selectrum-default-value-format))))))

(defun selectrum--update (&optional keep-selected)
  "Update state.
KEEP-SELECTED can be a candidate which should stay selected after
the update."
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
        ;; <https://github.com/radian-software/selectrum/issues/31>.
        (buffer-undo-list t)
        (input (buffer-substring (minibuffer-prompt-end)
                                 (point-max)))
        (keep-mark-active (not deactivate-mark)))
    (unless (equal input selectrum--previous-input-string)
      (selectrum--update-input-changed input keep-selected))
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
                       (progn
                         ;; We want the buffer to exist even if we
                         ;; don't get the window. While we currently
                         ;; don't display the buffer unless there are
                         ;; candidates, we still need the buffer to
                         ;; insert new candidates later, such as
                         ;; moving up from an empty directory. See
                         ;; #571.
                         (unless (get-buffer selectrum--display-action-buffer)
                           (selectrum--create-display-buffer))
                         (when selectrum--refined-candidates
                           (selectrum--get-display-window)))
                     (active-minibuffer-window)))
           (minibuf-after-string (or (selectrum--format-default) " "))
           (inserted-res
            (selectrum--insert-candidates
             window
             selectrum--virtual-input
             ;; FIXME: This only takes our count overlay into
             ;; account there might be other overlays prefixing the
             ;; prompt.
             (length count-info)))
           (horizp (car inserted-res))
           (inserted-string (cadddr inserted-res)))
      (setq-local selectrum--actual-num-candidates-displayed
                  (cadr inserted-res))
      (setq-local selectrum--first-index-displayed
                  (caddr inserted-res))
      (if selectrum-display-action
          ;; Insert candidates into action buffer.
          (with-current-buffer selectrum--display-action-buffer
            (erase-buffer)
            (insert inserted-string))
        ;; Candidates are shown in minibuffer, act accordingly.
        ;; Add padding for scrolled prompt.
        (unless (or horizp (zerop (window-hscroll window)))
          (setq inserted-string
                (replace-regexp-in-string
                 "^" (make-string (window-hscroll window) ?\s)
                 inserted-string)))
        ;; Add candidates to minibuffer string.
        (unless (or (zerop selectrum--actual-num-candidates-displayed)
                    (not selectrum--refined-candidates))
          (setq minibuf-after-string
                (concat minibuf-after-string inserted-string))))
      (move-overlay selectrum--candidates-overlay
                    (point-max) (point-max))
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
      (setq-local selectrum--is-initializing nil))))

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
        ((and vertical selectrum-fix-vertical-window-height)
         (let* ((max (selectrum--max-window-height))
                (lines (if selectrum-display-action
                           max
                         ;; Add one for prompt.
                         (1+ max)))
                ;; Include possible line spacing.
                (height (* lines selectrum--line-height)))
           (selectrum--set-window-height window height)))
        (t
         (when-let ((expand (selectrum--expand-window-for-content-p window)))
           (cond (selectrum-display-action
                  (selectrum--fit-window-to-buffer window))
                 (t
                  (selectrum--set-window-height window)))))))

(defun selectrum--fit-window-to-buffer (window)
  "Fit window height to its buffer contents.
Also works for frames if WINDOW is the root window of its frame."
  (let ((window-resize-pixelwise t)
        (window-size-fixed nil)
        (fit-frame-to-buffer 'vertically)
        (fit-window-to-buffer-horizontally nil))
    (fit-window-to-buffer window nil 1)))

(defun selectrum--set-window-height (window &optional height)
  "Set window height of WINDOW to HEIGHT pixel.
If HEIGHT is not given WINDOW will be updated to fit its content
vertically."
  (let ((dheight (or height (cdr (window-text-pixel-size window))))
        (wheight (window-pixel-height window))
        (window-resize-pixelwise t))
    (window-resize
     window (- dheight wheight) nil nil 'pixelwise)))

(defun selectrum--ensure-single-line (cand input settings)
  "Return single-line CAND string for INPUT.

Multi-line candidates are merged into a single line. The
resulting single-line candidates are then shortened by replacing
repeated whitespace and maybe truncating the result.

The specific details of the formatting are determined by
SETTINGS, see `selectrum-multiline-display-settings'."
  (let* (;; The formatting settings are the same for all multi-line
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
         (nlines/face (cdr nline/info))
         (lines (split-string cand "\n"))
         (len (length lines))
         (first-line
          (save-match-data
            (if (string-match "\\`\\(?:[ \t]*\n\\)*\\([^\n]*\\)" cand)
                (match-string 1 cand)
              cand)))
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
                         'face truncation/face)))))
    (concat shortened-line
            (propertize truncation/display
                        'face truncation/face)
            nlines
            (unless (string-empty-p shortened-matches)
              (propertize match/display
                          'face match/face))
            shortened-matches)))

(defun selectrum--annotation (fun cand face)
  "Return annotation for candidate.
Get annotation by calling FUN with CAND and apply FACE to it if
CAND does not have any face property defined."
  (when-let ((str (funcall fun cand)))
    (if (text-property-not-all 0 (length str) 'face nil str)
        str
      (propertize str 'face face))))

(defun selectrum--annotate (cands annotf docsigf)
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

(defun selectrum--replace-prop (str prop handler)
  "Replace STR parts with PROP using HANDLER.
HANDLER function can return a replacement for the part of string
which has the property value it receives as argument."
  (let ((len (length str))
        (pos 0)
        (chunks ()))
    (while (not (eq pos len))
      (let* ((end (next-single-property-change pos prop str len))
             (val (get-text-property pos prop str))
             (chunk (if-let ((rep (and val (funcall handler val))))
                        rep
                      (substring str pos end))))
        (push chunk chunks)
        (setq pos end)))
    (apply #'concat (nreverse chunks))))

(defun selectrum--display-string (str)
  "Return display string of STR.
Any string display specs in STR are replaced with the string they
will display as. This avoids prompt bleeding issues that occur
with display specs used within the after-string overlay.
Invisible parts of the display string are removed."
  (let ((str (selectrum--replace-prop
              str 'display (lambda (val) (when (stringp val) val)))))
    (selectrum--replace-prop str 'invisible (lambda (val) (when val "")))))

(defun selectrum--selection-highlight (str)
  "Return copy of STR with selection highlight."
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
  ;; <https://github.com/radian-software/selectrum/issues/21>
  ;; <https://github.com/radian-software/selectrum/issues/58>
  ;; <https://github.com/radian-software/selectrum/pull/76>
  (let ((str (copy-sequence str))
        (face 'selectrum-current-candidate))
    (if (version< emacs-version "27")
        (font-lock-prepend-text-property
         0 (length str)
         'face face str)
      (add-face-text-property
       0 (length str)
       face
       'append str))
    str))

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

(defun selectrum--highlighted-candidates (input first-index-displayed ncands)
  "Get highlighted candidates accoring to INPUT.
FIRST-INDEX-DISPLAYED is the index of the first candidate and
NCANDS the maximal number of candidates to get."
  (funcall
   selectrum-highlight-candidates-function
   input
   (seq-take
    (nthcdr
     first-index-displayed
     selectrum--refined-candidates)
    ;; Never allow more candidates than configured.
    (if (numberp selectrum-num-candidates-displayed)
        selectrum-num-candidates-displayed
      ncands))))

(defun selectrum--format-candidate
    (input candidate candidate-index current-index first-index
           &optional should-annotate)
  "Get display string of CANDIDATE.
INPUT is the input string, CANDIDATE-INDEX the display-index of
the CANDIDATE, CURRENT-INDEX the display-index of the selected
candidate in the UI and FIRST-INDEX is the first display-index
which is displayed in the UI."
  (let* ((hl (eq candidate-index current-index))
         (display-index (- candidate-index first-index))
         (single-line-candidate (if (string-match-p "\n" candidate)
                                    (selectrum--ensure-single-line
                                     candidate
                                     input
                                     selectrum-multiline-display-settings)
                                  candidate))
         (prefix (and should-annotate
                      (get-text-property
                       0 'selectrum-candidate-display-prefix
                       candidate)))
         (suffix (and should-annotate
                      (get-text-property
                       0 'selectrum-candidate-display-suffix
                       candidate)))
         (right-margin
          (and should-annotate
               (when-let (right-margin
                          (get-text-property
                           0 'selectrum-candidate-display-right-margin
                           candidate))
                 (concat (propertize
                          " "
                          'display
                          `(space
                            :align-to
                            (- right-fringe
                               ,(string-width right-margin)
                               ,selectrum-right-margin-padding)))
                         right-margin))))
         (displayed-candidate
          (selectrum--display-string
           single-line-candidate)))
    (when should-annotate
      (setq displayed-candidate
            (concat prefix displayed-candidate suffix right-margin)))
    ;; Add the ability to interact with candidates via the mouse.
    (add-text-properties
     0 (length displayed-candidate)
     (list
      'mouse-face 'selectrum-mouse-highlight
      'keymap
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap [mouse-1]
          `(lambda ()
             (interactive)
             (selectrum-select-current-candidate ,(1+ display-index))))
        (define-key keymap [mouse-3]
          `(lambda ()
             (interactive)
             (selectrum-insert-current-candidate ,(1+ display-index))))
        keymap))
     displayed-candidate)
    (when hl
      (setq displayed-candidate
            (selectrum--selection-highlight displayed-candidate)))
    (when-let (show-indices
               (cond
                ((functionp selectrum-show-indices) selectrum-show-indices)
                (selectrum-show-indices (lambda (i) (format "%2d " i)))))
      (setq displayed-candidate
            (concat
             (propertize
              (funcall show-indices (1+ display-index))
              'face 'minibuffer-prompt) displayed-candidate)))
    (when (and selectrum--quick-fun
               (not hl))
      (setq displayed-candidate
            (funcall selectrum--quick-fun display-index displayed-candidate)))
    displayed-candidate))

(defun selectrum--setup (candidates default buf)
  "Set up minibuffer for interactive candidate selection.
CANDIDATES is the list of candidate strings. DEFAULT is the
default value, see `minibuffer-default'. BUF is the buffer the
session was started from."
  (setq-local selectrum--last-buffer buf)
  (cond (selectrum--repeat
         (delete-minibuffer-contents)
         (insert
          (with-current-buffer
              (or (and (buffer-live-p selectrum--last-buffer)
                       selectrum--last-buffer)
                  (current-buffer))
            (or selectrum--last-input ""))))
        (t
         ;; Track globally and in last buffer.
         (setq-default selectrum--last-command this-command)
         (setq-default selectrum--last-prefix-arg current-prefix-arg)
         (when (buffer-live-p selectrum--last-buffer)
           (with-current-buffer selectrum--last-buffer
             (setq-local selectrum--last-command this-command)
             (setq-local selectrum--last-prefix-arg current-prefix-arg)))))
  (setq-local auto-hscroll-mode nil)
  (setq-local selectrum--is-initializing t)
  (setq-local selectrum--candidates-overlay
              (make-overlay (point) (point) nil
                            'front-advance 'rear-advance))
  (setq-local selectrum--count-overlay
              (make-overlay (point-min) (point-min)))
  ;; If metadata specifies a custom sort function use it as
  ;; `selectrum-preprocess-candidates-function' for this session.
  (when-let ((sortf (selectrum--get-meta 'display-sort-function)))
    (setq-local selectrum-preprocess-candidates-function sortf))
  (if (not (functionp candidates))
      (selectrum--preprocess candidates)
    (setq-local selectrum--preprocessed-candidates nil)
    (setq-local selectrum--dynamic-candidates-function candidates))
  (let ((default (or (car-safe default) default)))
    (setq-local selectrum--default-candidate
                (if (and default (symbolp default))
                    (symbol-name default)
                  default)))
  (setq-default selectrum--default-candidate
                selectrum--default-candidate)
  ;; Make sure to trigger an "user input changed" event, so that
  ;; candidate refinement happens in `post-command-hook' and an index
  ;; is assigned.
  (setq-local selectrum--previous-input-string nil)
  (setq-local selectrum--line-height (line-pixel-height))
  (add-hook 'post-command-hook #'selectrum--update nil 'local))

;;; Minibuffer commands
(defun selectrum-previous-candidate (&optional arg)
  "Move selection ARG candidates up.

If `selectrum-cycle-movement' is nil (the default), stop at the
end. Otherwise, loop back around."
  (interactive "p")
  (selectrum-next-candidate (- (or arg 1))))

(defun selectrum-next-candidate (&optional arg)
  "Move selection ARG candidates down.

If `selectrum-cycle-movement' is nil (the default), stop at the
end. Otherwise, loop back around.

If ARG is negative, move up instead of down."
  (interactive "p")
  (when selectrum--current-candidate-index
    (let ((new-pos (+ selectrum--current-candidate-index (or arg 1)))
          (max-index (1- (length (selectrum-get-current-candidates t))))
          (unselectable-input-line
           (and (selectrum--match-strictly-required-p)
                (cond (minibuffer-completing-file-name
                       (not (selectrum--at-existing-prompt-path-p)))
                      (t
                       (not (string-empty-p selectrum--virtual-input)))))))
      (setq selectrum--current-candidate-index
            (if (not selectrum-cycle-movement)
                ;; If we can't submit arbitrary input, prevent
                ;; moving to the input line.
                (selectrum--clamp new-pos
                                  (if unselectable-input-line 0 -1)
                                  max-index)
              (selectrum--cycle new-pos max-index
                                (not unselectable-input-line)))))))

(defun selectrum-previous-page (&optional arg)
  "Move selection upwards by ARG pages.

If `selectrum-cycle-movement' is nil (the default), stop at the
beginning. Otherwise, loop back around."
  (interactive "p")
  (selectrum-next-page (- (or arg 1))))

(defun selectrum-next-page (&optional arg)
  "Move selection downwards by ARG pages.

If `selectrum-cycle-movement' is nil (the default), stop at the
end. Otherwise, loop back around."
  (interactive "p")
  (when selectrum--current-candidate-index
    (let ((new-pos (+ selectrum--current-candidate-index
                      (* (or arg 1)
                         selectrum--actual-num-candidates-displayed)))
          (max-index (1- (length (selectrum-get-current-candidates t)))))
      ;; Paging commands cannot select the input line.
      (setq selectrum--current-candidate-index
            (if (not selectrum-cycle-movement)
                (selectrum--clamp new-pos 0 max-index)
              (selectrum--cycle new-pos max-index nil))))))

(defun selectrum-next-group (&optional arg)
  "Move selection downwards by ARG group(s).

If `selectrum-cycle-movement' is nil (the default), stop at the
last group. Otherwise, loop back around."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (cl-minusp arg)
      (selectrum-previous-group (abs arg))
    (when-let ((group-fn (selectrum--get-meta 'group-function)))
      (let* ((candidates (selectrum-get-current-candidates))
             (start-index selectrum--current-candidate-index)
             ;; These values updated in the loop:
             (group (funcall group-fn (nth start-index candidates) nil))
             (index (1+ start-index))
             (remaining-cands (nthcdr index candidates)))

        ;; If at the end and cycling, move to the beginning.
        (when (and (null remaining-cands)
                   selectrum-cycle-movement)
          (setq index 0
                remaining-cands candidates))

        (cl-block block
          ;; The common case of just moving to the immediate next group
          ;; can be done more simply then the general case of moving to
          ;; the ARG-th next group, so we add a special branch for it.
          (if (= arg 1)
              ;; Prevent infinite loop if we go through
              ;; completely.
              (while (and remaining-cands (/= index start-index))
                ;; When the found group is not equal to the starting
                ;; group
                (when (not (equal (funcall group-fn (car remaining-cands)
                                           nil)
                                  group ))
                  ;; then use the current index.
                  (setq selectrum--current-candidate-index index)
                  (cl-return-from block))
                (setq remaining-cands (cdr remaining-cands)
                      index (1+ index))
                (when (and (null remaining-cands)
                           selectrum-cycle-movement)
                  (setq remaining-cands candidates
                        index 0)))
            ;; In the general case, we need to check that the tested
            ;; candidate's is group is not equal to the previous
            ;; candidate's group. In this case, we must count how many
            ;; times we enter a new group, which may equal the starting
            ;; group if we've cycled around.
            ;;
            ;; If we can't cycle around, then we need to stop at the
            ;; last found group, similar to the effects of
            ;; `selectrum--clamp' as used in commands like
            ;; `selectrum-next-candidate'.
            (let ((prev-group nil)
                  (group-count 0)
                  (last-found-index nil))
              (while (and remaining-cands
                          ;; Prevent infinite loop if we go through
                          ;; completely.
                          (not (and (= index start-index)
                                    (zerop group-count))))
                (setq prev-group group
                      group (funcall group-fn (car remaining-cands) nil))
                ;; When the found group is not equal to the previous
                ;; group (i.e., we have reached the next group)
                (when (not (equal group prev-group))
                  ;; Increase the count of found groups.
                  (cl-incf group-count)
                  ;; Note the position of the new group.
                  (setq last-found-index index)
                  ;; And if the count of found groups equals arg,
                  ;; use the current index.
                  (when (= group-count arg)
                    (setq selectrum--current-candidate-index index)
                    (cl-return-from block)))
                (setq remaining-cands (cdr remaining-cands)
                      index (1+ index))
                ;; When we run out of candidates
                (when (null remaining-cands)
                  (cond
                   ;; Reset the loop if cycling
                   (selectrum-cycle-movement
                    (setq remaining-cands candidates
                          index 0))
                   ;; or use the last found group, if we found another
                   ;; group.
                   (last-found-index
                    (setq selectrum--current-candidate-index
                          last-found-index))))))))))))

(defun selectrum-previous-group (&optional arg)
  "Move selection upwards by ARG group(s).

If `selectrum-cycle-movement' is nil (the default), stop at the
first group. Otherwise, loop back around."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (cl-minusp arg)
      (selectrum-next-group (abs arg))
    (when-let ((group-fn (selectrum--get-meta 'group-function)))
      (let* ((candidates (selectrum-get-current-candidates))
             (start-index selectrum--current-candidate-index)
             (max-index (1- (length candidates)))
             ;; These values updated in the loop:
             (group (funcall group-fn (nth start-index candidates) nil))
             (candidates (reverse candidates))
             (index (1- start-index))
             (remaining-cands (last candidates start-index)))

        ;; If at the start and cycling, move to the end.
        (when (and (null remaining-cands)
                   selectrum-cycle-movement)
          (setq index max-index
                remaining-cands candidates))

        (cl-block block
          ;; The common case of just moving to the immediate previous group
          ;; can be done more simply then the general case of moving to
          ;; the ARG-th next group, so we add a special branch for it.
          (if (= arg 1)
              ;; Prevent infinite loop if we go through
              ;; completely.
              (while (and remaining-cands (/= index start-index))
                ;; When the found group is not equal to the starting
                ;; group
                (let ((test-group (funcall group-fn (car remaining-cands)
                                           nil)))
                  (when (not (equal test-group group))
                    ;; We have reached the end of the previous group.
                    ;; We now must move to the start of this group.
                    (setq remaining-cands (cdr remaining-cands)
                          index (1- index))
                    ;; Either we reach the end of the 2nd previous group
                    ;; (one after the start of the current group), or we
                    ;; run out of candidates.
                    (while (and remaining-cands
                                (equal test-group
                                       (funcall group-fn (car remaining-cands)
                                                nil)))
                      (setq remaining-cands (cdr remaining-cands)
                            index (1- index)))
                    ;; Either way, the start of `test-group' will be one
                    ;; more than the current index.
                    (setq selectrum--current-candidate-index (1+ index))
                    (cl-return-from block)))
                (setq remaining-cands (cdr remaining-cands)
                      index (1- index))
                (when (and (null remaining-cands)
                           selectrum-cycle-movement)
                  (setq remaining-cands candidates
                        index max-index)))
            ;; In the general case, we need to check that the tested
            ;; candidate's is group is not equal to the previous
            ;; candidate's group. In this case, we must count how many
            ;; times we enter a new group, which may equal the starting
            ;; group if we've cycled around.
            ;;
            ;; If we can't cycle around, then we need to stop at the
            ;; beginning of the last found group, similar to the effects
            ;; of `selectrum--clamp' as used in commands like
            ;; `selectrum-next-candidate'. If we find any group, then we
            ;; assume that the beginning of the first group is just the
            ;; first candidate. An "ungrouped" group, of sorts.
            (let ((prev-group nil)
                  (group-count 0)
                  (group-found nil))
              (while (and remaining-cands
                          ;; Prevent infinite loop if we go through
                          ;; completely.
                          (not (and (= index start-index)
                                    (zerop group-count))))
                (setq prev-group group
                      group (funcall group-fn (car remaining-cands) nil))
                ;; When the found group is not equal to the previous
                ;; group (i.e., we have reached the next group)
                (when (not (equal group prev-group))
                  ;; Increase the count of found groups.
                  (cl-incf group-count)
                  ;; Note that we found a group.
                  (setq group-found t)
                  ;; And if the count of found groups equals arg, we have
                  ;; reached the end of the desired previous group. We now
                  ;; must move to the start of this group.
                  (when (= group-count arg)
                    ;; Either we reach the end of the (ARG+1)-th previous
                    ;; group (one after the start of the current group),
                    ;; or we run out of candidates.
                    (setq remaining-cands (cdr remaining-cands)
                          index (1- index))
                    (while (and remaining-cands
                                (equal group
                                       (funcall group-fn (car remaining-cands)
                                                nil)))
                      (setq remaining-cands (cdr remaining-cands)
                            index (1- index)))
                    ;; Either way, the start of `test-group' will be one
                    ;; more than the current index.
                    (setq selectrum--current-candidate-index (1+ index))
                    (cl-return-from block)))
                (setq remaining-cands (cdr remaining-cands)
                      index (1- index))
                ;; When we run out of candidates
                (when (null remaining-cands)
                  (cond
                   ;; Reset the loop if cycling
                   (selectrum-cycle-movement
                    (setq remaining-cands candidates
                          index max-index))
                   ;; or, if we found a group, go to the first candidate.
                   (group-found
                    (setq selectrum--current-candidate-index 0))))))))))))

(defun selectrum-goto-beginning ()
  "Move selection to first candidate."
  (interactive)
  (when selectrum--current-candidate-index
    (setq-local selectrum--current-candidate-index 0)))

(defun selectrum-goto-end ()
  "Move selection to last candidate."
  (interactive)
  (when selectrum--current-candidate-index
    (setq-local selectrum--current-candidate-index
                (1- (length selectrum--refined-candidates)))))

(defun selectrum-backward-kill-sexp (&optional arg)
  "Selectrum wrapper for `backward-kill-sexp'.
ARG is the same as for `backward-kill-sexp'."
  (interactive "p")
  ;; For Selectrum file prompts `backward-kill-sexp' would wrongly
  ;; include trailing (read only) spaces as part of the input (see
  ;; `selectrum--minibuffer-local-filename-syntax').
  (save-restriction
    (narrow-to-region (minibuffer-prompt-end) (point-max))
    (backward-kill-sexp arg)))

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
If `selectrum--is-crm-session' is non-nil exit with the choosen candidates
plus CANDIDATE."
  (let* ((result
          (cond ((and selectrum--is-crm-session
                      (string-match crm-separator
                                    selectrum--previous-input-string))
                 (let* ((previous-input-string
                         selectrum--previous-input-string)
                        (separator
                         crm-separator)
                        (full-candidate
                         (selectrum--get-full candidate))
                        (crm
                         (if (or (not selectrum--current-candidate-index)
                                 (< selectrum--current-candidate-index
                                    0))
                             candidate
                           (with-temp-buffer
                             (insert previous-input-string)
                             (goto-char (point-min))
                             (while (re-search-forward
                                     separator nil t))
                             (delete-region (point) (point-max))
                             (insert full-candidate)
                             (buffer-string)))))
                   (dolist (cand (split-string crm separator t))
                     (run-hook-with-args
                      'selectrum-candidate-selected-hook
                      (selectrum--get-full cand)))
                   crm))
                (t
                 (run-hook-with-args
                  'selectrum-candidate-selected-hook
                  candidate)
                 (selectrum--get-full candidate))))
         (inhibit-read-only t))
    (delete-minibuffer-contents)
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
    (or selectrum--current-candidate-index -1)))

(defun selectrum-select-current-candidate (&optional arg)
  "Exit minibuffer, picking the currently selected candidate.
If there are no candidates, return the current user input, unless
a match is required, in which case do nothing.

Give a prefix argument ARG to select the nth displayed candidate.
Zero means to select the current user input. See
`selectrum-show-indices' which can be used to show candidate
indices."
  (interactive "P")
  (unless selectrum-is-active
    (user-error "Cannot select a candidate when Selectrum is not active"))
  (with-selected-window (active-minibuffer-window)
    (let* ((index (selectrum--index-for-arg arg))
           (valid-prompt-selection
            (and (< index 0)
                 (or (string-empty-p selectrum--virtual-input)
                     (if minibuffer-completing-file-name
                         (selectrum--at-existing-prompt-path-p)
                       (member (minibuffer-contents)
                               selectrum--refined-candidates))))))
      (cond ((and (< index 0)
                  (not valid-prompt-selection)
                  ;; There is no try-completion action in Selectrum,
                  ;; so `confirm-after-completion' is ignored.
                  (eq selectrum--match-is-required 'confirm)
                  (not (eq last-command this-command)))
             (minibuffer-message
              (propertize "Confirm" 'face 'minibuffer-prompt)))
            ((or (not (selectrum--match-strictly-required-p))
                 valid-prompt-selection
                 (>= index 0))
             (selectrum--exit-with
              (selectrum--get-candidate index)))
            ;; For dynamic tables we currently don't update the
            ;; candidates after each input. The following clause
            ;; ensures one can still exit out of such a prompt when a
            ;; valid candidate is in the input.
            ((and (functionp minibuffer-completion-table)
                  (eq t (test-completion (minibuffer-contents)
                                         minibuffer-completion-table)))
             (selectrum--exit-with (minibuffer-contents)))
            (t
             (minibuffer-message
              (propertize "Match required" 'face 'minibuffer-prompt)))))))

(defun selectrum-submit-exact-input ()
  "Exit minibuffer, using the current user input.
This differs from `selectrum-select-current-candidate' in that it
ignores the currently selected candidate, if one exists."
  (interactive)
  (let ((selectrum--current-candidate-index -1))
    (selectrum-select-current-candidate)))

(defun selectrum--reset-minibuffer-history-state ()
  "Reset history for current prompt."
  (setq-local minibuffer-history-position 0)
  (setq-local minibuffer-text-before-history
              (minibuffer-contents-no-properties)))

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
            (cond ((not selectrum--is-crm-session)
                   (delete-region (minibuffer-prompt-end)
                                  (point-max))
                   (insert full))
                  (t
                   ;; Select input after crm insertion.
                   (setq-local selectrum--repeat t)
                   (setq-local selectrum--current-candidate-index -1)
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
            (run-hook-with-args
             'selectrum-candidate-inserted-hook
             candidate))
          ;; Ensure refresh of UI. The input input string might be the
          ;; same when the prompt was reinserted. When the prompt was
          ;; selected this will switch selection to first candidate.
          (setq-local selectrum--previous-input-string nil)
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
           (hist-buf nil)
           (result
            (cl-letf* ((orig
                        (symbol-function #'selectrum-insert-current-candidate))
                       ((symbol-function #'selectrum-insert-current-candidate)
                        (lambda (&optional arg)
                          (interactive "P")
                          (if (eq hist-buf (current-buffer))
                              (selectrum--insert-history arg)
                            (funcall orig arg)))))
              (selectrum--minibuffer-with-setup-hook
                  (lambda ()
                    (setq hist-buf (current-buffer))
                    (setq-local selectrum-should-sort nil)
                    (setq-local selectrum-candidate-inserted-hook nil)
                    (setq-local selectrum-candidate-selected-hook nil)
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
                   (minibuffer-prompt) history nil t nil t))))))
      (if (get-text-property 0 'selectum--insert result)
          (progn
            (delete-minibuffer-contents)
            (insert result)
            (selectrum--reset-minibuffer-history-state))
        (if (and (selectrum--match-strictly-required-p)
                 (not (member result selectrum--refined-candidates)))
            (user-error "That history element is not one of the candidates")
          (selectrum--exit-with result))))))

(defun selectrum--insert-history (&optional arg)
  "Insert history item.
Only to be used from `selectrum-select-from-history'. Give a
prefix argument ARG to select the nth displayed candidate. Zero
means to select the current user input."
  (interactive "P")
  (let* ((index (selectrum--index-for-arg arg))
         (cand
          (selectrum--get-full
           (selectrum--get-candidate index))))
    (throw 'selectrum-insert-action
           (propertize cand 'selectum--insert t))))

(defun selectrum--quick-keys (len keys)
  "Get list of key combinations up to key length LEN.
KEYS is a list of key strings to combine."
  (unless (zerop len)
    (cl-loop with list = keys
             with olist = keys
             repeat (1- len)
             do (setq olist
                      (cl-loop
                       for char in list
                       nconc (cl-loop for ochar in olist
                                      collect (concat char ochar))))
             finally return olist)))

(defun selectrum--quick-read ()
  "Read index interactively using `selectrum-quick-keys'."
  (unless (cdr selectrum-quick-keys)
    (user-error "`selectrum-quick-keys' needs at least two keys"))
  (when (< selectrum--actual-num-candidates-displayed 2)
    (user-error "No candidates for quick selection"))
  (let* ((qkeys (mapcar #'char-to-string selectrum-quick-keys))
         (nkeys (length qkeys))
         (needed selectrum--actual-num-candidates-displayed)
         (len (ceiling (log needed nkeys)))
         (keys (seq-take (selectrum--quick-keys len qkeys) needed))
         (input nil)
         (selectrum--quick-fun
          (lambda (i cand)
            (let ((str (propertize (or (nth i keys) "")
                                   'face 'selectrum-quick-keys-highlight)))
              (when (and input (string-match (concat "\\`" input) str))
                (setq str (copy-sequence str))
                (add-face-text-property 0 (match-end 0)
                                        'selectrum-quick-keys-match nil str))
              (concat str (if (eq (car selectrum-display-style) 'vertical)
                              cand
                            (substring cand (min (length cand)
                                                 (length str)))))))))
    (if-let* ((input
               (cl-loop with pressed = 0
                        while (< pressed len)
                        do (selectrum--update)
                        for ev = (unwind-protect (read-key)
                                   (let ((selectrum--quick-fun nil))
                                     (selectrum--update)))
                        if (not (characterp ev))
                        return (vector ev)
                        for key = (char-to-string ev)
                        if (and (not (zerop pressed))
                                (equal ev ?\C-?))
                        do (setq pressed (1- pressed)
                                 input (substring
                                        input 0 (1- (length input))))
                        else if (not (member key qkeys))
                        return key
                        else
                        do (setq pressed (1+ pressed)
                                 input (concat input key))
                        finally return input))
              (pos (and (stringp input)
                        (cl-position input keys :test #'string=))))
        (+ selectrum--first-index-displayed pos)
      (prog1 nil
        (unless (and (vectorp input)
                     (memq (key-binding input)
                           '(selectrum-quick-select
                             selectrum-quick-insert)))
          (let* ((desc (key-description input))
                 (msg (if (equal "C-g" desc)
                          "Quit"
                        (format "No matching key: %S" desc))))
            (minibuffer-message
             (propertize msg 'face 'minibuffer-prompt))))))))

(defun selectrum-quick-select ()
  "Select a candidate using `selectrum-quick-keys'."
  (interactive)
  (when-let (index (selectrum--quick-read))
    (setq-local selectrum--current-candidate-index index)
    (selectrum-select-current-candidate)))

(defun selectrum-quick-insert ()
  "Insert a candidate using `selectrum-quick-keys'."
  (interactive)
  (when-let (index (selectrum--quick-read))
    (setq-local selectrum--current-candidate-index index)
    (selectrum-insert-current-candidate)))

;;; Main entry points

(cl-defun selectrum--read
    (prompt candidates &rest args &key
            default-candidate initial-input require-match
            history mc-table mc-predicate mc-allow-text-properties)
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
it's present. If can be a symbol, a string, or a list of those.
In case of a list the first candidate of the list gets sorted
first.

INITIAL-INPUT, if provided, is inserted into the user input area
initially (with point at the end).

REQUIRE-MATCH, if `confirm' or `confirm-after-completion' the
user needs to confirm the input if it isn't a member of the
candidates. Any other non-nil value won't let the user exit in
this case.

HISTORY is the `minibuffer-history-variable' to use (by default
`minibuffer-history').

For MC-TABLE and MC-PREDICATE see `minibuffer-completion-table'
and `minibuffer-completion-predicate'. They are used for internal
purposes and compatibility to Emacs completion API. They will be
locally in the minibuffer.

If MC-ALLOW-TEXT-PROPERTIES is non-nil, candidates selected from
function completion tables will always keep their text
properties. This argument is for internal purposes. The default
behavior, complying with that of `completing-read-default', is to
strip text properties from candidates except when selecting the
default candidate by submitting empty input."
  (let* ((minibuffer-allow-text-properties t)
         (resize-mini-windows 'grow-only)
         (prompt (selectrum--remove-default-from-prompt prompt))
         ;; <https://github.com/radian-software/selectrum/issues/99>
         (icomplete-mode nil)
         (buf (current-buffer))
         (res
          (selectrum--minibuffer-with-setup-hook
              (lambda ()
                ;; Already set the active flag as early as possible
                ;; so client setup hooks can use it to detect if
                ;; they are running in a Selectrum session.
                (setq-local selectrum-is-active t)
                (setq-local minibuffer-completion-table mc-table)
                (setq-local minibuffer-completion-predicate mc-predicate))
            (selectrum--minibuffer-with-setup-hook
                (:append (lambda ()
                           (setq-local selectrum--match-is-required
                                       require-match)
                           (selectrum--setup
                            candidates
                            (or minibuffer-default default-candidate)
                            buf)))
              (read-from-minibuffer
               prompt initial-input selectrum-minibuffer-map nil
               (or history 'minibuffer-history) default-candidate)))))
    (cond (mc-table
           ;; Behave like completing-read-default which strips the
           ;; text properties but leaves the default unchanged
           ;; when submitting the empty prompt to get it (see
           ;; #180, #107).
           (let ((exit-string (default-value 'selectrum--last-input))
                 (default (default-value 'selectrum--default-candidate)))
             (if (and exit-string
                      (string-empty-p exit-string)
                      (equal res default))
                 (or (car-safe default-candidate)
                     default-candidate)
               (if mc-allow-text-properties
                   res
                 (substring-no-properties res)))))
          (t res))))

;;;###autoload
(defun selectrum-completing-read
    (prompt collection &optional
            predicate require-match initial-input
            hist def _inherit-input-method)
  "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'."
  (when (consp initial-input)
    (setq initial-input
          (cons (car initial-input)
                ;; `completing-read' uses 0-based index while
                ;; `read-from-minibuffer' uses 1-based index, see
                ;; `completing-read-default'.
                (1+ (cdr initial-input)))))
  (selectrum--read
   prompt nil
   :initial-input initial-input
   :default-candidate def
   :require-match require-match
   :history hist
   :mc-table collection
   :mc-predicate predicate))

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
              (let ((coll (seq-remove
                           (lambda (i)
                             (member i inputs))
                           coll))
                    (ninput (substring input beg)))
                `((input . ,ninput)
                  (candidates . ,coll))))))
         (res nil))
    (setq
     res
     (selectrum--minibuffer-with-setup-hook
         (lambda ()
           (setq-local selectrum--is-crm-session t)
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
       (selectrum--read
        prompt
        candidates
        :require-match require-match
        :initial-input initial-input
        :history hist
        :default-candidate def
        :mc-table table
        :mc-predicate predicate)))
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
         (threshold (completion--cycle-threshold meta))
         (category (completion-metadata-get meta 'category))
         (styles (completion--styles meta))
         (bound (pcase category
                  ('file start)
                  (_ (+ start (car (completion-boundaries
                                    input collection predicate ""))))))
         (exit-func (plist-get completion-extra-properties
                               :exit-function))
         (cands (if (not selectrum-completion-in-region-styles)
                    (or (let ((completion-regexp-list nil))
                          (all-completions input collection predicate))
                        (nconc
                         (let ((completion-styles styles))
                           (completion-all-completions
                            input collection predicate
                            (- end start) meta))
                         nil))
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
    (cond ((null cands)
           (prog1 nil
             (unless completion-fail-discreetly (ding))
             (message "No match")))
          ((or (eq t threshold)
               (and (numberp threshold)
                    (not (nthcdr threshold cands))))
           (let ((cands (lambda (str pred action)
                          (if (eq action t)
                              cands
                            (complete-with-action
                             action collection str pred)))))
             ;; Used default completion for cycling.
             (setq completion-in-region-function
                   (lambda (&rest args)
                     (if (eq last-command #'completion-at-point)
                         (apply #'completion--in-region args)
                       (setq completion-in-region-function
                             #'selectrum-completion-in-region)
                       (apply #'selectrum-completion-in-region args))))
             (completion--in-region start end cands predicate)))
          (t
           (prog1 t
             (pcase category
               ('file
                (let* ((try (and (not (cdr cands))
                                 (try-completion
                                  input collection predicate)))
                       (comp (and (stringp try)
                                  try))
                       (initial (if (string-empty-p input)
                                    (abbreviate-file-name default-directory)
                                  input))
                       (path
                        (or comp
                            (selectrum--completing-read-file-name
                             "Completion: " collection predicate
                             nil initial))))
                  (setq result (if (and (derived-mode-p 'comint-mode)
                                        (not comp)
                                        (fboundp 'comint-quote-filename))
                                   (comint-quote-filename path)
                                 path)
                        exit-status 'sole)))
               (_
                (setq result
                      (if (not (cdr cands))
                          (car cands)
                        (selectrum--read
                         "Completion: " cands
                         :initial-input input
                         :mc-table collection
                         :mc-predicate predicate))
                      exit-status (cond ((not (member result cands)) 'sole)
                                        (t 'finished)))))
             (delete-region bound end)
             (push-mark (point) 'no-message)
             (insert (substring-no-properties result))
             (when exit-func
               (funcall exit-func result exit-status)))))))

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
  (let* ((buffalist (mapcar (lambda (buf)
                              (cons (buffer-name buf) buf))
                            (buffer-list)))
         (buffers (mapcar #'car (if predicate
                                    (cl-delete-if-not predicate buffalist)
                                  buffalist)))
         (candidates
          (lambda (input)
            (if (string-prefix-p " " input)
                `((candidates . ,(seq-filter
                                  (lambda (name)
                                    (string-prefix-p " " name))
                                  buffers))
                  (input . ,(substring input 1)))
              `((candidates . ,(seq-remove
                                (lambda (name)
                                  (string-prefix-p " " name))
                                buffers)))))))
    (selectrum--minibuffer-with-setup-hook
        (lambda ()
          (setq-local selectrum-should-sort nil)
          (setq-local selectrum-move-default-candidate nil))
      (selectrum--read
       prompt candidates
       :default-candidate def
       :require-match require-match
       :history 'buffer-name-history
       :mc-table #'internal-complete-buffer
       :mc-predicate predicate))))

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
              (when (and selectrum-files-select-input-dirs
                         (directory-name-p path))
                (setq-local selectrum--repeat t)
                (setq-local selectrum--current-candidate-index -1))
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
      (selectrum--read
       prompt coll
       :default-candidate def
       :initial-input (or (car-safe initial-input) initial-input)
       :history hist
       :require-match require-match
       :mc-table collection
       :mc-predicate predicate))))

;;;###autoload
(defun selectrum-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'."
  (let* ((crf completing-read-function)
         ;; See <https://github.com/radian-software/selectrum/issues/61>.
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
                     ;; Weird indentation to make things indent the
                     ;; same in both Emacs 28 and Emacs 29, because
                     ;; apparently something changed?
                     ( default-in-prompt-dir
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

;;;###autoload
(defun selectrum--fix-dired-read-dir-and-switches (func &rest args)
  "Make Dired do the \"right thing\" with its default candidate.
By default Dired uses `read-file-name' internally, which causes
Selectrum to provide you with the first file inside the working
directory as the default candidate. However, it would arguably be
more semantically appropriate to use `read-directory-name', and
this is especially important for Selectrum since this causes it
to select the working directory initially.

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

;; In Emacs 28.1, `find-function-source-path' was renamed to
;; `find-library-source-path'.
(defvar find-library-source-path)
(defvar find-function-source-path)

;;;###autoload
(defun selectrum-read-library-name ()
  "Read and return a library name.

Similar to `read-library-name' except it handles `load-path'
shadows correctly. Interactively, this function assumes that a
directory does not contain multiple versions of the same library.

While only library names are displayed interactively, file names
will be used as fallback candidates to accept the same input as
the built-in `read-library-name'."
  (eval-and-compile
    (require 'find-func))
  (let ((suffix-regexp
         ;; "elc" is excluded by `find-library-suffixes', but we want
         ;; to include it for candidates like `map.elc'.  Normal
         ;; completion doesn't seem to have that issue.
         (concat (regexp-opt (cons ".elc" (find-library-suffixes)))
                 "\\'"))
        (table (make-hash-table :test #'equal))
        (lib-name-cands nil)      ; Cleaned-up and disambiguated cands
        (file-name-cands nil))    ; Other candidates
    (dolist (dir (or (symbol-value
                      (if (boundp 'find-library-source-path)
                          ;; In Emacs 28.1, `find-function-source-path'
                          ;; was renamed to `find-library-source-path'.
                          'find-library-source-path
                        'find-function-source-path))
                     load-path))
      (condition-case _
          (let ((found-libs))
            (mapc
             (lambda (entry)
               (unless (string-match-p "^\\.\\.?$" entry)
                 (let* ((file-name (file-name-nondirectory entry))
                        (base (string-trim-right
                               file-name suffix-regexp)))
                   ;; We want to visually disambiguate libraries
                   ;; for which there are multiple versions in the
                   ;; load path.  In normal Selectrum completion,
                   ;; we only want to display a single candidate
                   ;; for each directory in the load path in which the
                   ;; library is located.
                   ;;
                   ;; For whatever reason, returning a path to an ELC
                   ;; file always opens the loaded version of the
                   ;; library, regardless of where the ELC file is
                   ;; located, so we ignore them for these lib-name
                   ;; candidates.
                   (unless (or (member base found-libs)
                               (string-match-p "\\.elc\\'" file-name))
                     (push entry (gethash base table))
                     (push base found-libs))
                   ;; Besides the bare library name (e.g., "cl-lib"),
                   ;; default completion also shows the actual file
                   ;; names (e.g., "cl-lib.el.gz" and "cl-lib.elc").
                   ;;
                   ;; We don't really need to show these for
                   ;; normal interactive use, but we must support them
                   ;; for libraries like Embark (see #577, #580).
                   (unless (member file-name file-name-cands)
                     (push (propertize file-name
                                       'fixedcase 'literal
                                       'selectrum--lib-path entry)
                           file-name-cands)))))
             (directory-files dir 'full suffix-regexp 'nosort)))
        (file-error)))
    ;; Now that we have a table of symbols and lists of files, we want
    ;; to create candidates with minimally unique display prefixes,
    ;; such as "folder1/library-symbol" and "folder2/library-symbol".
    ;;
    ;; The prefixes are made from the final components of the full
    ;; path to each found file, and we keep adding components from
    ;; earlier in the file path to the prefix until we have as many
    ;; unique prefixes as there are entries in `paths' (or until the
    ;; maximum number of components has been used).
    ;;
    ;; This approach assumes that each directory containing a version
    ;; of `lib-name' is only represented once in `paths', which we
    ;; ensured earlier while processing the found file names.
    (maphash
     (lambda (lib-name paths)
       (setq paths (nreverse paths)) ; Move shadowed paths to end.
       (cl-block nil
         (let ((num-components 1)
               (max-components
                (apply #'max (mapcar (lambda (path)
                                       (1+ (cl-count ?/ path)))
                                     paths))))
           (while t
             (let ((abbrev-paths
                    (seq-uniq
                     (mapcar (lambda (path)
                               (string-trim-right
                                (selectrum--trailing-components
                                 num-components path)
                                suffix-regexp))
                             paths))))
               (when (or (= num-components max-components)
                         (= (length paths) (length abbrev-paths)))
                 (let ((candidate-paths
                        (mapcar (lambda (path)
                                  (propertize
                                   lib-name
                                   'selectrum-candidate-display-prefix
                                   (file-name-directory
                                    (file-name-sans-extension
                                     (selectrum--trailing-components
                                      num-components path)))
                                   'fixedcase 'literal
                                   'selectrum--lib-path path))
                                paths)))
                   (setq lib-name-cands
                         (nconc candidate-paths lib-name-cands)))
                 (cl-return)))
             (cl-incf num-components)))))
     table)
    ;; If the user chooses a matching candidate with
    ;; `selectrum-submit-exact-input', then the returned candidate
    ;; won't have the needed property. In that case, we select the
    ;; first item in the list of propertized candidates that is
    ;; `equal' to the selected candidate. See issue #577.
    (let* ((all-cands (append lib-name-cands file-name-cands))
           (chosen-cand
            (selectrum--read "Library name: "  nil
                             :require-match t
                             :mc-allow-text-properties t
                             :mc-table (completion-table-in-turn
                                        lib-name-cands
                                        file-name-cands))))
      (or (get-text-property 0 'selectrum--lib-path chosen-cand)
          ;; Find the first candidate in `all-cands' that equals
          ;; the chosen candidate output.
          (cl-loop for lib-cand in all-cands
                   when (equal chosen-cand lib-cand)
                   return (get-text-property 0 'selectrum--lib-path
                                             lib-cand))))))

(defun selectrum-repeat ()
  "Repeat the last command that used Selectrum, and try to restore state."
  (interactive)
  (unless selectrum--last-command
    (user-error "No Selectrum command has been run yet"))
  (setq current-prefix-arg selectrum--last-prefix-arg
        this-command selectrum--last-command)
  (selectrum--minibuffer-with-setup-hook
      (lambda ()
        (setq-local selectrum--repeat t))
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
  (if (bound-and-true-p selectrum-is-active)
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
        (apply func args))
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
    :group 'selectrum
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

;;; Closing remarks

(provide 'selectrum)

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; selectrum.el ends here
