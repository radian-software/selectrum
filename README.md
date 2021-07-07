[![MELPA](https://melpa.org/packages/selectrum-badge.svg)](https://melpa.org/#/selectrum)
[![MELPA Stable](https://stable.melpa.org/packages/selectrum-badge.svg)](https://stable.melpa.org/#/selectrum)

# Selectrum

[News feed](https://github.com/raxod502/selectrum/commits/master/CHANGELOG.md.atom)

*Selectrum is a better solution for incremental narrowing in Emacs,
replacing [Helm](https://github.com/emacs-helm/helm),
[Ivy](https://github.com/abo-abo/swiper#ivy), and
[Ido](https://www.gnu.org/software/emacs/manual/html_node/ido/index.html)*.

<!-- longlines-start -->

<!-- toc -->

- [What is it?](#what-is-it)
- [Installation](#installation)
- [Usage](#usage)
      - [Alternative 1: Prescient](#alternative-1-prescient)
      - [Alternative 2: Orderless](#alternative-2-orderless)
- [User guide](#user-guide)
  * [Keybindings](#keybindings)
  * [Sorting and filtering](#sorting-and-filtering)
  * [Additional features](#additional-features)
  * [Customization](#customization)
  * [Complementary extensions](#complementary-extensions)
  * [But what is it doing to my Emacs??](#but-what-is-it-doing-to-my-emacs)
  * [News](#news)
- [Developer guide](#developer-guide)
  * [Usage of Selectrum](#usage-of-selectrum)
  * [Sorting, filtering, and highlighting](#sorting-filtering-and-highlighting)
  * [Text properties](#text-properties)
  * [Hooks](#hooks)
  * [Variables](#variables)
- [Contributor guide](#contributor-guide)
- [Caveats](#caveats)
- [Selectrum in comparison to other completion-systems](#selectrum-in-comparison-to-other-completion-systems)
  * [Ido](#ido)
  * [Helm](#helm)
  * [Ivy](#ivy)
  * [Icomplete](#icomplete)
  * [Vertico](#vertico)
  * [Icicles](#icicles)
  * [Snails](#snails)
  * [Sallet](#sallet)
  * [Raven](#raven)
  * [Swiper](#swiper)

<!-- tocstop -->

<!-- longlines-stop -->

## What is it?

Selectrum aims to provide a better completion UI using standard Emacs
APIs. In essence it is an interface for selecting items from a list.

You can use it to run a command with `M-x`:

<p align="center"> <img src="images/commands.png" alt="Picking from a
list of commands" height="200"/> </p>

You can use it to open a file with `C-x C-f` (`find-file`):

<p align="center"> <img src="images/files.png" alt="Navigating the
filesystem" height="200"/> </p>

Even [TRAMP](https://www.gnu.org/software/tramp/#Quick-Start-Guide)
works great out of the box:

<p align="center"> <img src="images/tramp.png" alt="Using sudo via
TRAMP" height="200"/> </p>

You can switch buffers:

<p align="center"> <img src="images/buffers.png" alt="Switching to
another buffer" height="200"/> </p>

And every other command in Emacs is automatically enhanced, without
the need for any configuration:

<p align="center"> <img src="images/libraries.png" alt="Finding
libraries, with load-path shadows" height="200"/> </p>

## Installation

Selectrum is [available as a package on
MELPA](https://melpa.org/#/selectrum). The easiest way to install this
package is using
[`straight.el`](https://github.com/raxod502/straight.el):

```elisp
(straight-use-package 'selectrum)
```

However, you may install using any other package manager if you
prefer.

## Usage

To enable Selectrum, simply add to your init-file:

```elisp
(selectrum-mode +1)
```

Now all completion commands will automatically use Selectrum.

The focus of Selectrum is on providing an enhanced completion UI and
compose with [other
packages](https://github.com/raxod502/selectrum#complementary-extensions)
which stay within the constraints of the standard Emacs API. Because
of the modular approach there are several possible package
combinations. Many tips and setup help for integration with other
packages can be found in our
[wiki](https://github.com/raxod502/selectrum/wiki/Additional-Configuration).

The default sorting method of Selectrum is simple and predictable. The
candidates are first sorted by their history position, then by length
and then alphabetically.

The default filtering of Selectrum uses the Emacs `completion-styles`.
The default setting of the `completion-styles` variable is rather
"basic" and you may want to adjust this variable for more advanced
filtering. See for example the built-in `substring` and `flex` styles.
Instead of using the built-in completion styles we recommended to use
additional packages. Here we highlight two possible approaches for
more advanced filtering and sorting: 1. Prescient and 2. Orderless.

##### Alternative 1: Prescient

Filtering and sorting can both be improved by installing the
[`selectrum-prescient`](https://github.com/raxod502/prescient.el)
package from MELPA and adding the following to your init-file.

```elisp
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)

;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)
```

* Your candidates are sorted by *frecency*. Your most recent choices
  are saved, and those are sorted first. After that, your most
  frequent choices are saved, and those are sorted next. The rest of
  the candidates are sorted by length. This algorithm turns out to do
  very well in practice while being fast and not very magical.
* Your input is split on spaces into subqueries, each of which must
  (by default) match as either a substring, a regexp, or an initialism
  (e.g. `ffap` matches `find-file-at-point`). The subqueries can match
  a candidate in any order, but a candidate must match all subqueries
  in order to remain in the filtered list of candidates.
  * Other matching styles are available in addition to the default
    three, and custom styles can be added by users.
  * Filtering features can be toggled on the fly, such as whether to
    use character/case folding or which matching styles are active.
  * Optionally, fully matched candidates can be listed before
    partially matched candidates while keeping the frecency-based
    sorting.
* The parts of each candidate that matched your input are highlighted,
  with important sections within each part (such as the initials of an
  initialism) highlighted in a second color.

##### Alternative 2: Orderless

Another popular choice for filtering is to use the flexible
[`orderless`](https://github.com/oantolin/orderless) completion style.

```elisp
(setq completion-styles '(orderless))

;; Persist history over Emacs restarts
(savehist-mode)

;; Optional performance optimization
;; by highlighting only the visible candidates.
(setq orderless-skip-highlighting (lambda () selectrum-is-active))
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
```

The candidates are sorted using the default sorting method of
Selectrum (by *recency*). The history is persisted using the Emacs
built-in `savehist-mode`. Afterwards the candidates are filtered and
highlighted using the `completion-styles`, in this case `orderless`.

In some cases you may want to consider to use Prescient on top of
Orderless. Prescient can be used to provide *frecency*-based sorting and
history persistence by adding the following.

```elisp
(setq selectrum-prescient-enable-filtering nil)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
```

## User guide

The design philosophy of Selectrum is to be as simple as possible,
because selecting an item from a list really doesn't have to be that
complicated, and you don't have time to learn all the hottest tricks
and keybindings for this. What this means is that Selectrum always
prioritizes consistency, simplicity, and understandability over making
optimal choices for workflow streamlining. The idea is that when
things go wrong, you'll find it easy to understand what happened and
how to fix it.

### Keybindings

* *To navigate to a candidate:* use the standard motion commands
  (`<up>`, `<down>`, `C-v`, `M-v`, `M-<`, `M->`). If you prefer, you
  can use `C-p` and `C-n` instead of the arrow keys.
* *To accept the currently selected candidate:* type `RET`/`C-m`.
  (With a prefix argument, accept instead the candidate at that point
  in the list, counting from one. See `selectrum-show-indices`. The
  value zero means to accept exactly what you've typed, as in the next
  bullet point.) You can also click the left mouse button on a
  candidate to choose it or use `M-m` to select one using
  `selectrum-quick-keys`.
* *To submit what you've typed, even if it's not a candidate:* you can
  use `<up>` or `C-p` to select the user input just like a regular
  candidate, and type `RET` as usual. (Alternatively, you can type
  `C-j` to submit your exact input without selecting it first.)
* *To abort:* as per usual, type `C-g`.
* *To navigate into the currently selected directory while finding a
  file\:* type `TAB`/`C-i`. (What this actually does is insert the
  currently selected candidate into the minibuffer, which for
  `find-file` has the effect of navigating into a directory.) With a
  positive prefix argument, insert the candidate at that display
  position (see `selectrum-show-indices`). You can also right click on
  a candidate to insert it into the minibuffer or use `M-i` for
  inserting one using `selectrum-quick-keys`.
* *To copy the current candidate:* type `M-w` or what is bound to
  `kill-ring-save`. When there's an active region in your input, this
  still copies the active region. The behavior of `M-w` is not
  modified when Transient Mark mode is disabled.
* *To select multiple candidates:* separate them with `crm-separator`
  (`,` by default). To make this workflow more convenient, you can use
  `TAB` to complete the currently selected candidate before typing
  `crm-separator` (for common values of `crm-separator` it will be
  automatically inserted for you). This feature only works in commands
  that use `completing-read-multiple`, such as `describe-face`. (If
  multiple selection is enabled, it is shown in the minibuffer
  prompt.)
* *To change the display style of candidates:* use `M-q` which will
  cycle from the currently used style through the styles in
  `selectrum-display-style-cycle-list`. With the default configuration
  this command will toggle between the vertical and an `icomplete`
  like horizontal display.

Selectrum respects your custom keybindings, so if you've bound
`next-line` to `M-*` for some reason, then pressing `M-*` will select
the next candidate. If you don't like the standard Selectrum bindings,
you can change them in `selectrum-minibuffer-map`.

The keybindings listed above are the *only* ones changed from standard
editing bindings. So, for example:

* All your standard horizontal motion, selection, insertion, and
  deletion commands work as usual.
* To delete your current input, just use `C-a C-k` or `C-S-backspace`
  (bound to `kill-whole-line`).
* To edit by word units use `M-DEL` like usual. To go up a directory
  you can use `C-M-DEL` (bound to `selectrum-backward-kill-sexp`). Be
  aware that on some Linux distributions, this binding is used to kill
  the X server, which can force-quit all programs you opened.
  Therefore, accidentally killing the X server can cause data
  corruption and loss of unsaved work. In such cases, you can instead
  use `ESC C-DEL`, which Emacs helpfully binds by default.
* To navigate to your home directory, you can just use `C-a C-k ~/`.
  Alternatively, like in default completion, you can type `~/` after a
  `/` to ignore the preceding input and move to the home directory.
* Minibuffer history navigation works as usual with `M-p` and `M-n`.
  `M-r` will invoke an improved version of history search with
  completion.

### Sorting and filtering

The default sorting and filtering in Selectrum is quite simple and
predictable. The method is similar to the one employed by Icomplete.
Candidates are sorted first by history position (by *recency*), then
by length and then alphabetically. Afterwards they are filtered and
highlighted using the `completion-styles`. This default behavior is
intended as a lowest common denominator that will definitely work.

It is strongly recommended that you customize `completion-styles`
using Orderless or install Prescient as described before. It is also
possible to supply your own sorting, filtering, and highlighting logic
if you would like. For that, see the developer guide later in this
documentation.

Independent of the sorting and filtering method, Selectrum adds two
special features on top:

* If your input matches one of the candidates exactly, then that
  candidate is unconditionally sorted first. (So, if you type in
  `find-file`, then `ido-find-file` will never be sorted before
  `find-file`, no matter what.) This is intended to reduce frustration
  in the case that you know what you want and you don't want Selectrum
  getting in the way.
* After that, if the caller of Selectrum specified a default candidate
  (for example, `describe-function` suggests the function near point
  as a default) then that candidate will be sorted before the rest.
  This means you can just press `RET` immediately to accept the
  default, like usual.

Case-sensitivity and other filter options should be configured via the
used refinement function. The built-in `completion-styles` support the
`completion-ignore-case`, `read-file-name-completion-ignore-case` and
`read-buffer-completion-ignore-case` options.

### Additional features

* You can repeat the last command that invoked Selectrum, restoring
  your user input and selected candidate, using `selectrum-repeat`.
  You must bind this command to a key sequence in order to use it,
  since running `selectrum-repeat` from `M-x` will dutifully repeat
  the last command that invoked Selectrum, which was `M-x`. For
  example:

  ```elisp
  (global-set-key (kbd "C-x C-z") #'selectrum-repeat)
  ```

### Customization

* By default, ten candidates are shown in the minibuffer at any given
  time. You can customize that by changing
  `selectrum-max-window-height`.
    * Normally `selectrum-max-window-height` acts as just a maximum
      for the height a window will expand to, if there isn't enough
      space for the current candidates. For vertically displayed
      candidates you might prefer to *always* have that height, even
      if there are fewer candidates. This behavior may be achieved by
      setting `selectrum-fix-vertical-window-height` to a non-nil
      value.
* The variable `selectrum-num-candidates-displayed` controls how many
  candidates are displayed in total. The default value `auto` will
  automatically use as many candidates as are possible to display by
  given space and height settings.
* The option `selectrum-display-style` controls how candidates are
  displayed, settings for vertical and horizontal display are included
  and you can cycle through display styles using the command
  `selectrum-cycle-display-style` which uses
  `selectrum-display-style-cycle-list` for cycling.
* You can use `selectrum-display-action` to display candidates in a
  window or frame outside the minibuffer. If you want to display the
  whole minibuffer (including the input line) in a separate frame you
  can use the
  [mini-frame](https://github.com/muffinmad/emacs-mini-frame) package,
  see the
  [wiki](https://github.com/raxod502/selectrum/wiki/Additional-Configuration#display-minibuffer-in-a-child-frame-with-mini-frame)
  for setup instructions. To run additional code when initializing the
  candidate buffer you can use `selectrum-display-action-hook`.
* The currently selected candidate is highlighted with the face
  `selectrum-current-candidate`. If you don't like the color, you can
  adjust it to taste.
  * By default, only the displayed text is highlighted. If you wish to
    extend the highlight until the margin, you can set
    `selectrum-extend-current-candidate-highlight` to `t`.
* By default, the total number of matches are shown before the prompt.
  This behavior can be customized using `selectrum-count-style`.
* You can show the indices of displayed candidates by customizing
  `selectrum-show-indices`. This may be helpful in telling you what
  prefix argument you should pass to
  `selectrum-select-current-candidate` and
  `selectrum-insert-current-candidate` in order to choose a candidate.
  Furthermore, if you want do display a custom index (e.g. letters
  instead of indices, roman numerals, etc.) you can set the
  `selectrum-show-indices` to a function that takes in the relative
  index of a candidate and returns the string you want to display.
* By default, Selectrum does also handle in buffer completion via
  `completion-in-region`. If you would like to disable that you can
  unset `selectrum-complete-in-buffer` before activating
  `selectrum-mode`.
* The `selectrum-completion-in-region` function can display
  annotations if the `completion-in-region-function` backend offers
  them. Customize the face `selectrum-completion-annotation` to change
  their appearance.
    * Customize the face `selectrum-completion-docsig` to change the
      appearance of function signatures show by
      `completion-in-region`.
    * Customize the face `completions-common-part` to change the
      appearance of the common prefix in `completion-in-region`
      candidates.
* You can configure the initial filtering of
  `selectrum-completion-in-region` using
  `selectrum-completion-in-region-styles`.
* The option `selectrum-should-sort` controls whether preprocessing
  functions should sort.
* You can configure the keys for quick candidate insertion and
  selection using `selectrum-quick-keys`. These are used when using
  the commands `selectrum-quick-select` or `selectrum-quick-insert`
  which provide you an `ivy-avy` like interface to quickly select a
  candidate via key annotations. You can configure the appearance of
  these key annotations with `selectrum-quick-keys-highlight` and
  `selectrum-quick-keys-match` face.
* Using the `selectrum-files-select-input-dirs` option you can adjust
  the selection behavior for file completions. When the option is
  non-nil the input gets selected whenever it contains a full
  directory name.

### Complementary extensions

For a fully fledged setup enabling additional features similar to
those you find in [Helm](https://github.com/emacs-helm/helm) or
[Ivy](https://github.com/abo-abo/swiper#ivy) we recommend the
following additional packages:

* Useful commands based on `completing-read` are provided by
  [consult](https://github.com/minad/consult). Consult is designed as
  the [counsel](https://github.com/abo-abo/swiper#counsel) equivalent
  for Selectrum and Icomplete or more generally any completion system
  based on `completing-read`.

* For filtering and frecency-based sorting there is
  [Prescient](https://github.com/raxod502/prescient.el).

* As an alternative filtering method, there is
  [orderless](https://github.com/oantolin/orderless). It supports many
  different matching styles and integrates with `completion-styles`.

* For minibuffer actions and occur/export features there is
  [embark](https://github.com/oantolin/embark/). Embark provides
  features like ivy-actions/ivy-occur in a framework agnostic way.

* Helpful minibuffer annotations for `M-x`, `describe-*` functions and
  completions in general are provided by
  [marginalia](https://github.com/minad/marginalia) which is similar
  to ivy-rich but works with any framework implementing the default
  API for completion annotations.

* You can display completions in a child frame using
  [emacs-mini-frame](https://github.com/muffinmad/emacs-mini-frame).

The above packages work well in combination and we are collaborating
with each other to ensure an optimal experience while not introducing
any hard dependencies. Our common denominator is the standard Emacs
API.

For other possibly interesting packages see our
[wiki](https://github.com/raxod502/selectrum/wiki/) which also
contains [configuration
tips](https://github.com/raxod502/selectrum/wiki/Additional-Configuration)
for many of these.

### But what is it doing to my Emacs??

By inspecting the source code of `selectrum-mode`, you will see that
Selectrum operates by setting a number of standard Emacs variables
(`completing-read-function`, `read-file-name-function`, etc.) and
installing advice on a number of standard functions
(`read-library-name`, `minibuffer-message`, etc.).

If you object to these changes being made magically, you can make them
yourself and refrain from enabling `selectrum-mode`. However,
backwards compatibility is not guaranteed for this usage, so you will
need to review the source code of `selectrum-mode` after each update
of Selectrum.

The autoloads of Selectrum are set up so that you can enable
`selectrum-mode` without actually loading Selectrum. It will only be
loaded once you use some of its functionality in an interactive
command.

### News

We document changes for users in the
[CHANGELOG](https://github.com/raxod502/selectrum/blob/master/CHANGELOG.md).
To keep up with latest changes and features you can subscribe to the
[feed](https://github.com/raxod502/selectrum/commits/master/CHANGELOG.md.atom).

## Developer guide

This section is intended for the authors of packages which integrate
with Selectrum, or for end users who wish to customize the sorting and
filtering behavior of Selectrum.

### Usage of Selectrum

**In normal usage, there should be no need to use any
Selectrum-specific functions. Simply use `completing-read` and
friends, and Selectrum will automatically enhance the experience if
`selectrum-mode` is enabled.**

Selectrum does expose some completion functions as part of its public
API.

* `selectrum-completing-read` (for `completing-read-function`)
* `selectrum-completing-read-multiple` (to override
  `completing-read-multiple`)
* `selectrum-completion-in-region` (for
  `completion-in-region-function`)
* `selectrum-read-buffer` (for `read-buffer-function`)
* `selectrum-read-file-name` (for `read-file-name-function`)
* `selectrum-read-directory-name` (to override `read-directory-name`)
* `selectrum-read-library-name` (to override `read-library-name`)

These functions are used as replacements for the standard completion
functions when `selectrum-mode` is enabled. If you want to define your
own commands using completion, it is recommended to use the standard
`completing-read` API.

### Sorting, filtering, and highlighting

Selectrum exposes a very simple API for sorting, filtering, and
highlighting. Each of these three tasks is controlled by a separate
user option:

* `selectrum-preprocess-candidates-function` takes the original list
  of candidates and sorts it (actually, it can do any sort of
  preprocessing it wants). Usually preprocessing only happens once.
  Under special circumstances where the candidate set is dynamic,
  preprocessing happens instead after each input change.
* `selectrum-refine-candidates-function` takes the preprocessed list
  and filters it using the user's input. This refinement happens every
  time the user input is updated.
* `selectrum-highlight-candidates-function` takes a list of the
  refined candidates that are going to be displayed in the minibuffer,
  and propertizes them with highlighting.

For exact specifications of these functions, including whether or not
the input list may be modified, please see their docstrings. This
information is important, because if you make copies of the candidate
list unnecessarily, there will be noticeable lag due to the slowness
of Emacs' garbage collector.

### Text properties

Selectrum allows changing the display of candidates within the
constraints of the official API by make use of text properties of
completion candidates. However it is preferable to use an annotation
function (or affixation which is introduced in Emacs 28), see `(info
"(elisp) Programmed Completion") to make the annotations work with any
compliant completion framework. We also have some information about
using annotations on the
[wiki](https://github.com/raxod502/selectrum/wiki/Tips-for-Creating-Commands#annotating-candidates).

The following text properties can be used, which may be applied to
candidates using `propertize`:

* `selectrum-candidate-display-prefix`: controls how the candidate is
  displayed in the list shown in the minibuffer. If this property is
  present, then its value is prepended to the candidate when it is
  displayed. This is used, for example, to display disambiguating
  parent directories in `read-library-name`.
* `selectrum-candidate-display-suffix`: same as the display prefix,
  but it's postpended instead of prepended when the candidate is
  dispalyed. This is used, for example, to display candidate
  annotations under `completion-in-region`.

Besides, we have:

* `selectrum-candidate-display-right-margin`: if this property is
  presented, its value is displayed at the right margin after the
  candidate. Currently Selectrum doesn't make use of this property. It
  can be used to display supplementary information.

Note that sorting, filtering, and highlighting is done on the standard
values of candidates, before any of these text properties are handled.

### Hooks

Selectrum provides two hooks for getting information about what
candidates were selected. These are intended primarily for packages
like `prescient.el` which want to record history statistics. The hooks
are:

* `selectrum-candidate-selected-hook`
* `selectrum-candidate-inserted-hook`

For more information, see their docstrings.

### Variables

You can use the variable `selectrum-is-active` to check if the current
minibuffer session is a Selectrum one.

To adjust session settings you can set the user option variables
locally in `minibuffer-with-setup-hook`. Additionally the following
variables can be used to adjust session behavior:

* `selectrum-move-default-candidate`

For more information, see the respective docstrings.

## Contributor guide

Please see [the contributor guide for my
projects](https://github.com/raxod502/contributor-guide). We have some
[test scripts](https://github.com/raxod502/selectrum/tree/master/test)
for testing minimal default configurations of common package
combinations. You can run them using

```sh
cd test; ./run.sh <package-combo>.el
```

Technical points:

* When adding a new state variable, make sure it is declared as a
  local variable (using `defvar-local`) so recursive sessions aren't
  affected. By convention we also use `setq-local` each time such a
  state variable is set.
* By default, `debug-on-error` doesn't work for errors that happen on
  `post-command-hook`. You can work around the issue like so:

  ```elisp
  (defun force-debug (func &rest args)
    (condition-case e
        (apply func args)
      ((debug error) (signal (car e) (cdr e)))))

  (advice-add #'selectrum--minibuffer-post-command-hook :around #'force-debug)
  ```

## Caveats

* There is no built-in support for alternate actions on minibuffer
  candidates but you can add those using
  [embark](https://github.com/oantolin/embark/).
* In Emacs 26 and earlier, the way that messages are displayed while
  the minibuffer is active is unworkably bad: they block out the
  entire minibuffer as long as they are displayed, and then mess up
  redisplay. This issue has been fixed in Emacs 27, and I suggest
  upgrading. I think the best solution for people running Emacs 26
  would be the development of a small third-party package which
  backports the improvement from Emacs 27. That way all
  minibuffer-based packages can benefit from the improvement.
* With certain theme settings face attributes can conflict with
  selection indication and faces used for match highlighting. This can
  become apparent when candidates use the `:background` property for
  example when matching org block lines using `consult-line`, see
  [#425](https://github.com/raxod502/selectrum/issues/425). To work
  around this specific case you can configure the consult option
  `consult-fontify-preserve`.
* There are a few standard features which aren't implemented in
  Selectrum, yet. We collect those in
  ([#481](https://github.com/raxod502/selectrum/issues/481)), most
  notable ones are:
  * We don't make use of `completion-boundaries`
    ([#448](https://github.com/raxod502/selectrum/issues/448)).
  * Dynamic table support is incomplete
    ([#114](https://github.com/raxod502/selectrum/issues/114)).

## Selectrum in comparison to other completion-systems

This section documents why I decided to write Selectrum instead of
using any of the numerous existing solutions in Emacs.

I have not used many of these packages extensively. So, if you think
I've overlooked an important part or I've written something mean or
unfair, **please** feel free to contribute a correction.

See [#23](https://github.com/raxod502/selectrum/issues/23) for
discussion.

### Ido

[Ido](https://www.gnu.org/software/emacs/manual/html_node/ido/index.html)
is a package for interactive selection that is included in Emacs by
default. It's a great improvement on the default `completing-read`
experience. However, I don't like how it displays candidates in a
horizontal instead of a vertical manner. It feels less intuitive to
me. Another key issue with Ido is that it hardly supports any commands
out of the box (only buffers and files). There is an extension package
[ido-completing-read+](https://github.com/DarwinAwardWinner/ido-completing-read-plus)
which adds support for the `completing-read` interface, but I have
been told that even this package does not handle all the cases
correctly.

There is a package
[`ido-vertical-mode`](https://github.com/creichert/ido-vertical-mode.el)
which makes Ido display candidates vertically instead of horizontally,
but I suspect that the problems with `completing-read` non-compliance
remain.

### Helm

[Helm](https://github.com/emacs-helm/helm) is an installable package
which provides an alternate vertical interface for candidate
selection. It has the advantage of having very many features and a
large number of packages which integrate with it. However, the problem
with Helm for me is exactly that it has too many features. Upon
opening a Helm menu, I am immediately confronted by numerous colors,
diagnostics, options, and pieces of help text. It is too complicated
for the problem I want solved. Of course, I am sure it is possible to
customize Helm so that it is simpler in appearance. But that would
take a long time and I would rather use a piece of software which was
designed for the use case I have in mind. I also personally prefer
using software that I have some hope of understanding, which ideally
means that they don't provide a hugely complex array of features of
which I only use one or two.

See [#203](https://github.com/raxod502/selectrum/issues/203).

### Ivy

[Ivy](https://github.com/abo-abo/swiper#ivy) is a promising
alternative to Selectrum. It is described as a minimal alternative to
Helm which provides a simpler interface. The problem with Ivy is that
its architecture and API have grown organically, and as a result the
implementation is complex. Ivy was originally designed to be used as a
backend to [Swiper](https://github.com/abo-abo/swiper#swiper), a
buffer search package that originally used Helm. When Ivy became a
more general-purpose interactive selection package, more and more
special cases were added to try to make various commands work
properly. As a result, the `ivy-read` API is complex with around 20
arguments and multiple special cases for particular values. Numerous
functions in Ivy,
[Counsel](https://github.com/abo-abo/swiper#counsel), and Swiper have
special cases hardcoded into them to detect when they're being called
from specific other functions in the other two packages.

The main differences between Selectrum and Ivy are:

* The Selectrum code base is simpler and more concise, since Selectrum
  provides a more restricted feature set.
* Selectrum focuses on the standard completion API offered by Emacs
  and tries to provide the best possible UI for this API. In contrast,
  Ivy deviates from this API and invents its own API with extra
  features, sacrificing reuse and composability down the road.
* The packages centered around the `completing-read` API are more
  composable, interchangable and modular. Since Selectrum does not offer
  a public completion API, the decoupling of the components is enforced.
* By focusing on a single API, the components can be tested against
  different implementations, e.g., Selectrum, Icomplete or default
  completion, which improves consistency and helps with correctness.

Selectrum does not support features which break the `completing-read`
API and works with *every* Emacs command with essentially no special
cases, specifically because it focuses on doing the common case as
well as possible.

### Icomplete

[Icomplete](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html)
is the built-in Emacs package for interactive selection. It is
basically the same as the standard `completing-read` framework, except
that the available candidates are displayed in the minibuffer as you
type. Unlike Selectrum, the candidates are displayed horizontally (by
default). This can be changed by some manual configuration, including
customizing `icomplete-separator`, although it is clear that this use
case is not an intended one for Icomplete. A serious usability problem
of Icomplete is that the way you select a candidate from lower down in
the list is very unintuitive: you must "rotate" the entire set of
candidates, whereupon the previous candidates become invisible since
they have wrapped to the bottom of the list.

With sufficient configuration, it is likely possible to replicate a
subset of the features of Selectrum using Icomplete. However, the
documentation of Icomplete is basically nonexistent, and to achieve
this configuration one must bend Icomplete rather severely away from
the interaction model it is designed for. In other words, the
configuration is not an enjoyable process, and the results will never
be equivalent in user experience to a package that was designed for
the desired interaction model in the first place. Selectrum, on the
other hand, offers a well-tuned and snappy vertical completion
interface that is robust and works out of the box.

There is a package which takes care of some of the manual labor of
configuring Icomplete, called
[`icomplete-vertical`](https://github.com/oantolin/icomplete-vertical).

It is worth noting the new [Fido
mode](https://github.com/emacs-mirror/emacs/commit/213643a890913f10bac710ca8537e8b1125941d6)
which will be included in Emacs 27. It is basically a variation of
Icomplete that behaves more like Ido. As such, Fido mode does not
offer solutions to the problems outlined in the above sections.

On the upside, Icomplete is the most API compliant enhanced completion
UI available. Selectrum also covers the most important aspects of the
API and strives to achieve full compliance, as well. For the few edge
cases left, see the Caveats section.

### Vertico

[Vertico](https://github.com/minad/vertico) is a new minimalistic
completion system based on the Emacs default completion offering a
similar UI as Selectrum. It uses a different implementation approach -
it extends the default completion system in a similar way as Icomplete
and is therefore fully compliant with all features of the
`completing-read` API. Overall Vertico follows a similar philosophy as
Selectrum, relying on default components and [complementary
packages](#complementary-extensions). Many of the complementary
packages, notably Consult, work well with both Selectrum and Vertico.
Selectrum offers a flexible UI, e.g., it supports both a horizontal
and a vertical display. Furthermore it provides Avy-style quick keys
and display actions, to show the completions in a buffer. On the other
hand, Vertico additionally supports cycling over candidates and
provides more commands for grouping support.

### Icicles

[Icicles](https://www.emacswiki.org/emacs/Icicles) is a package
somewhat like Helm, written by [Drew
Adams](https://www.emacswiki.org/emacs/DrewAdams). Like other packages
by Drew, Icicles is only available for manual download from EmacsWiki.
[It has been removed from
MELPA](https://github.com/melpa/melpa/pull/5008) due to community
consensus that this distribution mechanism has unacceptable security
risks, but Drew has declined to migrate to any other distribution
mechanism.

Because of this situation, I have never attempted to use Icicles, and
cannot comment on the package on the basis of its features. If you
would like to submit a pull request explaining the advantages and/or
disadvantages of Icicles versus Selectrum, we would appreciate it.

### Snails

[Snails](https://github.com/manateelazycat/snails) describes itself as
a "modern, easy-to-expand fuzzy-search framework". From the README, it
seems to provide a similar vertical completion interface to Selectrum.

One problem with Snails is that, like Ivy, it goes the route of
wrapping every possible command with a "backend" rather than using
existing Emacs interfaces to handle all possible commands.

### Sallet

[Sallet](https://github.com/Fuco1/sallet) describes itself as "a type
of light spherical helmet", according to the repo description.
However, it also appears to be another vertical completion interface.
Although I haven't used Sallet extensively, here are some differences
that I can note:

* Sallet seems to go the route of providing wrappers for all the
  possible commands, rather than implementing all of them via the
  existing `completing-read` interface. I am skeptical of this for the
  reasons outlined in the Ivy section.
* Sallet provides a "rich-text" approach to vertical completion, where
  you are shown an entire buffer with colors and multiple columns.
  Personally, I would prefer something more minimal that fits neatly
  into the minibuffer.
* There is no user-facing documentation, which suggests to me that the
  package is unfinished.

### Raven

[Raven](https://github.com/chameco/raven) is a little-known package
for vertical completion. It looks quite similar to Selectrum, and
seems pretty usable to me. The main difference is that Selectrum
simply has a more fully-rounded set of features (such as candidate
highlighting and a full `find-file` replacement). I suspect that these
features have simply not yet been implemented.

### Swiper

As discussed in the section on Ivy,
[Swiper](https://github.com/abo-abo/swiper#swiper) is a buffer-search
package that uses Ivy's interface and is coupled closely to the Ivy
implementation.

Does Selectrum attempt to provide a replacement for Swiper in addition
to Ivy and Counsel?

The answer is no - such functionality will not be part of Selectrum
itself, but there are two alternatives available.

* [CTRLF](https://github.com/raxod502/ctrlf) is a from scratch
redesigned buffer-search interface. During the design process, I
realized that a Selectrum-like interface is not the best way to
present buffer search. Instead, I decided on an improved variant of
the Isearch interface that takes inspiration from the standard text
search interface found in almost every other modern piece of software,
such as web browsers.

* [Consult](https://github.com/minad/consult): The Consult package
provides the command `consult-line` which behaves similarly to Swiper.
