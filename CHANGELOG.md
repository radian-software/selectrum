# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Breaking changes
* Support for Emacs versions older than 26.1 has been removed ([#454],
  [#465]).
* The experimental `selectrum-helm.el` package has been removed
  ([#313], [#466]).

### Features
* You can quickly select and insert a candidate using the commands
  `selectrum-quick-select` and `selectrum-quick-insert` which are
  bound to `M-i` and `M-m` (analogue to `C-i` and `C-m` used for
  normal insertion and selection). These commands are similar in
  functionality to `ivy-avy`. You can configure the used keys via
  `selectrum-quick-keys` option and their faces via
  `selectrum-quick-keys-highlight` and `selectrum-quick-keys-match`
  face. ([#16], [#304], [#479]).
* Add support for `x-group-function` completion metadata. The group
  title formatting is controlled by the customization variable
  `selectrum-group-format` and the faces `selectrum-group-separator`
  and `selectrum-group-title` ([#458], [#463]).

### Deprecated
The faces `selectrum-primary-highlight` and
`selectrum-secondary-highlight` have been deprecated. The match
highlighting should be configured via the used filtering/highlighting
packages. Users of `selectrum-prescient` can update to configure
`selectrum-prescient-primary-highlight` and
`selectrum-prescient-secondary-highlight` ([#455]).

### Enhancements
* File name completions using `selectrum-completion-in-region` have
  been improved. When starting with an empty input the completion
  starts at `default-directory` and file names are quoted in modes
  deriving from `comint-mode` ([#459], [#462]).
* `selectrum-completion-in-region` does support cycling (as configured
  per `completion-cycle-threshold`) now ([#419], [#456]).


### Bugs fixed
* When using `marginalia-mode` there would be an arrow in the fringe
  for the currently selected candidate which has been fixed ([#450],
  [#488]).
* When doing `completing-read-multiple`, selecting an additional
  candidate and exiting by pressing `RET` no longer fails when there
  are existing candidates already selected using `TAB` ([#460]).

[#16]: https://github.com/raxod502/selectrum/issues/16
[#304]: https://github.com/raxod502/selectrum/issues/304
[#313]: https://github.com/raxod502/selectrum/issues/313
[#419]: https://github.com/raxod502/selectrum/issues/419
[#450]: https://github.com/raxod502/selectrum/issues/450
[#454]: https://github.com/raxod502/selectrum/issues/454
[#455]: https://github.com/raxod502/selectrum/pull/455
[#456]: https://github.com/raxod502/selectrum/pull/456
[#458]: https://github.com/raxod502/selectrum/pull/458
[#459]: https://github.com/raxod502/selectrum/issues/459
[#460]: https://github.com/raxod502/selectrum/pull/460
[#462]: https://github.com/raxod502/selectrum/pull/462
[#463]: https://github.com/raxod502/selectrum/pull/463
[#465]: https://github.com/raxod502/selectrum/pull/465
[#466]: https://github.com/raxod502/selectrum/pull/466
[#479]: https://github.com/raxod502/selectrum/pull/479
[#488]: https://github.com/raxod502/selectrum/pull/488

## 3.1 (released 2021-02-21)
### Deprecated
* The `selectrum-read` API has been deprecated and made private. The
  intention of this change is to encourage users instead to rely on
  the plain `completing-read` function, which is completion system
  agnostic ([#446]).
* The user option `selectrum-fix-minibuffer-height` has been
  deprecated. The new variable `selectrum-fix-vertical-window-height`
  takes its place ([#305]).
* The special text property `selectrum-candidate-full` to change the
  canonical representation of a candidate has been deprecated
  ([#403]).
* The hooks `selectrum-candidate-inserted-hook` and
  `selectrum-candidate-selected-hook` originally also received the
  arguments passed to `selectrum-read` which has been deprecated.
  These hooks are expected to be updated to only receive a single
  argument now, the inserted/selected candidate ([#446]).
* `selectrum-default-candidate-refine-function` and
  `selectrum-default-candidate-highlight-function` have been
  deprecated, see the new default values for
  `selectrum-refine-candidates-function` and
  `selectrum-highlight-candidates-function` which should be used
  instead ([#330]).

### Changed defaults
* The default value of `selectrum-num-candidates-displayed` has
  changed to `auto`. If you have customized
  `selectrum-num-candidates-displayed` you should remove that from
  your configuration or also adjust it to `auto`. For configuring the
  window height you should use the new `selectrum-max-window-height`
  option now ([#305]).
* The new default functions used for
  `selectrum-refine-candidates-function` and
  `selectrum-highlight-candidates-function` have been updated to
  filter and highlight candidates according to `completion-styles` now
  ([#330]).

### Features
* Line spacing is taken into account when using a fixed window height
  ([#424], [#432]).
* The new option `selectrum-max-window-height` can now be used to
  configure the maximal display window height analogue to the built-in
  `max-mini-window-height`. The new option replaces the usage of the
  formerly used `selectrum-num-candidates-displayed` setting which is
  now set to `auto` by default. By using `auto` the number of
  candidates is automatically determined using the available space and
  current display settings. When setting the value to a number this
  will determine the actual amount of displayed candidates without
  having an effect on the window height ([#305]).
* The option `selectrum-display-style` can be used to configure the
  display style for candidates. Vertical and horizontal display styles
  are included and you can cycle through styles using the new
  `selectrum-cycle-display-style` command which uses the
  `selectrum-display-style-cycle-list` option for cycling ([#305]).
* `selectrum-exhibit` got an optional argument which allows to keep
  the current candidate selected after the update which is helpful for
  async completions ([#306], [#307], [#349]).
* The user option `selectrum-display-action` can be used to show
  candidates in another window or frame ([#230], [#309]).
* The user option `selectrum-show-indices` can now be a function that
  can be used to control the display of the a candidate's index ([#200]).
* The user option `selectrum-extend-current-candidate-highlight`
  determines whether to extend the highlighting of the current
  candidate until the margin (the default is nil). See [#208].
* The user option `selectrum-complete-in-buffer` can be used to
  control whether Selectrum should handle in buffer completion (the
  default is t) ([#261]).
* The user option `selectrum-default-value-format` can be used
  to specify the formatting of the default value indicator ([#445]).

### Enhancements
* When hovering over candidates with the mouse there are no messages
  shown anymore which avoids interference with candidates display
  ([#452]).
* The variable `selectrum-should-sort-p` and `selectrum-active-p` have
  been marked deprecated. You should use the new
  `selectrum-should-sort` and `selectrum-is-active`.
  `selectrum-should-sort` is also exposed as a user option now
  ([#441]).
* The variable `selectrum-move-default-candidate` can be used to
  configure how the UI handles the default value ([#441]).
* The command `selectrum-repeat` can also repeat the last recursive
  session now ([#322], [#440]).
* The default value of `selectrum-extend-current-candidate-highlight`
  has been changed to `auto` which will automatically extend the
  highlighting if the session uses annotations ([#427], [#430]).
* The accessibility of the default value in file completions has been
  improved. When the default exists in the prompting directory it gets
  sorted first otherwise the default is included as a virtual
  candidate ([#400], [#420], [#421]).
* Depending on the used font and display settings like `line-spacing`
  the minibuffer height could be slightly off so that the displayed
  candidates wouldn't be completely visible, which has been fixed
  ([#303], [#414]).
* The sorting of passed defaults for file completions has been
  improved such that paths like `/home/user/default`, `~/default` or a
  relative passed default get sorted first when they exist within the
  prompting directory ([#402], [#404]).
* Tramp completions have been improved. You now get completion for
  tramp methods and hosts. If a connection hasn't been established yet
  and you are manually typing the path, a message is shown that you
  can refresh using `selectrum-insert-current-candidate` which helps
  correcting typos before trying to establish a new connection. Lastly
  if tramp would error or you would quit from a password prompt
  Selectrum stopped working until you restarted the session, which has
  been fixed ([#392], [#394], [#405], [#408]).
* Selectrum will allow recursive sessions for
  `selectrum-completion-in-region` and `selectrum-select-from-history`
  so these commands work even if `enable-recursive-minibuffers` is not
  set by the user ([#100], [#397]).
* In file completions where the directory path of the input does not
  exist, the candidates are automatically gathered by interpreting the
  input as an partial-completion style input pattern (see
  `completion-styles-alist`). For example the input "/us/l/bi/" would
  give results for "/usr/local/bin/". With tramp paths this has to be
  triggered manually using `selectrum-insert-current-candidate` to
  avoid possible speed problems ([#390], [#393]).
* You can now complete environment variables in file completions by
  typing a "$" after a "/" ([#386], [#389]).
* The `selectrum-select-from-history` command has been improved. You
  can now insert a history item into the previous session using your
  default binding for `selectrum-insert-current-candidate`. To submit
  the history item and exit use `selectrum-select-current-candidate`
  ([#362]).
* When using commands where the prompt would exceed the window width
  the horizontal scroll wouldn't reset afterwards when a smaller
  element was pulled into the prompt under certain conditions (for
  example when using history commands), which has been fixed ([#360]).
* When the prompt is empty and the default value is shown you can now
  insert it using `selectrum-insert-current-candidate`. Also
  `selectrum-insert-current-candidate` now moves point to end of the
  prompt even when there were no candidate insertion to have
  consistent UI behaviour ([#359], [#369]).
* `selectrum-insert-current-candidate` will reset
  `minibuffer-history-position`, so that after "choosing" an item and
  using other history commands in succession the history will start
  from the beginning ([#361], [#368]).
* History commands don't automatically trigger a refresh for tramp
  paths. This is useful to prevent unintended opening of tramp
  connections. To trigger a refresh for the selected tramp path you
  can use `selectrum-insert-current-candidate` ([#358], [#361],
  [#365], [#367], [#368], [#372]).
* In file completions the prompt will also be selected when a match is
  required and the path exists ([#357]).
* Improved selection behaviour for history commands. When using
  `next-history-element`, `previous-history-element` or isearch for
  history browsing the inserted history element will get selected when
  a match isn't required ([#323], [#324], [#341], [#346], [#380]).
* Improved exit behaviour of `selectrum-select-current-candidate`. The
  commands gives feedback now when match is required and submission
  not possible. Also it allows submission of the prompt when a match
  is required and the prompt is a member of candidates ([#338]).
* You can now configure the initial filtering for candidates in
  `selectrum-completion-in-region` using
  `selectrum-completion-in-region-styles` ([#331], [#356]).
* Computation of candidates is faster for `describe-variable` ([#312],
  [#316], [#320], [#321], [#343]).
* Candidates of `completing-read-multiple` which are submitted by
  `selectrum-select-current-candidate` are now passed to
  `selectrum-candidate-selected-hook` one by one in the order they
  were added. Before the hook would not run for the multi candidates
  case ([#296]).
* File completions are faster because the internal handling was
  updated ([#334], [#335], [#339]). Most notably recomputation only
  happens on directory change now. Before, the candidates where
  recomputed on each input change which could slow down file
  completions significantly for cases where `read-file-name-internal`
  would be slow already ([#210], [#276], [#277]).
* You can now give a prefix argument to selection moving commands
  ([#275]).
* If completion table metadata or `completion-extra-properties` define
  an `affixation-function` (introduced in Emacs 28) Selectrum will use
  this information to annotate the candidates accordingly ([#240],
  [#271], [#286], [#288], [#289]).
* The argument passed to `selectrum-select-current-candidate` and
  `selectrum-insert-current-candidate` is now used to choose the nth
  displayed candidate instead of calculating an index based on the
  currently selected candidate ([#194], [#215]).
* `selectrum-insert-current-candidate` no longer adds inserted
  candidates to the history ([#212], [#213]).
* Selectrum now by default shows indices relative to displayed
  candidates ([#200]).
* Selectrum now uses the `initial-input` argument passed to
  `completing-read` which was ignored before ([#254]).
* The prompt gets initially selected now when it equals the default
  value. This aligns with Selectrum's behavior of sorting the default
  first and will also make such prompts behave like in default Emacs
  completion where you can immediately submit the initial input
  ([#253]).
* In buffer file completions act like normal completion now and insert
  the candidate without prompting if there is only one. You can drop
  into the minibuffer by triggering the completion again ([#261]).
* The mark is pushed at the beginning of the candidate inserted by in
  buffer completion so you can easily jump there ([#261]).
* When the prompt is selected``selectrum-insert-current-candidate`
  will now switch the selection to the first candidate. Before the
  prompt was reinserted in place so it did not have any useful effect
  ([#263]).
* Default settings of `selectrum-multiline-display-settings` have been
  improved. There is now also a displayed line count by default which
  can be configured as well ([#266], [#302], [#318], [#398]).

### Bugs fixed
* Candidate parts which used the `display` property wouldn't be
  correctly highlighted on selection, which has been fixed ([#411],
  [#413]).
* The minibuffer prompt face would bleed into the candidates when the
  candidate strings used the `display` property, which has been fixed
  ([#235], [#413]).
* Selectrum would error when providing a list as default value in file
  completions, which has been fixed ([#401], [#402]).
* Selectrum did not set `minibuffer-default` for the current
  completion session, which has been fixed ([#350], [#352], [#354]).
* When there were no candidates `selectrum-get-current-candidate`
  would throw an error, which has been fixed ([#347], [#348]).
* There were UI and display problems when the prompt width exceeded
  the available window width. When `auto-hscroll-mode` was set to
  `current-line` it would introduce constant back and forth scrolling
  issues and other values also wouldn't allow to use such a prompt
  correctly ([#344], [#345], [#374], [#375], [#377], [#378], [#379],
  [#381]).
* `selectrum-select-from-history` set variables
  `selectrum-should-sort-p`, `selectrum-candidate-inserted-hook`,
  `selectrum-candidate-selected-hook` and
  `enable-recursive-minibuffers` for subsequent recursive sessions,
  which has been fixed. It is also enhanced to trigger an error when
  called outside the minibuffer now [#337].
* `selectrum-completion-in-region` could trigger an error when
  `completion-all-completions` would be called within a session, which
  has been fixed ([#315], [#329]).
* `selectrum-select-from-history` wasn't autoloaded which would
  trigger an error when used before Selectrum was loaded, this has
  been fixed ([#310], [#328]).
* When let binding `minibuffer-message-timeout` around
  `minibuffer-message` within Selectrum sessions the value wouldn't be
  applied, which has been fixed ([#327]).
* `minibuffer-default` is now treated as the default when set, before
  it would have no effect. When a list the car is used as default as
  of now ([#324]).
* `selectrum-extend-current-candidate-highlight`,
  `selectrum-show-indices`, `selectrum-right-margin-padding` and
  `selectrum-multiline-display-settings` wouldn't use the local
  session bindings if there were any, which has been fixed ([#317]).
* `selectrum-insert-current-candidate` would duplicate the prompt for
  `completing-read-multiple` when the prompt was selected, which has
  been fixed. The behavior is now like in `completing-read` ([#296]).
* `selectrum-select-current-candidate` did not work correctly for
  `completing-read-multiple` when the prompt was submitted, which has
  been fixed ([#285], [#296]).
* `selectrum-candidate-inserted-hook` would run after using
  `selectrum-insert-current-candidate` with a selected prompt, which
  has been fixed ([#296]).
* Passing a symbol or a list of symbols to `completing-read` as
  default value DEF would trigger an error, which has been fixed.
  Selectrum now behaves like `completind-read-default` and returns the
  symbol (or the first in case of a list) ([#291], [#295]).
* `selectrum-active-p` would wrongly report an active status for
  recursive minibuffer session with Selectrum turned off, which has
  been fixed ([#293]).
* For in buffer file completions s-expression commands for path level
  navigation did not work which has been fixed ([#261]).
* Do not insert spaces after path completion in comint buffers
  ([#261])].
* The return value of `selectrum-completion-in-region` has been fixed
  according to the documented API of `completion-in-region` ([#251]).
* When strings of Selectrum display properties or completion table
  annotations have a face defined it gets used. Before those faces
  would be ignored ([#236], [#250]).
* Selectrum's internal minibuffer setup hook now runs after any other
  functions added to `minibuffer-setup-hook`. Before, you couldn't set
  `selectrum-should-sort-p` locally via `minibuffer-with-setup-hook`
  to adjust sorting for a single `selectrum-read` session ([#242]).
* `selectrum-completion-in-region` no longer unsets
  `selectrum-should-sort-p` for all recursive minibuffer sessions in
  the case the initial completion table specified its own
  `display-sort-function` ([#221]).
* The candiate list returned from a dynamic candidate function passed
  to `selectrum-read` is now also prevented to be modified in case
  it's a list of strings. Before the list only wasn't modfied when the
  function returned the alist format as specified by `selectrum-read`
  ([#220]).
* Annotations or usage or `selectrum-candidate-display-suffix`
  property in file completions were overwritten for directories and
  not displayed, which has been fixed ([#256], [#255]).
* `selectrum-repeat` did not set `this-command` when calling the
  last command, which has been fixed ([#438], [#439]).

[#100]: https://github.com/raxod502/selectrum/issues/100
[#194]: https://github.com/raxod502/selectrum/issues/194
[#200]: https://github.com/raxod502/selectrum/pull/200
[#208]: https://github.com/raxod502/selectrum/pull/208
[#210]: https://github.com/raxod502/selectrum/issues/210
[#212]: https://github.com/raxod502/selectrum/issues/212
[#213]: https://github.com/raxod502/selectrum/pull/213
[#215]: https://github.com/raxod502/selectrum/pull/215
[#220]: https://github.com/raxod502/selectrum/pull/220
[#221]: https://github.com/raxod502/selectrum/pull/221
[#242]: https://github.com/raxod502/selectrum/pull/242
[#230]: https://github.com/raxod502/selectrum/pull/230
[#235]: https://github.com/raxod502/selectrum/issues/235
[#236]: https://github.com/raxod502/selectrum/issues/236
[#240]: https://github.com/raxod502/selectrum/issues/240
[#250]: https://github.com/raxod502/selectrum/pull/250
[#251]: https://github.com/raxod502/selectrum/pull/251
[#253]: https://github.com/raxod502/selectrum/pull/253
[#254]: https://github.com/raxod502/selectrum/pull/254
[#255]: https://github.com/raxod502/selectrum/issues/255
[#256]: https://github.com/raxod502/selectrum/pull/256
[#263]: https://github.com/raxod502/selectrum/pull/263
[#266]: https://github.com/raxod502/selectrum/pull/266
[#271]: https://github.com/raxod502/selectrum/pull/271
[#275]: https://github.com/raxod502/selectrum/pull/275
[#276]: https://github.com/raxod502/selectrum/issues/276
[#277]: https://github.com/raxod502/selectrum/pull/277
[#285]: https://github.com/raxod502/selectrum/issues/285
[#286]: https://github.com/raxod502/selectrum/issues/286
[#288]: https://github.com/raxod502/selectrum/pull/288
[#289]: https://github.com/raxod502/selectrum/pull/289
[#293]: https://github.com/raxod502/selectrum/pull/293
[#291]: https://github.com/raxod502/selectrum/issues/291
[#295]: https://github.com/raxod502/selectrum/pull/295
[#296]: https://github.com/raxod502/selectrum/pull/296
[#302]: https://github.com/raxod502/selectrum/pull/302
[#303]: https://github.com/raxod502/selectrum/issues/303
[#305]: https://github.com/raxod502/selectrum/pull/305
[#306]: https://github.com/raxod502/selectrum/issues/306
[#307]: https://github.com/raxod502/selectrum/pull/307
[#309]: https://github.com/raxod502/selectrum/pull/309
[#310]: https://github.com/raxod502/selectrum/issues/310
[#312]: https://github.com/raxod502/selectrum/issues/312
[#315]: https://github.com/raxod502/selectrum/issues/315
[#316]: https://github.com/raxod502/selectrum/pull/316
[#317]: https://github.com/raxod502/selectrum/pull/317
[#318]: https://github.com/raxod502/selectrum/pull/318
[#320]: https://github.com/raxod502/selectrum/issues/320
[#321]: https://github.com/raxod502/selectrum/pull/321
[#322]: https://github.com/raxod502/selectrum/issues/322
[#323]: https://github.com/raxod502/selectrum/issues/323
[#324]: https://github.com/raxod502/selectrum/pull/324
[#327]: https://github.com/raxod502/selectrum/pull/327
[#328]: https://github.com/raxod502/selectrum/pull/328
[#329]: https://github.com/raxod502/selectrum/pull/329
[#330]: https://github.com/raxod502/selectrum/pull/330
[#331]: https://github.com/raxod502/selectrum/pull/331
[#334]: https://github.com/raxod502/selectrum/issues/334
[#335]: https://github.com/raxod502/selectrum/pull/335
[#337]: https://github.com/raxod502/selectrum/pull/337
[#338]: https://github.com/raxod502/selectrum/pull/338
[#339]: https://github.com/raxod502/selectrum/pull/339
[#341]: https://github.com/raxod502/selectrum/pull/341
[#343]: https://github.com/raxod502/selectrum/pull/343
[#344]: https://github.com/raxod502/selectrum/issues/344
[#345]: https://github.com/raxod502/selectrum/pull/345
[#346]: https://github.com/raxod502/selectrum/pull/346
[#347]: https://github.com/raxod502/selectrum/pull/347
[#348]: https://github.com/raxod502/selectrum/pull/348
[#349]: https://github.com/raxod502/selectrum/pull/349
[#350]: https://github.com/raxod502/selectrum/issues/350
[#352]: https://github.com/raxod502/selectrum/pull/352
[#354]: https://github.com/raxod502/selectrum/pull/354
[#356]: https://github.com/raxod502/selectrum/pull/356
[#357]: https://github.com/raxod502/selectrum/pull/357
[#358]: https://github.com/raxod502/selectrum/pull/358
[#359]: https://github.com/raxod502/selectrum/pull/359
[#360]: https://github.com/raxod502/selectrum/pull/360
[#361]: https://github.com/raxod502/selectrum/pull/361
[#362]: https://github.com/raxod502/selectrum/pull/362
[#365]: https://github.com/raxod502/selectrum/pull/365
[#367]: https://github.com/raxod502/selectrum/pull/367
[#368]: https://github.com/raxod502/selectrum/pull/368
[#369]: https://github.com/raxod502/selectrum/pull/369
[#372]: https://github.com/raxod502/selectrum/pull/372
[#374]: https://github.com/raxod502/selectrum/pull/374
[#375]: https://github.com/raxod502/selectrum/pull/375
[#377]: https://github.com/raxod502/selectrum/pull/377
[#378]: https://github.com/raxod502/selectrum/pull/378
[#379]: https://github.com/raxod502/selectrum/pull/379
[#380]: https://github.com/raxod502/selectrum/pull/380
[#381]: https://github.com/raxod502/selectrum/pull/381
[#386]: https://github.com/raxod502/selectrum/pull/386
[#389]: https://github.com/raxod502/selectrum/pull/389
[#390]: https://github.com/raxod502/selectrum/pull/390
[#392]: https://github.com/raxod502/selectrum/issues/392
[#393]: https://github.com/raxod502/selectrum/pull/393
[#394]: https://github.com/raxod502/selectrum/pull/394
[#397]: https://github.com/raxod502/selectrum/pull/397
[#398]: https://github.com/raxod502/selectrum/pull/398
[#400]: https://github.com/raxod502/selectrum/issues/400
[#401]: https://github.com/raxod502/selectrum/pull/401
[#402]: https://github.com/raxod502/selectrum/pull/402
[#403]: https://github.com/raxod502/selectrum/pull/403
[#404]: https://github.com/raxod502/selectrum/pull/404
[#405]: https://github.com/raxod502/selectrum/pull/405
[#408]: https://github.com/raxod502/selectrum/pull/408
[#411]: https://github.com/raxod502/selectrum/issues/411
[#413]: https://github.com/raxod502/selectrum/pull/413
[#414]: https://github.com/raxod502/selectrum/pull/414
[#420]: https://github.com/raxod502/selectrum/issues/420
[#424]: https://github.com/raxod502/selectrum/issues/424
[#421]: https://github.com/raxod502/selectrum/pull/421
[#427]: https://github.com/raxod502/selectrum/issues/427
[#430]: https://github.com/raxod502/selectrum/pull/430
[#432]: https://github.com/raxod502/selectrum/pull/432
[#438]: https://github.com/raxod502/selectrum/issues/438
[#439]: https://github.com/raxod502/selectrum/pull/439
[#440]: https://github.com/raxod502/selectrum/pull/440
[#444]: https://github.com/raxod502/selectrum/pull/444
[#445]: https://github.com/raxod502/selectrum/pull/445
[#446]: https://github.com/raxod502/selectrum/pull/446
[#452]: https://github.com/raxod502/selectrum/pull/452

## 3.0 (released 2020-10-20)
### Breaking changes
* The function `selectrum-read-directory-name` is no longer available.
* When reading directories and the default is already in the prompt,
  it gets selected so you can immediately submit it ([#126], [#127]).
  In correspondence with this change, the initial working directory
  for `read-directory-name` is now unchanged from the Emacs default,
  rather than being the parent directory.
* Selectrum now uses a keymap (`selectrum-minibuffer-map`) instead of
  an alist (`selectrum-minibuffer-bindings`, now removed) for
  configuring bindings ([#186]). This better meets users'
  expectations, and allows other packages (e.g., General) to better
  work with Selectrum's keybindings ([#71]).

### Features
* The user option `selectrum-completing-read-multiple-show-help` can
  be used to control display of additional usage information in the
  prompt in a `completing-read-multiple` session ([#130], [#132]).
* `selectrum-read` accepts two additional keyword arguments
  `minibuffer-completion-table` and
  `minibuffer-completion-predicate`. These can be used to pass the
  `completing-read` collection and predicate so they are available for
  internal handling of completion API features and for other external
  commands or packages which make use of them ([#94], [#95]).
* If the completion table passed to `completing-read` provides
  `annotation-function` or `display-sort-function` in its metadata,
  Selectrum will use this information to annotate or sort the
  candidates accordingly. Annotations defined by
  `completion-extra-properties` are handled, too ([#82], [#95]).
* One can trigger an update of Selectrum's completions UI manually by
  calling `selectrum-exhibit` ([#95]).
* You can now interact with candidates via the mouse. Left click
  (`mouse-1`) selects the candidate, and right click (`mouse-3`)
  inserts the candidate, just like `RET` and `TAB`, respectively. See
  [#113] and [#118].

### Enhancements
* If `selectrum-candidate-display-right-margin` is used the margin is
  included in the highlighting of the selected candidate so it's
  easier to see to which candidate the margin belongs to ([#166]).
* If the `default-filename` passed to `selectrum-read-file-name` is an
  absolute path it will still be sorted to the top when it is
  contained in the prompting directory ([#160]).
* `icomplete-mode` is now automatically disabled when entering
  Selectrum, to avoid conflicts ([#99]).
* Working with the default candidate has been improved in cases where
  it is not in the candidate list. Such candidates are currently shown
  in the prompt message. For example, the command `lgrep` might
  suggest searching through files matching `*.el` instead of just a
  specific file. See [#120], [#122].
  * While there is no user input, the default candidate remains
    visible in the prompt message. Previously, it would be hidden when
    the prompt line was not selected. Unchanged is the behavior is to
    hide the default candidate when text is typed, so that it is only
    visible when it can be submitted (similar to the effect of
    `minibuffer-electric-default-mode`).
  * The default candidate shown in the prompt message is now displayed
    with the face `selectrum-current-candidate` when it is selected.
  * Now that Selectrum always shows the default candidate when it can
    be submitted, it now attempts to remove the default candidate from
    prompt messages that already contain it. This decreases
    redundancy.
* When there is no default value the prompt shows `[default-value :
  ""]` to indicate that you would submit the empty string. Previously
  it showed `[default-value: none]` ([#133]).
* When reading file names spaces are now considered symbol
  constituents which means you can use s-expression commands to
  navigate and edit the input more efficently. A binding for
  `backward-kill-sexp` was added to go up a directory with `C-M-DEL`
  ([#138]).
* Compliance to default minibuffer API has been further improved by
  using an overlay for candidates display. Previously code which
  assumed that the minibuffer only contains user input would be likely
  to fail ([#124]). This also means inside the minibuffer
  `minibuffer-contents` now returns only the current input as expected
  ([#116], [#133]).
* Multiline candidates are now merged into a single truncated line so
  there is no gradual scrolling effect anymore when going through the
  candidate list. The first matched line is shown in front of the
  merged lines ([#133]). This formatting is customizable via
  `selectrum-multiline-display-settings` ([#147]).
* `selectrum-insert-current-candidate` now automatically inserts the
  separator for common values of `crm-separator` when using
  `completing-read-multiple` ([#197]).

### Bugs fixed
* The minibuffer height is now determined by the actual height of
  displayed candidates. Previously the height could be off for
  candidates containing unicode characters or other means which
  changed the display height such as `line-spacing` ([#146], [#151],
  [#154]).
* When passing a named function or compiled lambda as CANDIDATES
  argument to `selectrum-read` an error would be thrown, which has
  been fixed ([#163]).
* When selecting file name prompts and submitting them the result will
  be the selected prompt. Previously it could be the default file name
  passed to `selectrum-read-file-name` which is the behavior of
  default completion where there is no concept of an active selection
  ([#157], [#160]).
* When there are no candidates and a match isn't required the prompt
  will now be shown as selected to indicate one can submit the input
  ([#160]).
* If the MUSTMATCH argument to `read-directory-name` was non-nil the
  selection of the prompt wasn't visible which has been fixed ([#157],
  [#160]).
* If a predicate was passed to `read-buffer` an error would be thrown
  which has been fixed ([#159], [#161]).
* Dynamic collection functions can now reuse their returned
  candidates. Previously Selectrum could modify them even when the
  `:may-modify-candidates` argument wasn't passed to `selectrum-read`.
* Incremental history search via `isearch` wasn't working which has
  been fixed ([#124], [#133]).
* Empty string completion candidates are now ignored like in the
  default completion UI ([#101]).
* Text properties are now stripped for standard completion functions
  ([#107], [#108]).
* You can now select and submit empty input and for file prompts
  existing paths when require-match is non-nil ([#67], [#125]).
* The default candidate is now first selected, even when it is not in
  the candidate list, conforming with expectations. Previously, the
  first candidate in the list was selected instead. See [#120].
* `selectrum-insert-current-candidate` now works correctly for
  `completing-read-multiple` when `crm-separator` has a non default
  value. Previously it would replace the separator with commas when
  adding new candidates ([#140]).
* `selectrum-insert-current-candidate` now works from
  `selectrum-select-from-history` and other commands which ignore
  history by setting `minibuffer-history-variable` to `t`. Previously
  an error would be thrown ([#152]).
* When completing filenames and a match is required, non-normalized
  paths (e.g., `~/Documents//etc/hosts`) are accepted ([#190]).
* Pressing TAB when nothing matches shows a “No match” message in the
  minibuffer instead of signaling an error and erasing the minibuffer
  contents ([#193]). If `completion-fail-discreetly` is non-nil,
  nothing is done.
* Fix type mismatch when configuring `selectrum-count-style` in
  customizations.
* Submitting the default value with the empty prompt does no longer
  strip the text properties of the default candidate ([#180], [#198]).

[#67]: https://github.com/raxod502/selectrum/issues/67
[#71]: https://github.com/raxod502/selectrum/issues/71
[#82]: https://github.com/raxod502/selectrum/issues/82
[#94]: https://github.com/raxod502/selectrum/issues/94
[#95]: https://github.com/raxod502/selectrum/pull/95
[#99]: https://github.com/raxod502/selectrum/issues/99
[#101]: https://github.com/raxod502/selectrum/pull/101
[#107]: https://github.com/raxod502/selectrum/issues/107
[#108]: https://github.com/raxod502/selectrum/pull/108
[#113]: https://github.com/raxod502/selectrum/issues/113
[#116]: https://github.com/raxod502/selectrum/issues/116
[#118]: https://github.com/raxod502/selectrum/pull/118
[#120]: https://github.com/raxod502/selectrum/issues/120
[#122]: https://github.com/raxod502/selectrum/pull/122
[#124]: https://github.com/raxod502/selectrum/issues/124
[#125]: https://github.com/raxod502/selectrum/pull/125
[#126]: https://github.com/raxod502/selectrum/issues/126
[#127]: https://github.com/raxod502/selectrum/pull/127
[#130]: https://github.com/raxod502/selectrum/issues/130
[#132]: https://github.com/raxod502/selectrum/pull/132
[#133]: https://github.com/raxod502/selectrum/pull/133
[#138]: https://github.com/raxod502/selectrum/pull/138
[#140]: https://github.com/raxod502/selectrum/pull/140
[#146]: https://github.com/raxod502/selectrum/issues/146
[#147]: https://github.com/raxod502/selectrum/pull/147
[#151]: https://github.com/raxod502/selectrum/issues/151
[#152]: https://github.com/raxod502/selectrum/pull/152
[#154]: https://github.com/raxod502/selectrum/pull/154
[#157]: https://github.com/raxod502/selectrum/issues/157
[#159]: https://github.com/raxod502/selectrum/issues/159
[#160]: https://github.com/raxod502/selectrum/pull/160
[#161]: https://github.com/raxod502/selectrum/pull/161
[#163]: https://github.com/raxod502/selectrum/pull/163
[#166]: https://github.com/raxod502/selectrum/pull/166
[#180]: https://github.com/raxod502/selectrum/issues/180
[#186]: https://github.com/raxod502/selectrum/pull/186
[#190]: https://github.com/raxod502/selectrum/pull/190
[#193]: https://github.com/raxod502/selectrum/pull/193
[#197]: https://github.com/raxod502/selectrum/pull/197
[#198]: https://github.com/raxod502/selectrum/pull/198

## 2.0 (released 2020-07-18)
### Breaking changes
* The way to dynamically generate the candidate list has changed.
  Instead of rebinding `selectrum-preprocess-candidates-function` and
  `selectrum-refine-candidates-function`, you simply pass a function
  as the COLLECTION argument to `selectrum-read`. This function takes
  one argument, the current user input, and returns the list of
  candidates as strings. Alternatively, it can return an alist whose
  `candidates` key is the candidate list and whose `input` key is a
  transformed user input to use for highlighting.

  As part of this change, `selectrum-refine-candidates-function` no
  longer can return an alist; that functionality should instead be
  moved to the CANDIDATES function. (This feature was never properly
  supported in the first place if you tried to use it in a way that
  can't be done equivalently in the CANDIDATES function.)

  See [#27].

### Features
* You can now give a prefix argument to
  `selectrum-insert-current-candidate` to insert the candidate at a
  given index directly ([#96]).
* Candidates inserted by `selectrum-insert-current-candidate` are now
  added to history ([#54]).
* You can resume the last completion session using the
  `selectrum-repeat` command. (Note that you must bind this command to
  a key sequence in order to use it.) This command implements similar
  functionality to `ivy-resume`. See [#39].
* Experimental support for using Selectrum as a backend for Helm
  commands. Use it by enabling `selectrum-helm-mode` from the
  `selectrum-helm` library. See [#18].
* You can now give a prefix argument to
  `selectrum-select-current-candidate` to select the candidate at a
  given index directly. New user option `selectrum-show-indices` to
  display these indices for your convenience. This feature implements
  similar functionality to `ivy-avy`. See [#16].
* Recursive minibuffers are now supported.
* In the standard `completing-read` interface, you can use
  `previous-matching-history-element` to retrieve history
  elements. The binding now works properly in Selectrum too, except
  that you can use Selectrum to select a history element. See [#49],
  [#77]. If you prefer to use the original interface you can use
  `selectrum-previous-history-element` which is just not bound by
  default [#57].
* You can now cause the minibuffer to always have the same height,
  even if there are fewer candidates, by enabling
  `selectrum-fix-minibuffer-height` ([#35]).
* Multiple candidate selection is now supported, and we provide a
  `selectrum-completing-read-multiple` function which is installed
  automatically by `selectrum-mode`. This means that commands like
  `describe-face` (which delegate to `completing-read-multiple`
  internally) now use Selectrum by default. To select additional
  candidates within a supported command, use `TAB` and input
  `crm-separator` (`,` by default). See [#53], [#80], [#74].
* We provide a `selectrum-completion-in-region` function now and
  install it on `completion-in-region-function` in `selectrum-mode`,
  so `completion-at-point` will use Selectrum when there is more than
  one completion ([#42]). This function can display annotation
  informations if the `completion-at-point-function` backend offers
  them ([#62]), and will respect completion boundaries ([#89]).
  Appearance can be configured using the faces
  `selectrum-completion-annotation`, `selectrum-completion-docsig`,
  and `completions-common-part` ([#86]).

### Enhancements
* `selectrum-read-file-name` which is used as
  `read-file-name-function` now uses `read-file-name-default`
  internally. This means all default features of file completion
  should be available now. Most notably you can now use `M-n` to
  insert file names into the minibuffer (using
  `file-name-at-point-functions`) and you are able to use shortcuts
  like `//` or `~/` ([#50], [#52]).
* In `read-file-name`, when a default is provided (for example in the
  `dired-do-rename` command), we actually use it as the initial
  contents of the minibuffer, which allows you to have convenient
  access to the default filename when that default file does not exist
  ([#25]).
* We now bind `minibuffer-completing-file-name` during
  `read-file-name`, in conformance with the standard Emacs interface
  ([#30]).
* A new text property `selectrum-candidate-display-right-margin` is
  added, to display a string at the right margin after a candidate
  ([#44]).
* You can now access standard minibuffer history using `M-p` and `M-n`
  ([#4], [#38]).
* Previously, setting `resize-mini-windows` to nil would cause
  Selectrum to be unable to display any candidates. This has been
  fixed by having Selectrum bind the variable to `grow-only` when
  entering the minibuffer ([#35]).
* Previously, a large value of `selectrum-num-candidates-displayed`
  would produce a confusing result because `max-mini-window-height`
  imposed a lower limit on the height of the minibuffer. Now that
  variable is bound automatically by Selectrum based on the value of
  `selectrum-num-candidates-displayed` ([#22]).
* Multiline candidates are now displayed properly and do not mess up
  scrolling in the candidate list ([#12]).
* When you select the user input area and it doesn't have anything
  typed, we now show an overlay indicating that you are in this state,
  so it is less confusing. The overlay shows what default value will
  be submitted if you press return. See [#55].
* Switching buffers is now less confusing, because we don't modify the
  order of the buffer list at all. Previously the default buffer to
  switch to was moved to the top of the list. Now we leave it where it
  is, and just select it initially. `selectrum-read` grows a new
  argument `:no-move-default-candidate` to support this improvement.
* Previously, `selectrum-read` sometimes modified the list of
  candidates it was given. This has been fixed, and there is a new
  keyword argument `:may-modify-candidates` to re-enable the old
  behavior for cases where it is safe and the performance gains are
  useful. See [#74].

### Bugs fixed
* You can now use the undo system in the minibuffer. Previously,
  trying to do so would break Selectrum ([#31]).
* Passing a list of symbols to `selectrum-completing-read` works now.
* Previously, `selectrum-read-buffer` ignored its PREDICATE argument.
  This has now been fixed ([#32], [#33]).
* Previously, `selectrum-read` would return nil when
  `selectrum-submit-exact-input` was used on an empty input and no
  `:default-candidate` was provided. Now the empty string is returned,
  in accordance with the `completing-read` API ([#34]).
* The keymap used in the minibuffer now inherits from
  `minibuffer-local-map`, so standard minibuffer bindings should still
  work ([raxod502/ctrlf#41]).
* The application of face `selectrum-current-candidate` does not
  trample on the results of `selectrum-highlight-candidates-function`.
  In other words, the matched part of the current candidate is now
  highlighted just like the matched part of the other candidates. See
  ([#21], [#76]).
* Previously, an error was thrown if you used certain non-Selectrum
  minibuffer commands before loading Selectrum. This has been fixed
  ([#28]).
* If `selectrum-num-candidates-displayed` is set to one, the
  highlighting now works correctly. Before, the prompt would get
  highlighted instead of the current candidate. See [#85].
* `selectrum-read-library-name` previously, in certain versions of
  Emacs, showed some entries with `.el` appended. This has now been
  fixed. Also, `TAB` now inserts the current candidate and not the
  whole path to the library, so that the result can be submitted
  directly ([#73]).

[#4]: https://github.com/raxod502/selectrum/issues/4
[#12]: https://github.com/raxod502/selectrum/issues/12
[#16]: https://github.com/raxod502/selectrum/issues/16
[#18]: https://github.com/raxod502/selectrum/issues/18
[#21]: https://github.com/raxod502/selectrum/issues/21
[#22]: https://github.com/raxod502/selectrum/issues/22
[#25]: https://github.com/raxod502/selectrum/pull/25
[#27]: https://github.com/raxod502/selectrum/pull/27
[#28]: https://github.com/raxod502/selectrum/issues/28
[#30]: https://github.com/raxod502/selectrum/issues/30
[#31]: https://github.com/raxod502/selectrum/issues/31
[#32]: https://github.com/raxod502/selectrum/issues/32
[#33]: https://github.com/raxod502/selectrum/pull/33
[#34]: https://github.com/raxod502/selectrum/pull/34
[#35]: https://github.com/raxod502/selectrum/issues/35
[#38]: https://github.com/raxod502/selectrum/pull/38
[#39]: https://github.com/raxod502/selectrum/issues/39
[#42]: https://github.com/raxod502/selectrum/issues/42
[#44]: https://github.com/raxod502/selectrum/pull/44
[#49]: https://github.com/raxod502/selectrum/issues/49
[#50]: https://github.com/raxod502/selectrum/pull/50
[#52]: https://github.com/raxod502/selectrum/issues/52
[#53]: https://github.com/raxod502/selectrum/issues/53
[#54]: https://github.com/raxod502/selectrum/pull/54
[#55]: https://github.com/raxod502/selectrum/issues/55
[#57]: https://github.com/raxod502/selectrum/pull/57
[#62]: https://github.com/raxod502/selectrum/pull/62
[#73]: https://github.com/raxod502/selectrum/pull/73
[#74]: https://github.com/raxod502/selectrum/pull/74
[#76]: https://github.com/raxod502/selectrum/pull/76
[#77]: https://github.com/raxod502/selectrum/pull/77
[#80]: https://github.com/raxod502/selectrum/issues/80
[#85]: https://github.com/raxod502/selectrum/pull/85
[#86]: https://github.com/raxod502/selectrum/pull/86
[#89]: https://github.com/raxod502/selectrum/pull/89
[#96]: https://github.com/raxod502/selectrum/pull/96
[raxod502/ctrlf#41]: https://github.com/raxod502/ctrlf/issues/41

## 1.0 (released 2020-03-23)
### Added
* Package `selectrum`
* Minor mode `selectrum-mode`
* Faces:
    * `selectrum-current-candidate`
    * `selectrum-primary-highlight`
    * `selectrum-secondary-highlight`
* Interface user options:
    * `selectrum-num-candidates-displayed`
    * `selectrum-minibuffer-bindings`
    * `selectrum-count-style`
* Commands bound in minibuffer:
    * `selectrum-previous-candidate`
    * `selectrum-next-candidate`
    * `selectrum-previous-page`
    * `selectrum-next-page`
    * `selectrum-goto-beginning`
    * `selectrum-goto-end`
    * `selectrum-kill-ring-save`
    * `selectrum-select-current-candidate`
    * `selectrum-submit-exact-input`
    * `selectrum-insert-current-candidate`
* Entry points:
    * `selectrum-read`
    * `selectrum-completing-read`
    * `selectrum-read-buffer`
    * `selectrum-read-file-name`
    * `selectrum-read-directory-name`
    * `selectrum-read-library-name`
* Hooks:
    * `selectrum-candidate-selected-hook`
    * `selectrum-candidate-inserted-hook`
* API user options, variables, and functions:
    * `selectrum-refine-candidates-function`
    * `selectrum-default-candidate-refine-function`
    * `selectrum-preprocess-candidates-function`
    * `selectrum-default-candidate-preprocess-function`
    * `selectrum-highlight-candidates-function`
    * `selectrum-default-candidate-highlight-function`
    * `selectrum-should-sort-p`

[keep a changelog]: https://keepachangelog.com/en/1.0.0/
