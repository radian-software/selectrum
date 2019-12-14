# Selectrum

*Selectrum is a better solution for incremental narrowing in Emacs,
replacing [Helm](https://github.com/emacs-helm/helm) and
[Ivy](https://github.com/abo-abo/swiper).*

Documentation will come when the basic features have been implemented.
Usage is as follows. First install with
[`straight.el`](https://github.com/raxod502/straight.el):

    (straight-use-package
      '(selectrum :host github :repo "raxod502/selectrum"))

Then turn on `selectrum-mode` to enable Selectrum for anything that
uses `completing-read-function`, `read-buffer-function`, or
`read-file-name-function`.

For an optimal experience, integrate with
[`prescient.el`](https://github.com/raxod502/prescient.el). First
install:

    (straight-use-package
      '(selectrum-prescient :host github :repo "raxod502/prescient.el"
                            :files ("selectrum-prescient.el")))

Then turn on `selectrum-prescient-mode` to enable
frequency/recency-based sorting for all Selectrum commands.
