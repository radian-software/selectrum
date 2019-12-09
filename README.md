# Selectrum

*Selectrum is a better solution for incremental narrowing in Emacs,
replacing [Helm](https://github.com/emacs-helm/helm) and
[Ivy](https://github.com/abo-abo/swiper).*

Documentation will come when the basic features have been implemented.
Usage is as follows. First install with
[`straight.el`](https://github.com/raxod502/straight.el):

    (straight-use-package
      '(selectrum :host github :repo "raxod502/selectrum"))

Then enable `selectrum-mode` to enable Selectrum for
`completing-read`. Compatibility with Ivy and Helm is coming.
