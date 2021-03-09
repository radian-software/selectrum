#!/bin/sh
if [ "$#" -ne 1 ] || ! [ -r "$1" ]; then
  echo "Usage: $0 <config.el>" >&2
  exit 1
fi
/usr/bin/env \
    emacs -q --no-site-file --no-site-lisp --no-splash -l install.el -l "$1"
