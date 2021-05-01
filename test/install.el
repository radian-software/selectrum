;; -*- lexical-binding: t -*-

;; Use a temporary emacs.d directory for testing
(setq user-emacs-directory "/tmp/test-emacs.d/")

;; Setup package archive
(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-refresh-contents)

;; Install packages
(package-install 'consult)
(package-install 'embark)
(package-install 'embark-consult)
(package-install 'icomplete-vertical)
(package-install 'marginalia)
(package-install 'orderless)
(package-install 'selectrum)
(package-install 'selectrum-prescient)
(package-install 'vertico)

(message "Installed packages to /tmp/test-emacs.d/")
