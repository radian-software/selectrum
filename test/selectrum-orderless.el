;; -*- lexical-binding: t -*-

;; Use a temporary emacs.d directory for testing
(setq user-emacs-directory "/tmp/test-emacs.d/")

;; Setup package archive
(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-refresh-contents)

;; Install packages
(package-install 'selectrum)
(package-install 'orderless)
(package-install 'consult)
(package-install 'marginalia)
(package-install 'embark)
(package-install 'embark-consult)

;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Selectrum and Marginalia
(selectrum-mode)
(marginalia-mode)

;; Use completing-read prompter and set binding for Embark context menu
(setq embark-prompter #'embark-completing-read-prompter)
(global-set-key (kbd "M-o") #'embark-act)

;; Keybindings for Consult
(global-set-key (kbd "C-x b") #'consult-buffer)
