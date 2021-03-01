;; -*- lexical-binding: t -*-

;; Enable Selectrum, Prescient and Marginalia
(selectrum-mode)
(selectrum-prescient-mode)
(marginalia-mode)

;; Use completing-read prompter and set binding for Embark context menu
(setq embark-prompter #'embark-completing-read-prompter)
(global-set-key (kbd "M-o") #'embark-act)

;; Keybindings for Consult
(global-set-key (kbd "C-x b") #'consult-buffer)
