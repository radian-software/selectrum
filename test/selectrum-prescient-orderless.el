;; -*- lexical-binding: t -*-

;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Selectrum, Prescient and Marginalia
(selectrum-mode)
;; Disable filtering as orderless is used for that
(setq selectrum-prescient-enable-filtering nil)
(selectrum-prescient-mode)
(marginalia-mode)

;; Use completing-read prompter and set binding for Embark context menu
(setq embark-prompter #'embark-completing-read-prompter
      prefix-help-command #'embark-prefix-help-command)
(global-set-key (kbd "M-o") #'embark-act)

;; Keybindings for Consult
(global-set-key (kbd "C-x b") #'consult-buffer)
