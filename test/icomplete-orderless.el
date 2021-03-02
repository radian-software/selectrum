;; -*- lexical-binding: t -*-

;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Icomplete-vertical and Marginalia
(icomplete-vertical-mode)
(icomplete-mode)
(marginalia-mode)

;; Icomplete-vertical configuration
(setq icomplete-compute-delay 0)
(define-key icomplete-minibuffer-map (kbd "SPC")
  #'self-insert-command)
(define-key icomplete-minibuffer-map (kbd "<down>")
  #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>")
  #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "RET")
  #'minibuffer-force-complete-and-exit)

;; Use completing-read prompter and set binding for Embark context menu
(setq embark-prompter #'embark-completing-read-prompter)
(global-set-key (kbd "M-o") #'embark-act)

;; Keybindings for Consult
(global-set-key (kbd "C-x b") #'consult-buffer)
